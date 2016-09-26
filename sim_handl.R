# Simulation Handler - GHG Changes from Demand Response w PV + Battery Storage

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("R6")
if(!exists("batt_bank", mode = "function")) source("battery_bank.R")
if(!exists("disp_curv", mode = "function")) source("dispatch_curve.R")
if(!exists("bldg_load", mode = "function")) source("bldg_load.R")
if(!exists("grid_load", mode = "function")) source("grid_load.R")
if(!exists("pv_load", mode = "function")) source("pv_load.R")
if(!exists("sys_ctrl.R", mode = "function")) source("sys_ctrl.R")

test_bldg <- get_bldg(run_id = "testing")
test_ldc <- test_bldg$make_ldc()

check_ts_intervals = function(run_id = NULL) {
  
  if (is.null(run_id)) {
    flog.error("No run_id in check_ts_intervals")
  }
    
  log_path = paste(
                  "outputs\\", run_id, "\\ts_check_",
                  strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                  ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "ts_check")
  
  intervals = c(
    get_bldg()$get_metadata()$time_int,
    get_grid()$get_metadata()$time_int,
    get_pv()$get_metadata()$time_int)
  
  if (length(unique(intervals)) > 1) {
    flog.error(paste("Time-series have different freqs",
                     intervals),
               name = "ts_check")
    stop("Time-series have different freqs")
  }
  
  interval = list(
    "time_int" = as.difftime(intervals[1], units = "hours")
  )
  
  return(interval)
}
batt_sizer <- function(run_id = NULL, bldg_ts = NULL, dmd_frac = NULL, batt_type = NULL) {
  
  if (is.null(run_id)) {
    flog.appender(appender.file("outputs\\NO_ID_batt_sizer.log"),
                  name = "no_run_id")
    flog.warn(paste("No explicit run_id in batt_sizer. \n",
                    "Hopefully this is just to check_ts_intervals. \n",
                    "run_id is temporarily 'ts_check'"),
              name = "no_run_id")
    run_id = "ts_check"
  }
  interval = as.numeric(check_ts_intervals(run_id = "ts_check")[["time_int"]])
  
  try_path = paste("outputs\\", run_id, sep = "")
  if (dir.exists(file.path(try_path))) {
    copy_val = 1
    copy_txt = paste("_copy_",copy_val, sep = "")
    new_path = paste(try_path, copy_txt, sep = "")
    new_id = paste(run_id, copy_txt)
    while (dir.exists(file.path(new_path))) {
      copy_val = copy_val + 1
      copy_txt = paste("_copy_",copy_val, sep = "")
      new_path = paste(try_path, copy_txt, sep = "")
      new_id = paste(run_id, copy_txt)
    }
  }
  else {
    new_path = try_path
  }
  dir.create(file.path(new_path))
  log_path = paste(
                  new_path, "\\batt_sizer_", batt_type,
                  "_", dmd_frac, "_", run_id, "_",
                  strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                  ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sizer")

  max_step <- bldg_ts[which(bldg_ts$kw == max(bldg_ts$kw)),]
  flog.info(paste("Max kW happens at", max_step$date_time), name = "sizer")

  size_ts <- filter(bldg_ts,
                    as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  pv_ts <- filter(get_pv()$get_base_ts(),
                  as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  grid_ts <- filter(get_grid()$get_base_ts(),
                    as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)

  unmet_kwh <- sum(bldg_ts$kwh)
  unmet_thresh <- 0.0001*sum(bldg_ts$kwh)       # max unmet_kwh to trigger adequate size
  targ_kw <- max(max_step$kw)*(1 - dmd_frac)    # fraction of peak demand to be shaved
  test_capacity <- 0.5                            # intentionally low initial batt kwh size
  incr <- 0.05

  while ((unmet_kwh > unmet_thresh) & (incr > 0)) {
    
    # SET RUN and CTRL IDs HERE
    
    test_capacity <- test_capacity + incr*max(bldg_ts$kwh)
    batt_meta <- list(
                      "name" = "Boris the Battery",
                      "run_id" = run_id,
                      "ctrl_id" = "CTRLID",
                      "time_int" = interval
                      )
    temp_batt <- batt_bank$new(
                              meta = batt_meta,
                              type = batt_type,
                              nameplt = test_capacity
                              )
    ctrlr_meta <- list(
                        "name" = "Sam the System_Controller",
                        "run_id" = run_id,
                        "ctrl_id" = "CTRLID",
                        "time_int" = interval
                      )
    temp_ctrlr <- sys_ctrlr$new(
                                meta = ctrlr_meta,
                                dmd_targ = targ_kw,
                                batt = temp_batt,
                                bldg_ts = size_ts,
                                grid_ts = grid_ts,
                                pv_ts = pv_ts
                                )
    temp_ctrlr$traverse_ts(save_df = FALSE)
    unmet_kwh <- sum(temp_ctrlr$get_sim_df()$unmet_kw)*interval
    flog.info(paste("Unmet kWh is at -", round(unmet_kwh, 2), "kWh"),
              name = "sizer")
    
    if ((unmet_kwh < unmet_thresh) & (incr > 0.0005)) {
      unmet_kwh <- unmet_thresh + 1
      test_capacity <- test_capacity - incr*max(bldg_ts$kwh)
      incr <- incr*0.5
    }
    incr_sci <- format(incr, digits = 2, scientific = T)
    log_state = paste("Unmet kWh is at -", round(unmet_kwh, 2), "kWh",
                      "- Threshold is", round(unmet_thresh, 2), "kWh",
                      "- Dmnd targ is", round(targ_kw, 2), "kW",
                      "- Incrmt is", incr_sci,
                      "- Test Cap. is", round(test_capacity, 2), "kwh")
    flog.info(log_state, name = "sizer")
    noquote(print(log_state))
  }
  flog.info(paste("Final capacity is", test_capacity, "kwh"),
            name = "sizer")
  
  out_vec <- list("bank_kwh" = test_capacity, "unmet_kWh" = unmet_kwh)
  return(out_vec)
}
# batt_sizer(run_id = "test_folder", bldg_ts = test_bldg$get_base_ts(), dmd_frac = 0.8, batt_type = "li_ion")

sim_1yr <- function(run_id, bldg = NULL, batt_type = NULL, rand_factors = NULL, dispatch = NULL) {
  
  # figure out what demand_frac range to use
  # size for each demand_frac in this range
  # foreach ts in bldg$get_ts_df()
  # traverse ts
  
}


