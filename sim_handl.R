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

test_bldg <- get_bldg(run_id = "testing", copies = 2, type = "office")
test_ldc <- test_bldg$make_ldc()

make_run_folder = function(run_id) {
  try_path = paste("outputs\\", run_id, sep = "")
  if (dir.exists(file.path(try_path))) {
    copy_val = 1
    copy_txt = paste("_copy_",copy_val, sep = "")
    new_path = paste(try_path, copy_txt, sep = "")
    new_id = paste(run_id, copy_txt, sep = "")
    while (dir.exists(file.path(new_path))) {
      copy_val = copy_val + 1
      copy_txt = paste("_copy_",copy_val, sep = "")
      new_path = paste(try_path, copy_txt, sep = "")
      new_id = paste(run_id, copy_txt, sep = "")
    }
  }
  else {
    new_path = try_path
    new_id = run_id
  }
  dir.create(file.path(new_path))
  return(new_id)
}
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
    get_bldg(run_id = "check_ts", type = "office")$get_metadata()$time_int,
    get_pv(run_id = "check_ts", type = "office")$get_metadata()$time_int,
    get_grid(run_id = "check_ts", terr = "nyiso")$get_metadata()$time_int)
  
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
batt_sizer <- function(run_id, bldg_ts = NULL, pv_ts = NULL,
                                dmd_frac = NULL, batt_type = NULL) {
  
  interval = as.numeric(check_ts_intervals(run_id = "ts_check")[["time_int"]])
  
  log_path = paste(
                  "outputs\\", run_id, "\\batt_sizer_", batt_type,
                  "_", dmd_frac, "_", run_id, "_",
                  strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                  ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sizer")

  max_step <- bldg_ts[which(bldg_ts$kw == max(bldg_ts$kw)),]
  flog.info(paste("Max kW happens at", max_step$date_time), name = "sizer")

  size_ts <- filter(bldg_ts,
                    as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  pv_ts <- filter(pv_ts,
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
  
  out_vec <- list("batt_kwh" = test_capacity, "unmet_kWh" = unmet_kwh)
  return(out_vec)
}
# batt_sizer(run_id = "testing", bldg_ts = test_bldg$get_base_ts(),
#                                 pv_ts = test_pv$get_base_ts(),
#                                 dmd_frac = 0.3, batt_type = "li_ion")

sim_sizer <- function(run_id, bldg = NULL, batt_type = NULL, dispatch = NULL) {
  
  run_id = make_run_folder(run_id)
  log_path = paste("outputs\\", run_id,"\\sim_", bldg$get_metadata()[["bldg"]], "_",
                   batt_type, "_", 
                   # rand_factors[["bldg"]], "_",rand_factor[["pv"]], "_", rand_factors[["grid"]], "_",
                   strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                   ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sim_1yr")
  
  sim_output = data.frame("run_id" = numeric(), bldg = character(),
                          pv_kw = numeric(), dmd = numeric(),
                          ts_num = numeric(),
                          batt_type = character(), batt_kwh = numeric())
  
  # figure out what demand_frac range to use
  dmd_fracs = seq(0.2,0.4,0.1)
  pv = get_pv(run_id, copies = bldg$get_ts_count() - 1,
                      type = bldg$get_metadata()[["bldg"]])
  
  if (bldg$get_ts_count() > 1) {
    for (i in 1:length(dmd_fracs)) {
      cl <- makeCluster(3)
      registerDoSNOW(cl)
      
      test_dmd = dmd_fracs[i]
      funs_to_pass = c("batt_sizer", "check_ts_intervals")
      pkgs_to_pass = c("dplyr", "futile.logger")
      
      dmd_df = foreach(j = 1:(bldg$get_ts_count()), .combine = "rbind",
                       .export = funs_to_pass, .packages = pkgs_to_pass) %dopar% {
                  if(!exists("batt_bank", mode = "function")) source("battery_bank.R")
                  if(!exists("disp_curv", mode = "function")) source("dispatch_curve.R")
                  if(!exists("bldg_load", mode = "function")) source("bldg_load.R")
                  if(!exists("grid_load", mode = "function")) source("grid_load.R")
                  if(!exists("pv_load", mode = "function")) source("pv_load.R")
                  if(!exists("sys_ctrl.R", mode = "function")) source("sys_ctrl.R")       
                         
                  bldg_ts = bldg$get_ts_df()[[j]]
                  pv_ts = pv$get_ts_df()[[j]]
                  batt_kwh = batt_sizer(run_id = run_id, bldg_ts = bldg_ts,
                                                pv_ts = pv_ts,
                                                dmd_frac = test_dmd,
                                                batt_type = batt_type)$batt_kwh
                  
                  sim_1 = list("run_id" = run_id, bldg = bldg$get_metadata()[["bldg"]],
                                                   pv_kw = pv$get_metadata()[["kw"]],
                                                   dmd_frac = test_dmd,
                                                   ts_num = j,
                                                   batt_type = batt_type,
                                                   batt_kwh = batt_kwh)
      }
      stopCluster(cl)
      
      sim_output = rbind(sim_output, dmd_df)
    }
  }
  return(sim_output)
}
sim_sizer("testing", bldg = test_bldg, batt_type = "li_ion")

