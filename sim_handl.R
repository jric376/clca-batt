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

test_bldg <- get_bldg()

# 
# 
# check_ts_intervals = function() {
#   intervals = c(
#     private$bldg$get_metadata()$time_int,
#     private$grid$get_metadata()$time_int,
#     private$pv$get_metadata()$time_int)
#   
#   if (length(unique(intervals)) > 1) {
#     flog.error(paste("Time-series have different freqs",
#                      intervals))
#     stop("Time-series have different freqs")
#   }
#   
#   interval = list(
#     "time_int" = as.difftime(intervals[1], units = "hours")
#   )
#   private$metadata = append(private$metadata, interval)
# }

batt_sizer <- function(bldg_ts = NULL, dmd_frac = NULL, batt_type = NULL,
                       interval = 1/12) {

  log.path = paste(
                  "outputs\\batt_sizer_", batt_type,
                  "_", dmd_frac, "_",
                  # "_", meta[["run_id"]], "_",
                  # meta[["ctrl_id"]], "_",
                  strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                  ".log", sep = ""
  )
  flog.appender(appender.file(log.path), name = "sizer")

  max_step <- bldg_ts[which(bldg_ts$kw == max(bldg_ts$kw)),]
  flog.info(paste("Max kW happens at", max_step$date_time), name = "sizer")

  size_ts <- filter(bldg_ts, as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  # hist(size_ts$kw)
  pv_ts <- filter(get_pv()$get_base_ts(), as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  grid_ts <- filter(get_grid()$get_base_ts(), as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)

  unmet_kwh <- sum(bldg_ts$kwh)
  unmet_thresh <- 0.0001*sum(bldg_ts$kwh)       # max unmet_kwh to trigger adequate size
  targ_kw <- max(max_step$kw)*(1 - dmd_frac)    # fraction of peak demand to be shaved
  test_capacity <- 1                            # intentionally low initial batt kwh size
  incr <- 0.05

  while ((unmet_kwh > unmet_thresh) & (incr > 0)) {
    test_capacity <- test_capacity + incr*max(bldg_ts$kwh)
    batt_meta <- list(
                      "name" = "Boris the Battery",
                      "run_id" = "RUNID",
                      "ctrl_id" = "CTRLID",
                      "run_timestr" = "RUNTIMESTR",
                      "time_int" = interval
                      )
    temp_batt <- batt_bank$new(
                              meta = batt_meta,
                              type = batt_type,
                              nameplt = test_capacity
                              )
    ctrlr_meta <- list(
                        "name" = "Sam the System_Controller",
                        "run_id" = "RUNID",
                        "ctrl_id" = "CTRLID",
                        "run_timestr" = "RUNTIMESTR",
                        "time_int" = interval
                      )
    temp_ctrlr <- sys_ctrlr$new(
                                meta = ctrlr_meta,
                                dmd_targ = targ_kw,
                                batt = temp_batt,
                                bldg_ts = size_ts,
                                dispatch = get_disp(),
                                grid_ts = grid_ts,
                                pv_ts = pv_ts
                                )
    temp_ctrlr$traverse_ts()
    unmet_kwh <- sum(temp_ctrlr$get_sim_df()$unmet_kw)*interval
    flog.info(paste("Unmet kWh is at -", round(unmet_kwh, 2), "kWh"),
              name = "sizer")
    
    if ((unmet_kwh < unmet_thresh) & (incr > 0.0005)) {
      unmet_kwh <- unmet_thresh + 1
      test_capacity <- test_capacity - incr*max(bldg_ts$kwh)
      incr <- incr*0.5
    }
    flog.info(paste("-- Threshold is", round(unmet_thresh, 2), "kWh",
                    "-- Demand target is", round(targ_kw, 2), "kW",
                    "-- Increment is", incr,
                    "-- Test Capacity is", round(test_capacity, 2), "kwh"),
              name = "sizer")
  }
  flog.info(paste("Final capacity is", test_capacity, "kwh"),
            name = "sizer")
  
  out_vec <- list("sized_cap" = test_capacity, "unmet_kWh" = unmet_kwh)
  return(out_vec)
}
batt_sizer(test_bldg$get_base_ts(), 0.3, "li_ion")
batt_sizer(test_bldg$get_base_ts(), 0.3, "pb_a")
batt_sizer(test_bldg$get_base_ts(), 0.5, "li_ion")
batt_sizer(test_bldg$get_base_ts(), 0.5, "pb_a")

# time_int will be specified by check_ts_intervals output
# and HAS TO BE INCLUDED IN ALL BATTERY METADATA!!!
# WHETHER THAT HAPPENS HERE OR IN sys_ctrl.R
ctrlr_test <- sys_ctrlr$new(
                            meta = list(
                              "name" = "Sam the System_Controller",
                              "run_id" = "RUNID",
                              "ctrl_id" = "CTRLID",
                              "run_timestr" = "RUNTIMESTR",
                              "time_int" = 0.083
                            ),
                            dmd_targ = 2,
                            batt = get_batt(chem = "li_ion", kwh = 100),
                            bldg_ts = get_bldg()$get_base_ts(),
                            dispatch = get_disp(),
                            grid_ts = get_grid()$get_base_ts(),
                            pv_ts = get_pv()$get_base_ts()
)
ctrlr_test$traverse_ts()
