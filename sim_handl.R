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
                            batt = get_test_batt(0.083),
                            bldg = get_test_bldg()$get_base_ts(),
                            dispatch = get_test_disp(),
                            grid = get_test_grid()$get_base_ts(),
                            pv = get_test_pv()$get_base_ts()
)
ctrlr_test$get_batt()$get_metadata()
ctrlr_test$traverse_ts()
