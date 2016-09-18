# System Controller Object

# This object takes in the following values
# grid load (MW), bldg load (kW), PV load (kW)

# for a single time-step. It also takes in
# dispatch curve and battery bank objects

# Based on the values and states of these objects,
# it updates the objects and outputs
# an array of values decribing the next timestep

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

sys_ctrlr <- R6Class("System Controller",
                     public = list(
                       initialize = function(
                                             meta = NULL, dmd_targ = NULL,
                                             batt = NULL, bldg = NULL,
                                             dispatch = NULL, grid = NULL,
                                             pv = NULL
                                             ) {
                         private$dmd_targ = dmd_targ
                         private$batt = batt
                         private$bldg = bldg
                         private$dispatch = dispatch
                         private$grid = grid
                         private$pv = pv
                         # private$time_int = time_int
                         #also need to set battery time_int
                         self$add_metadata(meta)
                         
                         log_path = paste(
                                          "outputs/", meta[["name"]], "_",
                                          meta[["run_id"]], "_", meta[["ctrl_id"]], "_", 
                                          strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                                          ".log", sep = ""
                                          )
                         flog.appender(appender.file(log_path))
                         flog.error("Time interval variable not initialized in any controller yet.")
                       },
                       
                       add_metadata = function(metadata) {
                         if (length(metadata) < 1) {
                           flog.error(
                                      paste("Empty metadata in controller with dmd_targ ",
                                            private$dmd_targ, " with batt ", private$batt$chem
                                            )
                                      )
                         }
                         else {
                           for (datum_name in names(metadata)) {
                             private$metadata[[datum_name]] <- metadata[[datum_name]]
                           }
                         }
                       },
                       
                       check_ts_intervals = function() {
                         intervals = c(
                                        private$bldg$get_metadata()$time_int,
                                        private$grid$get_metadata()$time_int,
                                        private$pv$get_metadata()$time_int)
                         
                         if (length(unique(intervals)) > 1) {
                           flog.error(paste("Time-series have different freqs",
                                            intervals))
                           stop("Time-series have different freqs")
                         }
                       },
                       
                       draw_batt = function(kwh_val) {
                         private$batt$draw <- kwh_val
                         return(private$batt$get_state()$del_kwh)
                       },
                       
                       operate = function(bldg_kw = NULL, pv_kw = NULL) {
                         # this is the main function for deciding how and
                         # from where the system will draw power to meet
                         # the bldg demand and shave peak demand
                         
                         grid_kw = NA
                         batt_kw = NA
                         curtail_kw = NA
                         unmet_kw = NA
                         
                         if (bldg_kw > private$dmd_targ) {
                           if (pv_kw > bldg_kw) {
                             grid_kw = 0
                             batt_kw = self$draw_batt(pv_kw - bldg_kw)
                             curtail_kw = pv_kw - bldg_kw - batt_kw
                             unmet_kw = 0
                           }
                           else if (pv_kw > 0) {
                             grid_kw = bldg_kw - pv_kw
                             batt_kw = self$draw_batt(private$dmd_targ - grid_kw)
                             if ((grid_kw + batt_kw) > private$dmd_targ) {
                               if (batt_kw > 0) flog.warn(
                                                          paste(">Targ, Some solar",
                                                                private$metadata[["run_id"]],
                                                                private$metadata[["ctrl_id"]],
                                                                private$batt$get_state()
                                 )
                               )
                               unmet_kw = grid_kw + batt_kw - private$dmd_targ
                             }
                             else unmet_kw = 0
                             curtail_kw = 0
                           }
                           else {
                             grid_kw = private$dmd_targ
                             batt_kw = self$draw_batt(private$dmd_targ - bldg_kw)
                             if ((bldg_kw + batt_kw) > private$dmd_targ) {
                               if (batt_kw > 0) flog.warn(
                                                          paste(">Targ, No solar",
                                                                private$metadata[["run_id"]],
                                                                private$metadata[["ctrl_id"]],
                                                                private$batt$get_state()
                                                                )
                                                          )
                               unmet_kw = bldg_kw + batt_kw - private$dmd_targ
                             }
                             else unmet_kw = 0
                             curtail_kw = 0
                           }
                         }
                         else {
                           if (pv_kw > 0) {
                             batt_kw = self$draw_batt(pv_kw)
                             if (batt_kw < pv_kw) {
                               grid_kw = bldg_kw - pv_kw - batt_kw
                               curtail_kw = 0
                               if (grid_kw < 0) {
                                 curtail_kw = abs(grid_kw)
                                 grid_kw = 0
                               }
                             }
                             else {
                               grid_kw = bldg_kw
                               curtail_kw = 0
                             }
                           }
                           else {
                             grid_kw = bldg_kw
                             batt_kw = 0
                             curtail_kw = 0
                           }
                           unmet_kw = 0
                         }
                         
                         if (curtail_kw > 0) {
                           pv_kw = pv_kw - curtail_kw
                           if (pv_kw < 0) pv_kw = 0
                         }
                         
                         next_state = list(
                           "bldg_kw" = bldg_kw, "grid_kw" = grid_kw,
                           "pv_kw" = pv_kw, "batt_kw" = batt_kw, 
                           "unmet_kw" = unmet_kw, "curtail_kw" = curtail_kw
                         )
                         
                         return(next_state)
                       },

                       traverse_ts = function() {
                         # self$check_ts_intervals()
                         
                         bldg_kwh = private$bldg$get_base_ts()$kwh[1:9]
                         pv_kwh = c(8,1,1,1,1,1,1,1,1)
                         sim_df <- do.call(rbind, lapply(1:length(bldg_kwh), function(i) {
                           self$operate(bldg_kwh[i], pv_kwh[i]) 
                         }))
                         
                         return(sim_df)
                         
                         # sim_df <- do.call(rbind, lapply(1:nrow(private$bldg_ts), function(i) {
                         #  self$operate(private$bldg_ts, private$pv_ts)
                         # }
                         # ))

                       #   write.csv(sim_df, paste("outputs\\df\\test_",
                       # format(as.POSIXlt(Sys.time()), "%m%d_%H%M%S"),
                       # ".csv", sep = ""))
                       #   private$sim_df = sim_df
                       },
                       
                       get_targ = function() {
                         return(private$dmd_targ)
                       },
                       
                       get_batt = function() {
                         return(private$batt)
                       },
                       
                       get_bldg = function() {
                         return(private$bldg)
                       },
                       
                       get_grid = function() {
                         return(private$grid)
                       },
                       
                       get_pv = function() {
                         return(private$pv)
                       },
                       
                       get_bldg_ts = function() {
                         return(private$bldg$get_base_ts())
                       },
                       
                       get_dispatch = function() {
                         return(private$dispatch)
                       },
                       
                       get_grid_ts = function() {
                         return(private$grid$get_base_ts())
                       },
                       
                       get_pv_ts = function() {
                         return(private$pv$get_base_ts())
                       },
                       
                       get_metadata = function() {
                         return(private$metadata)
                       }
                     ),
                     private = list(
                       dmd_targ = NULL,
                       batt = NULL,
                       bldg = NULL,
                       dispatch = NULL,
                       grid = NULL,
                       pv = NULL,
                       metadata = NULL,
                       time_int = NULL,
                       sim_df = NULL
                     ))

ctrlr_test <- sys_ctrlr$new(
                            meta = list(
                              "name" = "Sam the System_Controller",
                              "run_id" = "RUNID",
                              "ctrl_id" = "CTRLID",
                              "run_timestr" = "RUNTIMESTR"
                            ),
                            dmd_targ = 3,
                            batt = get_test_batt(),
                            bldg = get_test_bldg(),
                            dispatch = get_test_disp(),
                            grid = get_test_grid(),
                            pv = get_test_pv()
                            )
ctrlr_test$traverse_ts()

targ_all_pv <- function (ctrlr = NULL) {
  ctrlr$draw_batt(-1)
  return(ctrlr$operate(bldg_kw = 11, pv_kw = 13))
}
targ_some_pv_disch <- function (ctrlr = NULL) {
  ctrlr$draw_batt(-7)
  return(ctrlr$operate(bldg_kw = 15, pv_kw = 1))
}
targ_some_pv_ch <- function (ctrlr = NULL) {
  ctrlr$draw_batt(-3)
  return(ctrlr$operate(bldg_kw = 12, pv_kw = 7))
}
targ_no_pv <- function (ctrlr = NULL) {
  ctrlr$draw_batt(-7)
  return(ctrlr$operate(bldg_kw = 11, pv_kw = 0))
}
notarg_pv_grid <- function(ctrlr = NULL) {
  ctrlr$draw_batt(-0.5)
  return(ctrlr$operate(bldg_kw = 8, pv_kw = 5))
}
notarg_curtail <- function(ctrlr = NULL) {
  ctrlr$draw_batt(-0.5)
  return(ctrlr$operate(bldg_kw = 1, pv_kw = 15))
}
notarg <- function(ctrlr = NULL) {
  return(ctrlr$operate(bldg_kw = 8, pv_kw = 0))
}
