# System Controller Object

# This object takes in the following time-series
# grid load (MW), bldg load (kW), PV load (kW)

# It also takes in a batt bank object and a
# demand response target

# Based on how the bldg load compares to the target
# and the state of the battery, it updates outputs
# an array of values decribing the next timestep

# Emissions changes are calculated after traversing
# the bldg + pv ts, using the grid ts and
# dispatch curves

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library(dplyr)
library(futile.logger)
library(R6)

sys_ctrlr <- R6Class("System Controller",
                     public = list(
                       initialize = function(
                                             meta = NULL, dmd_targ = NULL,
                                             batt = NULL, bldg_ts = NULL,
                                             dispatch = NULL, grid_ts = NULL,
                                             pv_ts = NULL
                                             ) {
                         
                         log_path = paste(
                           "outputs\\", meta[["run_id"]], "\\",
                           meta[["name"]], "_", meta[["ctrl_id"]], "_", 
                           strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                           ".log", sep = ""
                         )
                         flog.appender(appender.file(log_path), name = "ctrlr")
                         flog.threshold(ERROR, name = "ctrlr")
                         
                         private$dmd_targ = dmd_targ
                         private$batt = batt
                         private$bldg_ts = bldg_ts
                         private$dispatch = dispatch
                         private$grid_ts = grid_ts
                         private$pv_ts = pv_ts
                         self$add_metadata(meta)
                       },
                       
                       add_metadata = function(metadata) {
                         if (length(metadata) < 1) {
                           flog.error(
                                      paste("Empty metadata in controller with dmd_targ ",
                                            private$dmd_targ, " with batt ", private$batt$chem
                                            ),
                                      name = "ctrlr"
                                      )
                         }
                         else {
                           if (is.null(metadata[["time_int"]])) {
                             flog.error("Time interval variable not initialized in any controller yet.",
                                        name = "ctrlr")
                           }
                           else{
                               for (datum_name in names(metadata)) {
                                 private$metadata[[datum_name]] <- metadata[[datum_name]]
                               }
                           }
                         }
                       },
                       
                       draw_batt = function(kw_val) {
                         private$batt$draw <- kw_val
                         del_kwh <- private$batt$get_state()$del_kwh
                         del_kw <- del_kwh / as.numeric(private$metadata[["time_int"]])
                         return(del_kw)
                       },
                       
                       operate = function(timestep = NULL, bldg_kw = NULL, pv_kw = NULL) {
                         # this is the main function for deciding how and
                         # from where the system will draw power to meet
                         # the bldg demand and shave peak demand
                         
                         if (is.null(timestep)) {
                           stop("No timestep for batt to operate on")
                         }
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
                                                               ),
                                                          name = "ctrlr"
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
                                                                ),
                                                          name = "ctrlr"
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
                             
                             # THIS IS WHERE BATT WOULD CHARGE FROM GRID
                             # GIVEN TEMPORAL AND OTHER CONSTRAINTS
                             # i.e. using timestep and maybe SoC
                             
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
                           "date_time" = timestep,
                           "bldg_kw" = bldg_kw, "grid_kw" = grid_kw,
                           "pv_kw" = pv_kw, "batt_kw" = batt_kw, 
                           "unmet_kw" = unmet_kw, "curtail_kw" = curtail_kw
                         )
                         next_state <- append(next_state, private$batt$get_state())
                         
                         return(next_state)
                       },

                       traverse_ts = function(save_df) {# Boolean controlling whether
                                                        # each simulation is saved as csv
                         
                         timesteps = private$bldg_ts$date_time
                         bldg_kw = private$bldg_ts$kw
                         pv_kw = private$pv_ts$kw
                         
                         sim_df <- bind_rows(lapply(1:length(bldg_kw), function(i) {
                           self$operate(timesteps[i], bldg_kw[i], pv_kw[i])
                         }))
                         
                         # emissions and cost columns could be added to sim_df here
                         
                         private$sim_df = sim_df
                         
                         if (save_df) {
                           sim_path <- list(paste("outputs\\", private$metadata[["run_id"]],
                                             sep = ""),
                                            "\\df")
                           for (i in 1:length(sim_path)) {
                             curr_path = paste(sim_path[1:i], collapse ="")
                             if(!dir.exists(file.path(curr_path))) {
                               dir.create(file.path(curr_path))
                             }
                           }
                           nameplt_abrv <- paste(round(private$batt$nameplate, 2)*1000, "W", sep = "")
                           targ_abrv <- 10 - round(private$dmd_targ/max(bldg_kw), 1)*10
                           write.csv(sim_df, paste(curr_path, "\\", private$metadata[["ctrl_id"]],
                                                   "_", private$batt$chem, nameplt_abrv, "_", targ_abrv,
                                                   ".csv", sep = ""))
                         }
                       },
                       
                       get_targ = function() {
                         return(private$dmd_targ)
                       },
                       
                       get_batt = function() {
                         return(private$batt)
                       },
                       
                       get_bldg_ts = function() {
                         return(private$bldg_ts)
                       },
                       
                       get_dispatch = function() {
                         return(private$dispatch)
                       },
                       
                       get_grid_ts = function() {
                         return(private$grid_ts)
                       },
                       
                       get_pv_ts = function() {
                         return(private$pv_ts)
                       },
                       
                       get_metadata = function() {
                         return(private$metadata)
                       },
                       
                       get_sim_df = function() {
                         return(private$sim_df)
                       }
                     ),
                     private = list(
                       dmd_targ = NULL,
                       batt = NULL,
                       bldg_ts = NULL,
                       dispatch = NULL,
                       grid_ts = NULL,
                       pv_ts = NULL,
                       metadata = NULL,
                       sim_df = NULL
                     )
)

# targ_all_pv <- function (ctrlr = NULL) {
#   ctrlr$draw_batt(-1)
#   return(ctrlr$operate(bldg_kw = 11, pv_kw = 13))
# }
# targ_some_pv_disch <- function (ctrlr = NULL) {
#   ctrlr$draw_batt(-7)
#   return(ctrlr$operate(bldg_kw = 15, pv_kw = 1))
# }
# targ_some_pv_ch <- function (ctrlr = NULL) {
#   ctrlr$draw_batt(-3)
#   return(ctrlr$operate(bldg_kw = 12, pv_kw = 7))
# }
# targ_no_pv <- function (ctrlr = NULL) {
#   ctrlr$draw_batt(-7)
#   return(ctrlr$operate(bldg_kw = 11, pv_kw = 0))
# }
# notarg_pv_grid <- function(ctrlr = NULL) {
#   ctrlr$draw_batt(-0.5)
#   return(ctrlr$operate(bldg_kw = 8, pv_kw = 5))
# }
# notarg_curtail <- function(ctrlr = NULL) {
#   ctrlr$draw_batt(-0.5)
#   return(ctrlr$operate(bldg_kw = 1, pv_kw = 15))
# }
# notarg <- function(ctrlr = NULL) {
#   return(ctrlr$operate(bldg_kw = 8, pv_kw = 0))
# }
