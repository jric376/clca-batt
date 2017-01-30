# System Controller Object

# This object takes in the following time-series
# grid load (MW), bldg load (kW), PV load (kW)

# It also takes in a batt bank object and a
# demand response target (dmd_targ)

# Right now, this ctrlr only handles peak shaving.
# Based on whether the bldg load is > or < dmd_targ
# the presence of PV generation, and the state of
# the battery, it updates outputs and builds up a
# dataframe of 1-yr worth of simulation.

# Emissions changes are calculated after traversing
# the bldg + pv ts, using the grid ts and
# dispatch curves

library(dplyr)
library(futile.logger)
library(R6)
if(!exists("bill_calc", mode = "function")) source("costs_calc.R")
if(!exists("disp_curv", mode = "function")) source("dispatch_curve.R")

sys_ctrlr <- R6Class("System Controller",
                     public = list(
                       # The dmd_targ sets the threshold for peak shaving
                       # as the ctrlr traverses 1yr in 5min timesteps
                       # in the bldg, grid, and pv time-series.
                       # The battery and dispatch curve objects
                       # are called upon at each timestep.
                       
                       initialize = function(
                                             meta = NULL, dmd_targ = NULL,
                                             batt = NULL, bldg_ts = NULL,
                                             disp = NULL, grid_ts = NULL,
                                             pv_ts = NULL
                                             ) {
                         
                         log_path = paste(
                           "outputs/", meta[["run_id"]], "/",
                           meta[["ctrl_id"]],
                           ".log", sep = ""
                         )
                         flog.appender(appender.file(log_path), name = "ctrlr")
                         flog.threshold(ERROR, name = "ctrlr")
                         
                         private$dmd_targ = dmd_targ
                         private$batt = batt
                         private$bldg_ts = bldg_ts
                         private$disp = disp
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
                         # (dis)charges the battery with a specified
                         # amount of energy, given a time interval and
                         # power draw
                         
                         private$batt$draw <- kw_val
                         del_kwh <- private$batt$get_state()$del_kwh
                         del_kw <- del_kwh / as.numeric(private$metadata[["time_int"]])
                         return(del_kw)
                       },
                       
                       operate = function(timestep = NULL, bldg_kw = NULL,
                                          pv_kw = NULL, iso_mw = NULL) {
                         # main function for deciding how and
                         # from where the system will direct power to meet
                         # the bldg demand and shave peak demand
                         
                         if (is.null(timestep)) {
                           stop("No timestep for batt to operate on")
                         }
                         grid_kw = NA
                         batt_kw = NA
                         curtail_kw = NA
                         unmet_kw = NA
                         
                         if (bldg_kw > private$dmd_targ) {
                           # this chunk is triggered
                           # when peak shaving is REQUIRED
                           
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
                           # this chunk executes if
                           # peak shaving is NOT NEEDED
                           
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
                             
                             # here, timestep is a chr w format "%Y-%m-%d %H:%M:%S"
                             hr <- as.numeric(strftime(timestep, format = "%H"))
                             if ((hr >= 1 | hr <= 5) & private$batt$soc <= 0.5) {
                                 max_draw = (private$dmd_targ - bldg_kw)*0.5
                                 batt_kw = self$draw_batt(max_draw)
                                 grid_kw = bldg_kw + max_draw
                             }
                             else {
                                   grid_kw = bldg_kw
                                   batt_kw = 0
                             }
                             curtail_kw = 0
                           }
                           unmet_kw = 0
                         }
                         
                         if (curtail_kw > 0) {
                           pv_kw = pv_kw - curtail_kw
                           if (pv_kw < 0) pv_kw = 0
                         }
                         
                         bill_costs <- get_bill_cost(private$metadata[["bldg_nm"]],
                                           timestep,
                                           as.numeric(private$metadata[["time_int"]]),
                                           bldg_kw, grid_kw)
                         
                         if(private$metadata[["ctrl_id"]] == "batt_sizer") {
                           # sizing a battery requires no emissions accounting
                           
                           bldg_plc2erta <- 0
                           grid_plc2erta <- 0
                         }
                         else{
                           # extra 0.001 factor is needed to convert results
                           # of get_emish call from kg CO2eq per MWh to per kWh
                           # so that multiplying by bldg_kw results in 
                           # kg CO2eq as the only remaining unit
                           
                           bldg_plc2erta <- (bldg_kw*0.001)*as.numeric(private$metadata[["time_int"]])* 
                                                private$disp$get_emish(iso_mw)
                           grid_plc2erta <- (grid_kw*0.001)*as.numeric(private$metadata[["time_int"]])* 
                                                private$disp$get_emish(iso_mw)
                           private$disp$stochastize_costs()
                         }
                           
                         next_state = list(
                           "date_time" = strftime(timestep, format = "%Y-%m-%d %H:%M"),
                           "iso_mw" = iso_mw,
                           "bldg_kw" = bldg_kw, "grid_kw" = grid_kw,
                           "pv_kw" = pv_kw, "batt_kw" = batt_kw, 
                           "unmet_kw" = unmet_kw, "curtail_kw" = curtail_kw,
                           "bldg_plc2erta" = bldg_plc2erta, "grid_plc2erta" = grid_plc2erta
                         )
                         next_state <- append(next_state, private$batt$get_state())
                         next_state <- append(next_state, bill_costs)
                         
                         return(next_state)
                       },

                       traverse_ts = function(n = "full", log = FALSE, save_df) {
                         
                         
                         # takes booleans controlling whether
                         # traversing gets logged to console
                         # and whether results are saved as csv
                         
                         timesteps = private$bldg_ts$date_time
                         bldg_kw = private$bldg_ts$kw
                         pv_kw = private$pv_ts$kw
                         
                         if (is.null(private$grid_ts)) {
                           # if no emissions accounting is needed
                           # use filler ts for grid load
                           
                           iso_mw = rep(1000, length(private$bldg_ts$kw))
                         }
                         else {
                           iso_mw = private$grid_ts$mw
                         }
                         
                         if(is.numeric(n)) {
                           steps = n
                         }
                         else {
                           steps = length(bldg_kw) - 1
                         }
                         
                         sim_df <- bind_rows(lapply(1:steps, function(i) {
                           if(log) {
                             flog.error(paste(timesteps[i], private$metadata[["ctrl_id"]]),
                                        name = "ctrlr")
                           }
                           self$operate(timesteps[i], bldg_kw[i], pv_kw[i], iso_mw[i])
                         }))
                         
                         private$sim_df = sim_df
                         
                         if (save_df) {
                           sim_path <- list(paste("outputs/", private$metadata[["run_id"]],
                                             sep = ""),
                                            "/df")
                           for (i in 1:length(sim_path)) {
                             curr_path = paste(sim_path[1:i], collapse ="")
                             if(!dir.exists(file.path(curr_path))) {
                               dir.create(file.path(curr_path))
                             }
                           }
                           nameplt_abrv <- paste(round(private$batt$nameplate, 2)*1000, "W", sep = "")
                           targ_abrv <- 100 - round(private$dmd_targ/max(bldg_kw), 2)*100
                           write.csv(sim_df, paste(curr_path, "/", private$metadata[["ctrl_id"]],
                                                   "_", private$batt$chem, nameplt_abrv, "_shave", targ_abrv,
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
                         return(private$disp)
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
                       disp = NULL,
                       grid_ts = NULL,
                       pv_ts = NULL,
                       metadata = NULL,
                       sim_df = NULL
                     )
)