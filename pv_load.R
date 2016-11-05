# PV Load Object

# This object contains a time-series of generated solar power
# plus metadata.

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("data.table")
library("plyr")
library("dtplyr")
# library("dplyr")
library("foreach")
library("iterators")
library("doSNOW")
library("R6")
library("RSQLite")

pv_load <- R6Class("PV Load",
                     public = list(
                       
                       cell_eff = 0.17,
                       inv_eff = 0.96,
                       nameplate = 0.0,
                       sys_loss = 1 - 0.1408, # from NREL System Advisor Model
                       self_shade = 0.3644, # to avoid panels shading panels
                       # self_shade factor involves solving
                       # cos(panel_tilt) / (cos(panel_tilt) + sec(panel_tilt))
                       plc2erta = 1834, # kg CO2eq / kWp, inclulding BOS
                       cap_cost.lo = 2000,
                       cap_cost.hi = 5300,
                       om_cost.lo = 12,
                       om_cost.hi = 22.50,
                       
                       initialize = function(
                         meta = NA,
                         rand_copies = NA
                       ) {
                         
                         if (meta[["bldg"]] == "office") {
                           array_area = 1369 # m2
                         }
                         if (meta[["bldg"]] == "empty") {
                           array_area = 0 # m2
                         }
                         array_area <- array_area*self$self_shade
                         # nameplate calc assumes SolarWorld SW 280-290 MONO BLACK
                         self$nameplate <- 0.290*array_area/(1.675*1.001)
                         
                         self$add_ts_df(readRDS("inputs\\solar_min.rds"),
                                        rand_copies,
                                        array_area)
                         self$add_metadata(meta)
                       },
                       
                       add_metadata = function(metadata) {
                         if (length(metadata) < 1) {
                           private$metadata = 'Empty'
                         }
                         else {
                           for (datum_name in names(metadata)) {
                             private$metadata[[datum_name]] <- metadata[[datum_name]]
                           }
                         }
                       },
                       
                       add_ts_df = function(rds, copies, area) {
                         ts_df = list()
                         
                         dt <- select(rds, date_time)
                         ghi <- select(rds, min_ind, contains("ghi_min."))
                         
                         for (i in 1:copies) {
                           
                           ghi_samp <- select(ghi, sample(1:ncol(ghi), 1))
                           names(ghi_samp) <- "ghi"
                           ghi_samp <- mutate(ghi_samp,
                                          ghi = ghi*0.001,
                                          kw = ghi*area*self$sys_loss*self$cell_eff*self$inv_eff) %>%
                                        select(-ghi)
                           
                           ts_df[[i]] <- cbind(dt, ghi_samp)
                         }
                         
                         private$ts_df = ts_df
                       },
                       
                       set_interval = function(ts) {
                         start_pt = 2
                         interval.num = abs(as.numeric(
                                             (difftime(ts$date_time[start_pt],
                                                       ts$date_time[start_pt + 1],
                                                       units = "hours"))))
                         interval = list("time_int" = interval.num)
                         private$metadata = append(private$metadata, interval)
                         
                         return(interval.num)
                       },
                       
                       get_ts_count = function() {
                         return(length(private$ts_df))
                       },
                       
                       get_ts_df = function(index) {
                         if (missing(index)) return(private$ts_df)
                         if (index %in%  seq.int(1:length(private$ts_df))) {
                           return(private$ts_df[[index]])
                         }
                         else {
                           stop(paste(
                             "Index", index, "not in bounds.",
                             "It's between 1 and", length(private$ts_df)
                           )
                           )
                         }
                       },
                       
                       get_metadata = function() {
                         return(private$metadata)
                       }
                     ),
                     private = list(
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_pv <- function(run_id, type, copies = 0) {

  metadat = list(
    "bldg" = type,
    "run_id" = run_id,
    "copies" = copies
  )
  
  pv_test <- pv_load$new(
                        meta = metadat,
                        rand_copies = copies
  )
  
  return(pv_test)
}