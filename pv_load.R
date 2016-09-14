# PV Load Object

# This object contains a time-series of generated solar power
# plus metadata.

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library('foreach')
library('iterators')
library('doSNOW')
library('R6')

pv_load <- R6Class("PV Load",
                     public = list(
                       initialize = function(
                         meta = NA, pv_ts_path = NA,
                         rand_copies = NA, rand_factor = NA
                       ) {
                         self$add_metadata(meta)
                         self$add_base_ts(read.csv(pv_ts_path, head = T, stringsAsFactors = F))
                         # self$stochastize_ts(rand_copies, rand_factor)
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
                       
                       add_base_ts = function(base_ts) {
                         if (missing(base_ts)) {
                           return("Base time-series data is missing")
                         }
                         else {
                           base_ts$date_time = strptime(base_ts$time_hr, format="%m/%d/%Y %H:%M")
                           base_ts$time_hr = NULL
                           
                           start_pt = sample(1:(length(base_ts$date_time)-1),1)
                           interval = list(
                             "time_int" = abs(as.numeric(
                               (difftime(base_ts$date_time[start_pt],
                                         base_ts$date_time[start_pt + 1],
                                         units = "hours"))))
                           )
                           # base_ts$date_time = strftime(base_ts$date_time, format="%m/%d %H:%M:%S")
                           base_ts$kW = base_ts$PVinv_W*0.001*(1-0.1408) # loss factor taken from SAM
                           base_ts$PVinv_W = NULL
                           
                           private$metadata = append(private$metadata, interval)
                           private$base_ts = base_ts
                         }
                       },
                       
                       stochastize_ts = function(copies = 1, rand_factor) {
                         ts_df = list()
                         ts_df[[1]] = private$base_ts
                         cl <- makeCluster(3)
                         
                         for (i in 1:copies) {
                           registerDoSNOW(cl)
                           j = i + 1
                           new_ts = private$base_ts
                           
                           
                           foreach(x = iter(new_ts, by = 'col'), nm = colnames(new_ts)) %do%
                             if (is.numeric(x)) {
                               x = sapply(x, function(y) rnorm(1, mean = y, sd = y*rand_factor))
                               new_ts[[nm]] = x
                             }
                           
                           ts_df[[j]] = new_ts
                         }
                         stopCluster(cl)
                         
                         private$ts_df = ts_df
                       },
                       
                       get_base_ts = function() {
                         return(private$base_ts)
                       },
                       
                       get_ts_count = function() {
                         return(length(private$ts_df))
                       },
                       
                       get_ts_df = function() {
                         return(private$ts_df)
                       },
                       
                       get_metadata = function() {
                         return(private$metadata)
                       }
                     ),
                     private = list(
                       base_ts = NULL,
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_test_pv <- function() {
  metadat = list(
    "name" = "Penelope the PV_TS",
    "run_id" = "RUNID",
    "ctrl_id" = "CTRLID",
    "run_timestr" = "RUNTIMESTR"
  )
  pv_test <- pv_load$new(
    pv_ts_path = "inputs/solar_nycDC.csv",
    meta = metadat, rand_copies = 2, rand_factor = 0.05
  )
  
  return(pv_test)
}
