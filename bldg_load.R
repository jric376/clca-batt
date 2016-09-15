# Building Load Object

# This object containsa time-series of building energy
# consumption, plus metadata about the building, and
# get methods for stats describing the load profile.

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library('foreach')
library('iterators')
library('doSNOW')
library('R6')

bldg_load <- R6Class("Bldg Load",
                     public = list(
                       initialize = function(meta = NULL, bldg_ts_path = NULL,
                                             rand_copies = NULL, rand_factor = NULL) {
                         self$add_metadata(meta)
                         self$add_base_ts(read.csv(bldg_ts_path, head = T, stringsAsFactors = F))
                         self$stochastize_ts(rand_copies, rand_factor)
                       },
                       
                       add_metadata = function(metadata) {
                         if (length(metadata) < 1) {
                           private$metadata$bldg_nm = 'Empty'
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
                           base_ts$date_time = strptime(base_ts$time_5min, format="%m/%d %H:%M:%S")
                           base_ts$time_5min = NULL
                           base_ts$kwh = base_ts$elec.J*(2.778E-7) # J to kWh
                           base_ts$elec.J = NULL
                           base_ts$gas.J = NULL
                           
                           start_pt = sample(1:(length(base_ts$date_time)-1),1)
                           interval = list(
                             "time_int" = abs(as.numeric(
                               (difftime(base_ts$date_time[start_pt],
                                         base_ts$date_time[start_pt + 1],
                                         units = "hours"))))
                           )
                           # base_ts$date_time = strftime(base_ts$date_time, format="%m/%d %H:%M:%S")
                           
                           private$metadata = append(private$metadata, interval)
                           base_ts$kw = base_ts$kwh*private$metadata$time_int
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
                       
                       get_ts_df = function(index) {
                         if (missing(index)) return(private$ts_df)
                         if (index %in%  seq.int(1:length(private$ts_df))) {
                          return(private$ts_df[index])
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
                       base_ts = NULL,
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_test_bldg <- function() {
  metadat = list(
    "name" = "Bob the Building_TS",
    "run_id" = "RUNID",
    "ctrl_id" = "CTRLID",
    "run_timestr" = "RUNTIMESTR"
  )
  bldg_test <- bldg_load$new(
    bldg_ts_path = "inputs/bldg_gasheat.csv",
    meta = metadat, rand_copies = 4, rand_factor = 0.1
  )
  
  return(bldg_test)
}
