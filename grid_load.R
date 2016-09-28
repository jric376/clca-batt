# Grid Load Object

# This object contains a time-series of grid power demand
# plus metadata about the grid territory / energy mix, and
# get methods for stats describing the load profile.

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library('foreach')
library('iterators')
library('doSNOW')
library('R6')

grid_load <- R6Class("Grid Load",
                     public = list(
                       initialize = function(
                                              meta = NA, grid_ts_path = NA,
                                              rand_copies = NA, rand_factor = NA
                                             ) {
                       self$add_metadata(meta)
                       self$add_base_ts(read.csv(grid_ts_path, head = T, stringsAsFactors = F))
                       self$stochastize_ts(rand_copies, rand_factor)
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
                         base_ts$date_time = strptime(base_ts$time_5min, format="%m/%d/%Y %H:%M")
                         base_ts$date_time = as.POSIXct(base_ts$date_time)
                         base_ts$time_5min = NULL
                         
                         start_pt = sample(1:(length(base_ts$date_time)-1),1)
                         interval = list(
                                        "time_int" = abs(as.numeric(
                                                        (difftime(base_ts$date_time[start_pt],
                                                        base_ts$date_time[start_pt + 1],
                                                        units = "hours"))))
                         )
                         # base_ts$date_time = strftime(base_ts$date_time, format="%m/%d %H:%M:%S")
                         
                         private$metadata = append(private$metadata, interval)
                         private$base_ts = base_ts
                       }
                     },
                     
                     stochastize_ts = function(copies = 1, rand_factor) {
                       ts_df = list()
                       ts_df[[1]] = private$base_ts
                       
                       if (copies > 0) {
                         
                         for (i in 1:copies) {
                           j = i + 1
                           new_ts = private$base_ts
                           
                           
                           foreach(x = iter(new_ts, by = 'col'), nm = colnames(new_ts)) %dopar%
                             if (is.numeric(x)) {
                               x = sapply(x, function(y) rnorm(1, mean = y, sd = y*rand_factor))
                               new_ts[[nm]] = x
                             }
                           
                           ts_df[[j]] = new_ts
                         }
                       }
                       
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
                       base_ts = NULL,
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_grid <- function(run_id, terr, copies = 0, factor = 0.1) {
  metadat = list(
    "territory" = terr,
    "run_id" = run_id,
    "copies" = copies,
    "factor" = factor
  )
  if (terr == "nyiso") {
    path = "inputs\\2014pal_combined.csv"
  }
    
  grid_test <- grid_load$new(
    grid_ts_path = path,
    meta = metadat, rand_copies = copies, rand_factor = factor
  )
  
  return(grid_test)
}
