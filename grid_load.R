# Grid Load Object

# This object contains a time-series of grid power demand
# plus metadata about the grid territory / energy mix, and
# get methods for stats describing the load profile.

# Assumes that the intervals b/w timesteps are constant

library("plyr")
library("dplyr")
library('foreach')
library('iterators')
library('doSNOW')
library('R6')

grid_load <- R6Class("Grid Load",
                     public = list(
                       # The grid load has
                       # a path pointing to time-series csv
                       # a number of randomized copies of the orig series
                       # a fractional value that scales the norm distribution
                       # used in randomizing each copy (see stochastize_ts)
                       
                       initialize = function(
                                              meta = NA, grid_ts_path = NA,
                                              rand_copies = NA, rand_factor = NA
                                             ) {
                       self$add_metadata(meta)
                       self$add_base_ts(fread(grid_ts_path, header = TRUE, stringsAsFactors = FALSE))
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
                       # Cleans base time-series by formatting date
                       
                       if (missing(base_ts)) {
                         return("Base time-series data is missing")
                       }
                       else {
                         base_ts <- base_ts %>%
                                    mutate(date_time = as.POSIXct(strptime(time_5min,
                                                                           format="%m/%d/%Y %H:%M"))) %>%
                                    select(-time_5min)
                         
                         interval = self$set_interval(base_ts)
                         
                         private$base_ts = base_ts[2:nrow(base_ts),]
                       }
                     },
                     
                     set_interval = function(ts) {
                       # Checks interval b/w 2nd and 3rd timesteps
                       # copies the time difference into object metadata
                       
                       start_pt = 2
                       interval.num = abs(as.numeric(
                                           (difftime(ts$date_time[start_pt],
                                                     ts$date_time[start_pt + 1],
                                                     units = "hours"))))
                       interval = list("time_int" = interval.num)
                       private$metadata = append(private$metadata, interval)
                       
                       return(interval.num)
                     },
                     
                     stochastize_ts = function(copies = 1, rand_factor) {
                       # Given a base time-series, this creates
                       # a list of time-series, each randomized
                       # point-by-point, with fluctuations picked
                       # using a normal distribution, scaled by rand_factor
                       
                       ts_df = list()
                       ts_df[[1]] = private$base_ts
                       
                       if (copies > 0) {
                         
                         for (i in 1:copies) {
                           j = i + 1
                           new_ts = private$base_ts %>%
                                      mutate(mw = mw*(1 + rnorm(nrow(private$base_ts), sd = rand_factor)))
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
  # Default function for creating a grid load object
  # based on grid regional title, random copies and random factor
  
  metadat = list(
    "territory" = terr,
    "run_id" = run_id,
    "copies" = copies,
    "factor" = factor
  )
  if (terr == "nyiso") {
    path = "inputs/2014pal_combined.csv"
  }
    
  grid_test <- grid_load$new(
    grid_ts_path = path,
    meta = metadat, rand_copies = copies, rand_factor = factor
  )
  
  return(grid_test)
}
