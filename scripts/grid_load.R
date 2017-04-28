# Grid Load Object

# This object contains a time-series of grid power demand
# plus metadata about the grid territory / energy mix, and
# get methods for stats describing the load profile.

# Assumes that the intervals b/w timesteps are constant

source("scripts/load_profile.R")

grid_load <- R6Class("Grid Load",
  inherit = Load_Profile,
  public = list(
   # The grid load has
   # a path pointing to time-series csv
   # a number of randomized copies of the orig series
   # a fractional value that scales the norm distribution
   # used in randomizing each copy (see stochastize_ts)

    add_base_ts = function(base_ts) {
       # Cleans base time-series by formatting date
       
      if (missing(base_ts)) {
        return("Base time-series data is missing")
      } else {
        base_ts <- base_ts %>%
                   mutate(date_time = as.POSIXct(strptime(time_5min,
                                                          format="%m/%d/%Y %H:%M"))) %>%
                   select(-time_5min)
         
        interval = self$set_interval(base_ts)
         
        private$base_ts = base_ts[2:nrow(base_ts),]
      }
    },
    
    stochastize_ts = function(copies = 1, rand_factor) {
     # Given a base time-series, this creates
     # a list of time-series, each randomized
     # point-by-point, with fluctuations picked
     # using a normal distribution, scaled by rand_factor
     
     mw_df <- select(private$base_ts, contains("mw"))
     
     if (copies > 0) {
       last_col <- paste0("mw.",copies)
                     
       mw_df <- sapply(1:copies, function(x) {
         private$base_ts %>%
           transmute(mw = mw*(1 + rnorm(nrow(private$base_ts),
                                        sd = rand_factor)))}) %>% 
         as.data.frame() %>% 
         cbind.data.frame(mw.0 = private$base_ts$mw)
     }
     
     ts_df <- cbind.data.frame(date_time = private$base_ts$date_time,
                               mw_df) %>% 
       rename_(.dots = setNames("mw.0", last_col))
     
     private$ts_df <- ts_df
    }
  )
)

get_grid <- function(run_id, terr, copies = 0, factor = 0.05) {
  # Default function for creating a grid load object
  # based on grid regional title, random copies and random factor
  
  metadat = list(
    "load_nm" = terr,
    "run_id" = run_id,
    "copies" = copies,
    "factor" = factor
  )
  if (terr == "nyiso") {
    path = "inputs/2014pal_combined.csv"
  }
    
  grid_test <- grid_load$new(
    ts_path = path, meta = metadat,
    rand_copies = copies, rand_factor = factor
  )
  
  return(grid_test)
}