# Load Profile Object

# This is the interface object for other objects which contain
# - a time-series of energy consumption
# - metadata about the load profile
# - get methods for stats describing the load profile

# Assumes that the intervals b/w timesteps are constant

library("plyr")
library("dplyr")
library("foreach")
library("iterators")
library("doSNOW")
library("R6")

Load_Profile <- R6Class("Load_Profile",
  public = list(
   # The building load has
   # a path pointing to time-series csv
   # a number of randomized copies of the orig series
   # a fractional value that scales the norm distribution
   # used in randomizing each copy (see stochastize_ts)
   
   initialize = function(meta = NULL, ts_path = NULL,
                         rand_copies = NULL, rand_factor = NULL) {
     self$add_metadata(meta)
     self$add_base_ts(data.table::fread(ts_path, head = TRUE,
                        stringsAsFactors = FALSE))
     self$stochastize_ts(rand_copies, rand_factor)
  },
   
    add_metadata = function(metadata) {
      if (length(metadata) < 1) {
        private$metadata$load_nm = 'Empty'
      } else {
        for (datum_name in names(metadata)) {
          private$metadata[[datum_name]] <- metadata[[datum_name]]
        }
      }
    },
   
    add_base_ts = function(base_ts) {
      # Cleans base time-series by
      # formatting date and converting units
      
      stop("This is the superclass add_base_ts method.")
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
      
      stop("This is the superclass stochastize_ts method.")
    },
   
    get_base_ts = function() {
      return(private$base_ts)
    },
   
    get_ts_count = function() {
      return(length(select(private$ts_df, -date_time)))
    },
    
    get_ts_df = function(index) {
      if (missing(index) | index == 0) {
        return(select(private$ts_df, date_time, 2))
      } else {
        if (index == "full") {
          return(private$ts_df)
        } else {
          df <- select(private$ts_df, date_time, contains(paste0(".",index)))
          colnames(df) <- gsub(".\\d+$","", colnames(df))
          return(df)
        }
      }
      return(NULL)
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