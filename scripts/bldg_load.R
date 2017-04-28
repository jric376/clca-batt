# Building Load Object

# This object containsa time-series of building energy
# consumption, plus metadata about the building, and
# get methods for stats describing the load profile.

# Assumes that the intervals b/w timesteps are constant

source("scripts/load_profile.R")

bldg_load <- R6Class("Bldg Load",
  inherit = Load_Profile,
  public = list(
   # The building load has
   # a path pointing to time-series csv
   # a number of randomized copies of the orig series
   # a fractional value that scales the norm distribution
      # used in randomizing each copy (see stochastize_ts)
   
   add_metadata = function(metadata) {
     if (length(metadata) < 1) {
       private$metadata$load_nm = 'Empty'
     }
     else {
       for (datum_name in names(metadata)) {
         private$metadata[[datum_name]] <- metadata[[datum_name]]
       }
     }
   },
   
   add_base_ts = function(base_ts) {
     # Cleans base time-series by
     # formatting date,
     # converting units (from J to kWh and kW)
     
     if (missing(base_ts)) {
       return("Base time-series data is missing")
     }
     else {
       base_ts <- base_ts %>%
         mutate(date_time = as.POSIXct(strptime(time_5min,
                                                format = "%m/%d %H:%M:%S")))
                    
       interval = self$set_interval(base_ts)
       base_ts <- base_ts %>%
                    mutate(kw = elec.J*(2.778E-7)/interval) %>%  # J to kW
                    select(-time_5min, -elec.J, -gas.J)
       private$base_ts = base_ts
     }
   },
   
   stochastize_ts = function(copies = 1, rand_factor) {
     # Given a base time-series, this creates
     # a list of time-series, each randomized
     # point-by-point, with fluctuations picked
     # using a normal distribution, scaled by rand_factor
     
     to_kwh <- function(x, int = private$metadata[["time_int"]]) {
       x/int
     }
     
     kw_df <- select(private$base_ts, contains("kw"))
     
     if (copies > 0) {
       last_col <- c(paste0("kw.",copies),
                     paste0("kwh.", copies))
       
       kw_df <- sapply(1:copies, function(x) {
         private$base_ts %>%
           transmute(kw = kw*(1 + rnorm(nrow(private$base_ts),
                                     sd = rand_factor)))}) %>% 
         as.data.frame() %>% 
         cbind.data.frame(kw.0 = private$base_ts$kw)
     }
     
     kwh_df <- select(kw_df, contains("kw")) %>% 
       mutate_all(to_kwh)
     colnames(kwh_df) <- gsub("^kw","kwh", colnames(kwh_df))
     
     ts_df <- cbind.data.frame(date_time = private$base_ts$date_time,
                               kw_df,
                               kwh_df)  %>% 
       rename_(.dots = setNames("kw.0", last_col[1])) %>% 
       rename_(.dots = setNames("kwh.0", last_col[2]))
     
     private$ts_df <- ts_df
  },
  
  get_ts_count = function() {
    # extra factor of 1/2 accounts for
    # kw and kwh cols in bldg load
    return(length(select(private$ts_df, -date_time))/2)
  },
  
  get_ts_df = function(index) {
    if (missing(index) | index == 0) {
      return(select(private$ts_df, date_time, kw, kwh))
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
  }
  )
)

get_bldg <- function(run_id, type, copies = 0, factor = 0.05) {
  # Default function for creating a building load object
  # based on building type, random copies and random factor
  
  if (type == "apt") {
    path = "inputs/apt_161202.csv"
  }
  if (type == "hospital") {
    path = "inputs/hospital_161208.csv"
  }
  if (type == "office") {
    path = "inputs/office_med_160929.csv"
  }
  if (type == "supermarket") {
    path = "inputs/supermarket_161116.csv"
  }
  
  metadat = list(
    "load_nm" = type,
    "run_id" = run_id,
    "copies" = copies,
    "factor" = factor
  )
  bldg_test <- bldg_load$new(
    ts_path =  path, meta = metadat,
    rand_copies = copies, rand_factor = factor
  )
  
  return(bldg_test)
}