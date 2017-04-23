# PV Load Object

# This object contains a time-series of generated solar power
# plus metadata.

# Assumes constant time intervals

library("data.table")
source("scripts/load_profile.R")

pv_load <- R6Class("PV Load",
  inherit = Load_Profile,
  public = list(
    # The PV load has hard-coded parameters:
   
    # cell efficiency, inverter eff., nameplate kWp
    # system losses (from NREL SAM), self-shade factor,
    # emissions factor inclulding BOS (kg CO2eq / kWp),
    # cap costs lo/hi ($/kW), O&M costs lo/hi ($/kW-yr)
   
    # self_shade factor involves solving
    # for maximum length of array shadows
    # which occurs on winter solstice.
    # the value here reflects the avg 
    # of shadow length in the morning
   
    # cos(180-solar_azimuth) / tan(solar_elevation)
   
    # and in the afternoon
   
    # cos(solar_azimuth-180) / tan(solar_elevation)
   
    # for New York City, NY.
   
    # These values can be accessed for
    # locations worldwide at
    # https://www.esrl.noaa.gov/gmd/grad/solcalc/
   
    cell_eff = 0.17, inv_eff = 0.93, nameplate = 0.0,
    sys_loss = 1 - 0.1408, self_shade = 0.3644,
    plc2erta = 1834,
    cap_cost.lo = 2000, cap_cost.hi = 5300,
    om_cost.lo = 12, om_cost.hi = 22.50,
   
    initialize = function(
      meta = NA,
      rand_copies = NA
    ) {
     
      if (meta[["bldg"]] == "apt") {
        array_area = 628 # m2
      }
      if (meta[["bldg"]] == "hospital") {
        array_area = 3735 # m2
      }
      if (meta[["bldg"]] == "office") {
        array_area = 1369 # m2
      }
      if (meta[["bldg"]] == "supermarket") {
        array_area = 3717 # m2
      }
      if (meta[["bldg"]] == "empty") {
        array_area = 0 # m2
      }
      array_area <- array_area*self$self_shade
      # nameplate calc assumes SolarWorld SW 280-290 MONO BLACK
      self$nameplate <- 0.290*array_area/(1.675*1.001)
      self$cap_cost.lo <- self$cap_cost.lo*self$nameplate
      self$cap_cost.hi <- self$cap_cost.hi*self$nameplate
    
      self$add_ts_df(readRDS("inputs/solar_min.rds"),
                     rand_copies,
                     array_area)
      self$add_metadata(meta)
   },
   
   add_ts_df = function(rds, copies, area) {
     # The PV object selects randomized copies
     # of generation time-series
     # from a r-data object (rds)
     # that gets built in solar_data.R
     
     ts_df = list()
     
     dt <- select(rds, date_time)
     ghi <- select(rds, min_ind, contains("ghi_min."))
     
     mw_df <- sapply(1:copies, function(x) {
       ghi_samp <- select(ghi, sample(1:ncol(ghi), 1))
       names(ghi_samp) <- "ghi"
        
       ghi_samp <- ghi_samp %>% 
         transmute(kw = ghi*area*self$sys_loss*self$cell_eff*self$inv_eff)}) %>% 
       as.data.frame()
     
     ts_df <- cbind.data.frame(dt, mw_df)
     
     private$ts_df = ts_df
   }
 ),
 private = list(
   ts_df = NULL, # takes a list of dataframes
   metadata = NULL
 )
)

get_pv <- function(run_id, type, copies = 0) {
  # Default function for creating a solar generation object
  # based on building type (sets size of array)
  # and number of random copies
  
  metadat = list(
    "bldg" = type,
    "run_id" = run_id,
    "copies" = copies
  )
  pv_test <- pv_load$new(meta = metadat,
                         rand_copies = copies)
  
  return(pv_test)
}