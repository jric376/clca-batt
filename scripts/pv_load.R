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
    
    lat = 40.783, tilt = 40.783,
    cell_eff = 0.17, inv_eff = 0.93, nameplate = 0.0,
    sys_loss = 1 - 0.1408, self_shade = 0.3644,
    plc2erta = 1834,
    cap_cost.lo = 2000, cap_cost.hi = 5300,
    om_cost.lo = 12, om_cost.hi = 22.50,
   
    initialize = function(
      meta = NA,
      rand_copies = NA
    ) {
     
      if (meta[["load_nm"]] == "apt") {
        array_area = 628 # m2
      }
      if (meta[["load_nm"]] == "hospital") {
        array_area = 3735 # m2
      }
      if (meta[["load_nm"]] == "office") {
        array_area = 1369 # m2
      }
      if (meta[["load_nm"]] == "supermarket") {
        array_area = 3717 # m2
      }
      if (meta[["load_nm"]] == "empty") {
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
     
     dt <- select(rds, date_time, dhi:declin_ang,
                  -kt, -tempC, -clr_ghi) %>% 
       mutate(incid_ang = cospi(self$lat-self$tilt)*cos(declin_ang)*cos(hr_ang)+sinpi(self$lat-self$tilt)*sin(declin_ang),
              znith_ang = cospi(self$lat)*cos(declin_ang)*cos(hr_ang)+sinpi(self$lat)*sin(declin_ang),
              rb_fac = incid_ang/cos(znith_ang),
              rd_fac = (1+cospi(self$tilt))/2,
              rr_fac = (1-cospi(self$tilt))/2,
              incid_tot = dni*rb_fac + dhi*rd_fac + 0.2*ghi*rr_fac)
     kt <- select(rds, min_ind, contains("kt_min."))
     
     kw_df <- sapply(1:copies, function(x) {
       kt_samp <- select(kt, sample(2:ncol(kt), 1))
       names(kt_samp) <- "kt"
        
       kt_samp <- kt_samp %>%
         transmute(kw = 0.001*dt$incid_tot*kt*area*self$sys_loss*self$cell_eff*self$inv_eff)
       }) %>% 
       as.data.frame()
     
     ts_df <- cbind.data.frame(dt, kw_df) %>% 
       select(date_time, contains("kw"))
     
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
    "load_nm" = type,
    "run_id" = run_id,
    "copies" = copies
  )
  pv_test <- pv_load$new(meta = metadat,
                         rand_copies = copies)
  
  return(pv_test)
}