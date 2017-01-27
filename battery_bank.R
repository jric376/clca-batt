# Battery Bank Object

# This object can be called to operate like a battery bank, specified by
# its chemistry and capacity (in kWh). It assumes 5min timesteps.

# battery characteristics are hard-coded (in add_type function),
# rather than being loaded from a csv, etc.

library("dplyr")
library("futile.logger")
library('R6')

batt_bank <- R6Class("Batteries",
    public = list(
      # Battery bank has variables that change while (dis)charging:
      
      # capacity (kWh), last (dis)charge (kWh)
      # state-of-charge, equivalent cycles
      
      cap = 0,
      del_kwh = 0.0,
      soc = 0.0,
      cyc_eq = 0.0,
      
      # and constants. These values (units) are
      # from Hiremath 2015, Supplementary Information:
      
      # battery chemistry, power rating (kW), nameplate capacity (kWh)
      # round-trip eff, min state-of-charge
      # cycles to failure lo/hi (rated at 80% cap), 
      # energy density (Wh / kg), emissions factor (kg CO2eq / kg batt),
      # cap costs lo/hi ($ / kWh), O&M costs lo/hi ($ / kWh),
      # replacement costs lo/hi (4 / kWh)
      
      chem = 'NaDa', pwr_rt = 0, nameplate = 0,
      round_eff = 0.0, min_soc = 0.0, 
      cyc_fail.lo = 0, cyc_fail.hi = 0,
      eng_dens = 0.0, plc2erta = 0.0,
      cap_cost.lo = 0, cap_cost.hi = 0,
      om_cost.lo = 0, om_cost.hi = 0,
      repl_cost.lo = 0, repl_cost.hi = 0,
      
      initialize = function(
                            meta = NULL, type = NULL, nameplate = NULL
                            ) {
        self$add_metadata(meta)
        self$nameplate = nameplate
        self$soc = 1
        self$add_type(type)
        
        log_path = paste(
                          "outputs\\", meta[["run_id"]], "\\",
                          meta[["name"]], "_", meta[["ctrl_id"]], "_",  
                          strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                          ".log", sep = ""
                        )
        flog.appender(appender.file(log_path), name = "batt")
        flog.threshold(ERROR, name = "batt")
        
        # limits pwr rate to depletion (given min_soc) in 1hr
        self$pwr_rt = (1-self$min_soc)*nameplate/(meta[["time_int"]]*self$round_eff)
        self$cap = nameplate
      },
      
      add_metadata = function(metadata) {
        if (length(metadata) < 1) {
          flog.error(
            paste("Empty metadata in battery with ", self$chem,
                  " and size ", self$nameplate
            ),
            name = "batt"
          )
        }
        else {
          for (datum_name in names(metadata)) {
            private$metadata[[datum_name]] <- metadata[[datum_name]]
          }
        }
      },
      
      add_type = function(type) {
        if (length(type) < 1) {
          flog.error(
                      paste("Empty battery type ",
                            private$metadata[["run_id"]],
                            private$metadata[["ctrl_id"]]
                            ),
                      name = "batt"
          )
          stop("Empty battery type")
        }
        types = list(
          vrf = list(
            chem = "vr_flow", round_eff = 0.75,
            min_soc = 0,
            cyc_fail.lo = 10000, cyc_fail.hi = 13000,
            eng_dens = 20, plc2erta = 2.7,
            cap_cost.lo = 342, cap_cost.hi = 1360,
            om_cost.lo = 3, om_cost.hi = 40,
            repl_cost.lo = 88, repl_cost.hi = 304
          ),
          li_ion = list(
            chem = "li_ion", round_eff = 0.9,
            min_soc = 0.2,
            cyc_fail.lo = 1000, cyc_fail.hi = 6000,
            eng_dens = 140, plc2erta = 22,
            cap_cost.lo = 513, cap_cost.hi = 1263,
            om_cost.lo = 8, om_cost.hi = 13,
            repl_cost.lo = 209, repl_cost.hi = 304
          ),
          nas = list(
            chem = "na_s", round_eff = 0.81,
            min_soc = 0.2,
            cyc_fail.lo = 2000, cyc_fail.hi = 4500,
            eng_dens = 116, plc2erta = 14.9,
            cap_cost.lo = 543, cap_cost.hi = 1600,
            om_cost.lo = 11, om_cost.hi = 32,
            repl_cost.lo = 269, repl_cost.hi = 1033
          ),
          pb_a = list(
            chem = "pb_a", round_eff = 0.82,
            min_soc = 0.25,
            cyc_fail.lo = 200, cyc_fail.hi = 4500,
            eng_dens = 27, plc2erta = 2.7,
            cap_cost.lo = 663, cap_cost.hi = 2255,
            om_cost.lo = 13, om_cost.hi = 56,
            repl_cost.lo = 333, repl_cost.hi = 686
          ),
          pb_a_r = list(
            chem = "pb_a_r", round_eff = 0.82,
            min_soc = 0.25,
            cyc_fail.lo = 200, cyc_fail.hi = 4500,
            eng_dens = 27, plc2erta = 1.9,
            cap_cost.lo = 663, cap_cost.hi = 2255,
            om_cost.lo = 13, om_cost.hi = 56,
            repl_cost.lo = 333, repl_cost.hi = 686
          )
        )
        
        # copies battery specs into properties of object 
        chosen_params <- types[[type]]
        for (param_name in names(chosen_params)) {
          self[[param_name]] <- chosen_params[[param_name]]
          # print(chosen_params[[param_name]])
        }
      },
      
      get_state = function() {
        # This function returns the most recent state
        # information about the battery
        # 1) delta kWh, 2) state-of-charge, 3) equivalent cycles
        
        state_params = list(
          "cap" = self$cap,
          "del_kwh" = self$del_kwh,
          "soc" = self$soc,
          "cyc_eq" = self$cyc_eq
          )
        
        return(state_params)
      },
      
      get_metadata = function() {
        return(private$metadata)
      }
    ),
    active = list(
      draw = function(kw_val) {
        # This function takes a request for (dis)charge in kW
        # and returns the maximum dischargeable amount to meet
        # this demand
        
        pwr_frac <- 1
        cap_frac <- 1
        
        if (kw_val < 0) {
          kw_val <- kw_val / self$round_eff
        }
        else {
          kw_val <- kw_val*self$round_eff
        }
        if (abs(kw_val) > self$pwr_rt) {
          pwr_frac <- self$pwr_rt / abs(kw_val)
          kw_val <- kw_val*pwr_frac
        }
        
        kwh_val <- kw_val*as.numeric(private$metadata[["time_int"]])
        del_soc <- kwh_val / self$nameplate
        old_cap <- self$cap
        old_soc <- self$soc
        
        new_soc <- old_soc + del_soc
        self$change_soc <- del_soc
        
        if (new_soc <= self$min_soc) {
          if (old_soc > self$min_soc) {
            cap_frac <- (old_soc - self$min_soc) / (old_soc - new_soc)
          }
          else cap_frac <- 0
        }
        if (new_soc >= 1) {
          if (old_soc < 1) {
            cap_frac <- (1 - old_soc) / (new_soc - old_soc)
          }
          else cap_frac <- 0
        }
        
        self$change_cap <- kwh_val*cap_frac
        self$incr_cyc <- abs(del_soc / (1 - self$min_soc))*(cap_frac / 2)

        flog.info(paste("Starting cap -", round(old_cap, 2), "-",
                        "Old SoC -", round(old_soc, 2), "-",
                        "Del SoC -", round(del_soc, 2), "-",
                        "New SoC -", round(self$soc, 2), "-",
                        "Pwr frac", round(pwr_frac, 2), "-",
                        "Cap frac", round(cap_frac, 2), "-",
                        "Remaining capacity is", round(self$cap, 2)),
                  name = "ctrlr"
        )
        
        return(self)
      },
      change_soc = function(soc_val) {
        # Calculates a new state-of-charge
        # based on the supplied delta SoC, e.g. "soc_val"
        
        if (missing(soc_val)) return(self$soc)
        else self$soc <- self$soc + soc_val
        
        if (self$soc > 1) (self$soc <- 1)
        if (self$soc < self$min_soc) (self$soc <- self$min_soc)
        
        return(self)
      },
      change_cap = function(cap_val) {
        # Calculates a new battery bank capacity
        # based on the supplied delta kWh, e.g. "cap_val"
        
        if (missing(cap_val)) return(self$cap)
        
        self$cap <- self$cap + cap_val
        self$del_kwh <- cap_val
        
        return(self)
      },
      incr_cyc = function(cyc_val) {
        # Increments equivalent cycles of the battery object
        # based on supplied number of equiv cycles, e.g. "cyc_val"
        if (missing(cyc_val)) return(self$cyc_eq)
        else self$cyc_eq <- self$cyc_eq + cyc_val
        
        return(self)
      }
    ),
    private = list(
      metadata = NULL
    )
)

get_batt <- function(chem = NULL, kwh = NULL, interval = 1/12) {
  # This is the function for creating a battery object,
  # which makes use of the R6 initialization function ($new)
  # most of the metadata is filler; only "time_int" matters
  # in the current setup of the simulations
  
  if (is.null(chem) | !is.numeric(kwh)) {
    return()
  }
  metadat = list(
    "name" = "Boris the Battery",
    "run_id" = "RUNID",
    "ctrl_id" = "CTRLID",
    "run_timestr" = "RUNTIMESTR",
    "time_int" = interval
  )
  interval <- as.numeric(interval)
  bank <- batt_bank$new(
                        meta = metadat,
                        type = chem,
                        nameplate = kwh
                        )
  
  return(bank)
}

# The following should really be integrated with tests in the
# "testthat" package. Still a work in progress

deplete_test <- function(test_batt) {
  test_batt$draw <- -0.6*test_batt$nameplate
  test_batt$draw <- -0.1*test_batt$nameplate
  test_batt$draw <- -0.1*test_batt$nameplate
}

fill_test <- function(test_batt) {
  test_batt$draw <- -0.6*test_batt$nameplate
  test_batt$draw <- 0.5*test_batt$nameplate
  test_batt$draw <- 0.3*test_batt$nameplate
}
