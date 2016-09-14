# Battery Bank Object

# This object can be called to operate like a battery bank, specified by
# its chemistry.

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("futile.logger")
library('R6')

batt_bank <- R6Class("Batteries",
    public = list(
      # values + units from Hiremath 2015, Supplementary Information:
      # round-trip eff, min SoC, cycles to failure (80% cap), 
      # energy density (Wh / kg), Co2eq_rate (kg CO2eq / kg)
      
      chem = 'NaDa',
      pwr_rt = 0,
      nameplate = 0,
      cap = 0,
      del_kwh = 0.0,
      round_eff = 0.0,
      min_soc = 0.0,
      cyc_fail = 0,
      eng_dens = 0.0,
      soc = 0.0,
      cyc_eq = 0.0,
      co2eq_rt = 0.0,
      time_int = 0.0,
      
      initialize = function(
                            meta = NULL, type = NULL,
                            pwr = NULL, nameplt = NULL,
                            time_int = NULL
                            ) {
        self$add_metadata(meta)
        self$pwr_rt = pwr
        self$nameplate = nameplt
        self$cap = nameplt
        self$soc = 1
        self$add_type(type)
        self$time_int = time_int
        
        log_path = paste(
                          "outputs/", meta[["name"]], "_",
                          meta[["run_id"]], "_", meta[["ctrl_id"]], "_", 
                          strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                          ".log", sep = ""
                        )
        flog.appender(appender.file(log_path))
      },
      
      add_metadata = function(metadata) {
        if (length(metadata) < 1) {
          flog.error(
            paste("Empty metadata in battery with ", self$chem,
                  " and size ", self$nameplate
            )
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
                            )
          )
          stop("Empty battery type")
        }
        types = list(
          li_ion = list(
            chem = 'li_ion', round_eff = 0.9,
            min_soc = 0.2, cyc_fail = 10250,
            eng_dens = 140, co2eq_rt = 22
          ),
          pb_a = list(
            chem = 'pb_a', round_eff = 0.82,
            min_soc = 0.3, cyc_fail = 1250,
            eng_dens = 27, co2eq_rt = 2.7
          ),
          pb_a_r = list(
            chem = 'pb_a_r', round_eff = 0.82,
            min_soc = 0.3, cyc_fail = 1250,
            eng_dens = 27, co2eq_rt = 1.9
          )
        )
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
          
          chem = self$chem,
          cap = self$cap,
          del_kwh = self$del_kwh,
          soc = self$soc,
          cyc_eq = self$cyc_eq
          )
        
        return(state_params)
      },
      
      get_metadata = function() {
        return(private$metadata)
      }
    ),
    active = list(
      draw = function(kwh_val) {
        # This function takes a kwh request for (dis)charge
        # and returns the maximum dischargeable amount to meet
        # this demand
        
        print(paste("Starting capacity is", self$cap))
        
        usable_frac <- 1
        old_soc <- self$soc

        # NEEDS TO TAKE IN      self$time_int
        # CALC KW AND COMPARE TO PWR_RT AND SCALE KWH_VAL IF NEC.
        flog.warn(
                  paste(
                        "Still need to verify kW (based on",
                        kwh_val,
                        "kWh) isn't too high"
                  )
        )
        
        if (kwh_val < 0) {
          del_soc <- kwh_val / (self$round_eff*self$nameplate)
        }
        else {
          del_soc <- (kwh_val*self$round_eff) / self$nameplate
        }
        
        new_soc <- old_soc + del_soc
        
        print(paste("Old soc is", old_soc))
        print(paste("Del soc is", del_soc))
        
        self$change_soc <- del_soc
        
        if (new_soc <= self$min_soc) {
          if (old_soc > self$min_soc) {
            usable_frac <- (old_soc - self$min_soc) / (old_soc - new_soc)
          }
          else usable_frac <- 0
        }
        if (new_soc >= 1) {
          if (old_soc < 1) {
            usable_frac <- (1 - old_soc) / (new_soc - old_soc)
          }
          else usable_frac <- 0
        }
        print(paste("Usable frac", usable_frac))
        
        if (kwh_val < 0) {
          self$change_cap <- (kwh_val/self$round_eff)*usable_frac
        }
        else {
          self$change_cap <- kwh_val*self$round_eff*usable_frac
        }
        
        self$incr_cyc <- abs(del_soc / (1 - self$min_soc))*(usable_frac / 2)
        
        return(self)
      },
      change_soc = function(soc_val) {
        if (abs(soc_val) > 1) (return())
        
        if (missing(soc_val)) return(self$soc)
        else self$soc <- self$soc + soc_val
        
        if (self$soc > 1) (self$soc <- 1)
        if (self$soc < self$min_soc) (self$soc <- self$min_soc)
        
        print(paste("New soc is", self$soc))
        
        return(self)
      },
      change_cap = function(cap_val) {
        if (missing(cap_val)) return(self$cap)
        
        self$cap <- self$cap + cap_val
        self$del_kwh <- cap_val
        
        print(paste("Remaining capacity is", self$cap))
        
        return(self)
      },
      incr_cyc = function(cyc_val) {
        if (missing(cyc_val)) return(self$cyc_eq)
        else self$cyc_eq <- self$cyc_eq + cyc_val
        
        fail_state = (self$cyc_eq >= self$cyc_fail)
        if (fail_state) print('Battery failed')
        
        print(paste("New eq. cycles is", self$cyc_eq))
        
        return(self)
      }
    ),
    private = list(
      metadata = NULL
    )
)

metadat = list(
  "name" = "Boris the Battery",
  "run_id" = "RUNID",
  "ctrl_id" = "CTRLID",
  "run_timestr" = "RUNTIMESTR"
)
test_bank <- batt_bank$new(
                            meta = metadat, type = 'li_ion',
                            pwr = 50, nameplt = 65
                          )
test_bank$draw <- -52

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
