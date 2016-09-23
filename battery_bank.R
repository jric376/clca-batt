# Battery Bank Object

# This object can be called to operate like a battery bank, specified by
# its chemistry.

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
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
      
      initialize = function(
                            meta = NULL, type = NULL, nameplt = NULL
                            ) {
        self$add_metadata(meta)
        self$nameplate = nameplt
        self$pwr_rt = nameplt*20  # limits (dis)charge to 20C / hr in each timestep
        self$cap = nameplt        # C refers to nameplate (full) capacity
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
        # This function takes a kwh request for (dis)charge
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
        
        if (missing(soc_val)) return(self$soc)
        else self$soc <- self$soc + soc_val
        
        if (self$soc > 1) (self$soc <- 1)
        if (self$soc < self$min_soc) (self$soc <- self$min_soc)
        
        return(self)
      },
      change_cap = function(cap_val) {
        if (missing(cap_val)) return(self$cap)
        
        self$cap <- self$cap + cap_val
        self$del_kwh <- cap_val
        
        return(self)
      },
      incr_cyc = function(cyc_val) {
        if (missing(cyc_val)) return(self$cyc_eq)
        else self$cyc_eq <- self$cyc_eq + cyc_val
        
        fail_state = (self$cyc_eq >= self$cyc_fail)
        if (fail_state) {
          flog.error(paste(private$metadata[["name"]], "failed"),
                     name = "batt")
        }
        
        return(self)
      }
    ),
    private = list(
      metadata = NULL
    )
)

get_batt <- function(chem = NULL, kwh = NULL, interval = 1/12) {
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
                        nameplt = kwh
                        )
  
  return(bank)
}

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
