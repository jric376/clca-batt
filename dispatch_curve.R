# Dispatch Curve Object

# This object can be called to operate like a dispatch curve, simulating
# the operation of grid resources in time, based on marginal cost

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("R6")

disp_curv <- R6Class("Dispatch",
    public = list(
      marg_costs = NULL,
      all_plants = NULL,
      iso_terr = NULL,
      
      initialize = function(meta = NULL, terr = NULL) {
        
        if (terr == "nyiso") {
          mc_path = "inputs\\marg_costs.csv"
          plts_path = "inputs\\plants_all.csv"
          terr = toupper(terr)
        }
        else {
          stop("No paths given for marginal costs or plants on the grid.")
        }
        self$iso_terr = terr
        self$add_metadata(meta)
        # takes paths to marg cost and plants files as strings
        # reads in csvs
        
        # units:
        # heat_rt Btu/kWh, FC $/MMBtu, VOM $/kWh
        log_path = paste(
                          "outputs\\", meta[["run_id"]], "\\",
                          meta[["name"]], "_", meta[["ctrl_id"]], "_",  
                          strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                          ".log", sep = ""
        )
        flog.appender(appender.file(log_path), name = "disp")
        self$marg_costs = read.csv(mc_path, head = T, stringsAsFactors = F)[,2:3]
        self$set_mc_stats()
        self$all_plants = read.csv(plts_path, head = T, stringsAsFactors = F)
        self$set_plants()
        self$attach_costs()
        self$set_dispatch()
      },
      
      add_metadata = function(metadata) {
        if (length(metadata) < 1) {
          print(length(metadata))
          flog.error(
            paste("Empty metadata in dispatch curve with ", self$iso_terr),
            name = "disp"
          )
        }
        else {
          for (datum_name in names(metadata)) {
            private$metadata[[datum_name]] <- metadata[[datum_name]]
          }
        }
      },
      
      attach_costs = function() {
        private$iso_plants = merge(
                               x = private$iso_plants, y = private$marg_costs_stats,
                               by.x = "plprmfl", by.y = "prim_fuel", suffixes = "",
                               all.x = T
        )
        if (anyNA(private$iso_plants$MC)) {
          flog.error(
                      paste(
                            "Fuels without marginal cost statistics in dispatch",
                            private$metadata[["name"]], private$metadata[["run_id"]],
                            private$metadata[["ctrl_id"]]
                            ),
                      name = "disp"
          )
        }
        self$stochastize_costs()
      },
      
      operate = function(dmd_mw) {
        if(missing(dmd_mw)) {
          flog.error(paste("dmd_mw is missing"), name = "disp")
        }
        cumul_plc2erta <- self$get_emish(dmd_mw)
        self$stochastize_costs()
        return(cumul_plc2erta)
      },
      
      get_emish = function(dmd_mw) {
        active_plants <- private$disp_frame %>%
                        filter(cumul_cap < dmd_mw)
        cumul_co2eq <- max(active_plants$cumul_plc2erta)
        
        return(cumul_co2eq)
      },
      
      stochastize_costs = function() {
        # applies stochastizer to all lines in dataframe
        
        private$iso_plants$MC_rand = mapply(
                                              function(x) self$stochastizer(x),
                                              private$iso_plants$plprmfl
        )
        self$set_dispatch()
      },
      
      stochastizer = function(fuel, hold_seed = F) {
        # randomizes grid resource costs based on distribution of prices
        # with the same fuel type across resource types
        # (i.e. gas turbine v steam turbine)
        
        if (hold_seed){
          set.seed(4)
        }
        
        ref_line <- subset(private$marg_costs_stats, private$marg_costs_stats$prim_fuel == fuel)
        # print(ref_line)
        mean_val <- ref_line$MC
        sd_val <- ref_line$stdev
        
        new_mc <- rnorm(1, mean=mean_val, sd=sd_val)
        if (is.na(new_mc)) {
          flog.error(paste(fuel, "(fuel type is throwing an error when stochastizing its price"),
                     name = "disp")
        }
        
        return(new_mc)
      },
      
      set_mc_stats = function() {
        mc_stats = aggregate(MC ~ prim_fuel, self$marg_costs, mean)
        mc_stats$stdev <- aggregate(MC ~ prim_fuel, self$marg_costs, sd)$MC
        mc_stats[is.na(mc_stats)] = 0
        
        private$marg_costs_stats = mc_stats
      },
      
      set_plants = function() {
        if (is.na(self$iso_terr)) {
          flog.error(
                      paste(
                            "No ISO territory in", private$metadata[["name"]], 
                            private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                            ),
                      name = "disp"
          )
        }
        else {
          self$all_plants = subset(self$all_plants, self$all_plants$plngenan > 0)
          if (!any(self$all_plants$isorto == self$iso_terr)) {
            flog.error(
                        paste(
                              "No plants in", private$metadata[["name"]],
                              "in ISO territory", self$iso_terr,
                              private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                              ),
                        name = "disp"
            )
          }
          private$iso_plants = subset(self$all_plants, self$all_plants$isorto == self$iso_terr)
          
          if (self$iso_terr == "NYISO") {
            sys.avail <- 0.87 # NYISO System Availability, source: Gilmore, 2010
            private$iso_plants$namepcap*sys.avail
          }
        }
      },
      
      set_dispatch = function(disp_frame) {
        disp_frame = data.frame(
                                    private$iso_plants$orispl, private$iso_plants$plprmfl,
                                    private$iso_plants$MC, private$iso_plants$MC_rand,
                                    private$iso_plants$stdev,
                                    private$iso_plants$namepcap, private$iso_plants$plc2erta,
                                    check.names = F
                                      )
        
        colnames(disp_frame) = c("orispl", "plprmfl", "MC","MC_rand",
                                 "se", "namepcap", "plc2erta")
        disp_frame = arrange(disp_frame, MC_rand)
        disp_frame$cumul_cap = cumsum(disp_frame$namepcap)
        
        if (anyNA(disp_frame$plc2erta)) {
          flog.error(
                      paste(
                            "NAs in the emissions rates for some plants in",
                            self$iso_terr, "in", private$metadata[["name"]],
                            private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                            ),
                      name = "disp"
            )
        }
        disp_frame$wtd_plc2erta = disp_frame$namepcap*disp_frame$plc2erta
        disp_frame$cumul_plc2erta = cumsum(disp_frame$wtd_plc2erta)
        disp_frame$cumul_plc2erta = disp_frame$cumul_plc2erta / disp_frame$cumul_cap
        
        private$disp_frame = disp_frame
        self$assign_colors()
        
        return(self)
      },
      
      get_metadata = function() {
        return(private$metadata)
      },
      
      get_dispatch = function() {
        return(private$disp_frame)
      },
      
      get_mc_stats = function() {
        return(private$marg_costs_stats)
      },
      
      get_iso_plants = function(iso = self$iso_terr) {
        if (iso == self$iso_terr){
          # print(length(private$iso_plants$isorto))
          return(private$iso_plants)
        }
        if (missing(iso)) {
          return("Need to select a subset of plants")
        }
        else {
          output_plants = subset(self$all_plants, self$all_plants$isorto == iso)
          # print(length(output_plants$isorto))
          return(output_plants)
        }
      },
      
      assign_colors = function() {
        
        coal <- c("BIT", "LIG", "SC", "SGC", "SUB", "WC")
        petroleum <- c("DFO", "JF", "KER", "PC", "RFO", "WO")
        biomass <- c("BLQ", "DG", "MSB", "OBG", "OBS", "WDL", "WDS")
        
        private$disp_frame$fuel_type <- "black"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "WAT"] <- "Hydro"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "SUN"] <- "PV"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "WND"] <- "Wind"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "NG"] <- "Nat. Gas"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "NUC"] <- "Nuclear"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl == "LFG"] <- "Landfill Gas"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl %in% biomass] <- "Biomass"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl %in% petroleum] <- "Petro-fuels"
        private$disp_frame$fuel_type[private$disp_frame$plprmfl %in% coal] <- "Coal-based"
        
        # ADD LEVELS c(...) and then reassign fuel_type with factor(fuel_type, LEVELS)
      }
    ),
    private = list(
      metadata = NULL,
      marg_costs_stats = NULL,
      iso_plants = NULL,
      disp_frame = NULL
    )
)


