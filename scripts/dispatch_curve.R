# Dispatch Curve Object

# This object can be called to operate 
# ike a dispatch curve, simulating
# the operation of grid resources
# in time, based on marginal cost

library("dplyr")
library("futile.logger")
library("R6")

disp_curv <- R6Class("Dispatch",
    public = list(
      marg_costs = NULL,
      all_plants = NULL,
      iso_terr = NULL,
      
      initialize = function(meta = NULL, terr = NULL, hold_seed = FALSE) {
        
        if (terr == "nyiso") {
          mc_path = "inputs/marg_costs.csv"
          plts_path = "inputs/plants_all.csv"
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
        log_path = paste("outputs/", meta[["run_id"]], "/",
                         meta[["name"]], "_", meta[["ctrl_id"]], "_",  
                         strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                         ".log", sep = ""
        )
        flog.appender(appender.file(log_path), name = "disp")
        
        self$set_mc_stats(read.csv(mc_path, head = T, stringsAsFactors = F)[,2:3])
        self$all_plants <- read.csv(plts_path, head = T, stringsAsFactors = F) %>% 
          filter(plngenan > 0)
        self$set_plants()
        self$attach_costs()
        self$stochastize_costs(hold_seed)
        self$set_dispatch()
        self$assign_colors()
      },
      
      add_metadata = function(metadata) {
        if (length(metadata) < 1) {
          # print(length(metadata))
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
        private$iso_plants <- private$iso_plants %>% 
          left_join(private$marg_costs_stats, by = "plprmfl") %>% 
          arrange(plprmfl)
        
        if (anyNA(private$iso_plants$MC_mean)) {
          flog.error(
                      paste("Fuels without marginal cost statistics in dispatch",
                            private$metadata[["name"]], private$metadata[["run_id"]],
                            private$metadata[["ctrl_id"]]
                            ),
                      name = "disp")
        }
      },
      
      get_emish = function(dmd_mw) {
        if(missing(dmd_mw)) {
          flog.error(paste("dmd_mw is missing"), name = "disp")
        }
        if(dmd_mw > max(private$disp_frame$cumul_cap)) {
          flog.error(paste("Demand (", dmd_mw, "kW) too high for ISO"))
        }
        active_plants <- private$disp_frame %>%
                            filter(cumul_cap < dmd_mw)
        cumul_plc2erta <- max(active_plants$cumul_plc2erta)
        
        return(cumul_plc2erta)
      },
      
      stochastize_costs = function(hold_seed) {
        # randomizes grid resource costs based on distribution of prices
        # with the same fuel type across resource types
        # (i.e. gas turbine v steam turbine)
        
        if (hold_seed){
          set.seed(4)
        }
        private$iso_plants <- private$iso_plants %>% 
          mutate(MC_rand = rnorm(nrow(.), MC_mean, MC_sd),
                 MC_rand = ifelse(MC_rand < 0, 0, MC_rand))
        
        self$set_dispatch()
      },
      
      set_mc_stats = function(file) {
        # Incorporates a std deviation of 1% of mean
        # where there is none in the data
        private$marg_costs_stats <- file %>% 
          group_by(prim_fuel) %>% 
          summarise_all(.funs = c("mean", "sd")) %>% 
          mutate(sd = ifelse(is.na(sd), 0, sd),
                 sd = ifelse(mean != 0 & sd == 0,
                             mean*0.05, sd)) %>% 
          rename("plprmfl" = prim_fuel,
                 "MC_mean" = mean,
                 "MC_sd" = sd) %>% 
          as.data.frame()
      },
      
      set_plants = function() {
        if (is.na(self$iso_terr)) {
          flog.error(
                      paste("No ISO territory in", private$metadata[["name"]], 
                            private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                            ),
                      name = "disp")
        }
        else {
          if (!any(self$all_plants$isorto == self$iso_terr)) {
            flog.error(
                        paste("No plants in", private$metadata[["name"]],
                              "in ISO territory", self$iso_terr,
                              private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                              ),
                        name = "disp")
          }
          private$iso_plants <- filter(self$all_plants, isorto == self$iso_terr) %>% 
            select(orispl, plprmfl, namepcap, plc2erta) 
          
          if (self$iso_terr == "NYISO") {
            sys.avail <- 0.87 # NYISO System Availability, source: Gilmore, 2010
            private$iso_plants$namepcap <- private$iso_plants$namepcap*sys.avail
          }
        }
      },
      
      set_dispatch = function(disp_frame) {
        disp_frame <- private$iso_plants %>%
          arrange(MC_rand) %>% 
          mutate(cumul_cap = cumsum(namepcap))
        
        if (anyNA(disp_frame$plc2erta)) {
          flog.error(paste("NAs in the emissions rates for some plants in",
                           self$iso_terr, "in", private$metadata[["name"]],
                           private$metadata[["run_id"]], private$metadata[["ctrl_id"]]
                          ),
                      name = "disp")
        }
        
        # Emissions rates are given in lb / MWh here. Can be converted to kg in 'plots.R'
        private$disp_frame <- disp_frame %>% 
          mutate(wtd_plc2erta = namepcap*plc2erta,
                 cumul_plc2erta = cumsum(wtd_plc2erta) / cumul_cap)
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
        return(private$iso_plants)
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