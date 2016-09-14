# Dispatch Curve Object

# This object can be called to operate like a dispatch curve, simulating
# the operation of grid resources in time, based on marginal cost

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("ggplot2")
library("R6")

disp_curv <- R6Class("Dispatch",
    public = list(
      marg_costs = NULL,
      all_plants = NULL,
      iso_terr = NULL,
      
      initialize = function(meta = NULL, mc_path = NULL, plts_path = NULL, iso_terr = NULL) {
        
        self$iso_terr = iso_terr
        self$add_metadata(meta)
        # takes paths to marg cost and plants files as strings
        # reads in csvs
        
        # units:
        # heat_rt Btu/kWh, FC $/MMBtu, VOM $/kWh
        log_path = paste(
                          "outputs/", meta[["name"]], "_",
                          meta[["run_id"]], "_", meta[["ctrl_id"]], "_", 
                          strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                          ".log", sep = ""
        )
        flog.appender(appender.file(log_path))
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
            paste("Empty metadata in dispatch curve with ", self$iso_terr)
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
                            )
          )
        }
        self$stochastize_costs()
      },
      
      stochastize_costs = function(redraw = F) {
        # applies stochastizer to all lines in dataframe
        
        private$iso_plants$MC_rand = mapply(
                                              function(x) self$stochastizer(x),
                                              private$iso_plants$plprmfl
        )
        if (redraw) {
          self$set_dispatch()
          self$plot_dispatch()
        }
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
          print(fuel)
          print('This fuel type is throwing an error when stochastizing its price')
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
                            )
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
                              )
            )
          }
          private$iso_plants = subset(self$all_plants, self$all_plants$isorto == self$iso_terr)
          
          if (self$iso_terr == "NYISO") {
            sys.avail <- 0.87 # NYISO System Availability, source: Gilmore, 2010
            private$iso_plants$namepcap*sys.avail
          }
        }
      },
      
      set_dispatch = function() {
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
                            )
            )
        }
        disp_frame$wtd_plc2erta = disp_frame$namepcap*disp_frame$plc2erta
        disp_frame$wtd_plc2erta = cumsum(disp_frame$wtd_plc2erta)
        disp_frame$cumul_plc2erta = disp_frame$wtd_plc2erta / disp_frame$cumul_cap
        
        private$disp_frame = disp_frame
        
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
      },
      
      plot_dispatch = function() {
        self$assign_colors()
        disp_plot <- ggplot(
                            self$get_dispatch(),
                            aes(
                                x = cumul_cap, y = MC_rand,
                                size = namepcap
                                )
                            ) +
          geom_errorbar(
            aes(ymax = MC_rand + se, ymin = MC_rand - se),
            width = 1, colour = "black", size = 1,
            position = position_dodge(width = 1)
          ) +
          geom_point(aes(fill = fuel_type), shape = 21, alpha = 1/1.5) +
          scale_fill_brewer(name = "Fuel", type = "div", palette = "Set1",
                            guide = guide_legend(override.aes = list(alpha = 1, size = 5))
                            ) +
          scale_size(name = "Capacity (MW)", breaks = c(250,500,1000,2000), range = c(3,13)) +
          labs(x = "Cumulative Capacity (MW)",
               y = "Marg Cost ($ / kWh)",
               title = "Dispatch Curve") +
          theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
          theme(panel.grid.major = element_line(colour = "gray85")) +
          theme(panel.grid.minor = element_line(colour = "gray85")) +
          theme(legend.position = c(0.30, 0.70), legend.box = "horizontal",
                legend.background = element_rect(colour = "gray75")) +
          expand_limits(y = c(0, 0.32))
        
        return(disp_plot)
      }
    ),
    private = list(
      metadata = NULL,
      marg_costs_stats = NULL,
      iso_plants = NULL,
      disp_frame = NULL
    )
)

mc_input = "inputs/marg_costs.csv"
plants_input = "inputs/plants_all.csv"
iso_terr = "NYISO"

get_test_disp <- function() {
  metadat = list(
    "name" = "Doris the Dispatch",
    "run_id" = "RUNID",
    "ctrl_id" = "CTRLID",
    "run_timestr" = "RUNTIMESTR"
  )
  dispatch <- disp_curv$new(
    meta = metadat, mc_path = mc_input, 
    plts_path = plants_input, iso_terr = iso_terr
  )
  
  return(dispatch)
}

