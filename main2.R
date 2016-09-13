# Grid Dispatch Curve Creation

# This reads in csvs containing information about grid resources and
# generates a dispatch curve based on their marginal costs

## SOURCES
# Load profile - NYISO 2014
# Grid resources - EPA eGrid 2012

rm(list=ls())
wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
setwd(as.character(wd_path))
library("data.table")
library("plyr")

# Acronyms from EPA eGrid2012 documentation
coal <- c("BIT", "LIG", "SC", "SGC", "SUB", "WC")
petroleum <- c("DFO", "JF", "KER", "PC", "RFO", "WO")
biomass <- c("BLQ", "DG", "MSB", "OBG", "OBS", "WDS")

marg.costs <- read.csv("inputs/marg_costs.csv", head = T, stringsAsFactors = F)[,2:3] # heat_rt Btu/kWh, FC $/MMBtu, VOM $/kWh
marg.costs.stats <- aggregate(MC ~ prim_fuel, marg.costs, mean)
marg.costs.stats$stdev <- aggregate(MC ~ prim_fuel, marg.costs, sd)$MC
marg.costs.stats[is.na(marg.costs.stats)] <- 0

sys.avail <- 0.87 # NYISO System Availability, source: Gilmore, 2010

plant.table <- read.csv("inputs/plants_all.csv", head = T, stringsAsFactors = F)
gen.table <- read.csv("inputs/gens_all.csv", head = T, stringsAsFactors = F) # not used at the moment
plant.table <- subset(plant.table, plant.table$plngenan > 0) # removes non-negative generation
nyiso.plants <- subset(plant.table, plant.table$isorto == 'NYISO')  # isolates NYISO plants
nyiso.plants$namepcap <- nyiso.plants$namepcap*sys.avail # reduces plant capacities by factor
nyiso.plants <- merge(
                      x = nyiso.plants, y = marg.costs.stats,  
                      by.x = 'plprmfl', by.y = 'prim_fuel', suffixes = ''
                      )
# attaches marginal cost based on primary fuel

stochastize_MC <- function(fuel, hold_seed = F) {
  # use this code in attaching grid marg costs in order to
  # test effect of randomness on shape / sequence of dispatchs
  
  if (hold_seed){
    set.seed(4)
  }
  
  ref_line <- subset(marg.costs.stats, marg.costs.stats$prim_fuel == fuel)
  # print(ref_line)
  mean_val <- ref_line$MC
  sd_val <- ref_line$stdev
  
  new_mc <- rnorm(1, mean=mean_val, sd=sd_val)
    
  return(new_mc)
}

nyiso.plants$MC_rand <- copy(nyiso.plants$MC)
nyiso.plants.slim <- data.frame(
                                nyiso.plants$orispl, nyiso.plants$plprmfl,
                                nyiso.plants$MC, nyiso.plants$MC_rand,
                                nyiso.plants$namepcap, nyiso.plants$plc2erta
                                )

# randomizes marginal costs within each primary fuel, based on distribution among prime mover
nyiso.plants.slim$nyiso.plants.MC_rand <- mapply(
                                                  function(x) stochastize_MC(x),
                                                  nyiso.plants.slim$nyiso.plants.plprmfl
                                                  )

nyiso.dispatch <- arrange(nyiso.plants.slim, nyiso.plants.slim$nyiso.plants.MC_rand)
nyiso.dispatch$cumul_capacity <- cumsum(nyiso.dispatch$nyiso.plants.namepcap)
nyiso.dispatch$cumul_plc2erta <- 0

# Dispatch curve plotting
{
nyiso.dispatch$color <- "black"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "WAT"] <- "blue"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "SUN"] <- "orange"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "WND"] <- "cadetblue1"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "NG"] <- "darkgreen"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "NUC"] <- "magenta"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl == "LFG"] <- "gold3"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl %in% biomass] <- "green"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl %in% petroleum] <- "gray"
nyiso.dispatch$color[nyiso.dispatch$nyiso.plants.plprmfl %in% coal] <- "red"

plot(nyiso.dispatch$cumul_capacity, nyiso.dispatch$nyiso.plants.MC_rand, type = "l", lty = 3, xlim = c(0, 35000),
     main = "NYISO Dispatch Curve (2014)", xlab = "Capacity (MW)", ylab = "Marg. Cost ($ / kWh)")
axis(side = 2, at = seq(0,0.4,0.05))
abline(h=(seq(0,0.5,0.05)), col = "gray", lty = "dotted")
points(
        nyiso.dispatch$cumul_capacity, nyiso.dispatch$nyiso.plants.MC_rand,
        col = nyiso.dispatch$color, pch = 16, lwd = 40
      )
legend("topleft", c("Wind", "Hydro", "PV", "Coal","Nuclear", "Nat Gas", "LFG", "Biomass", "Petroleum-based"),
       col = c("cadetblue1", "blue", "orange", "red", "magenta", "darkgreen", "gold3", "green", "gray"), pch = 16)

plot(nyiso.dispatch$cumul_capacity, nyiso.dispatch$nyiso.plants.plc2erta,
     type = "p",col = nyiso.dispatch$color, ylim = c(0,10000),
     main = "NYISO EF Curve (2014)", xlab = "Capacity (MW)", ylab = "EF (lb CO2eq / MWh)")
abline(h=(seq(0,10000,1000)), col = "gray", lty = "dotted")
legend("topleft", c("Wind", "Hydro", "PV", "Coal","Nuclear", "Nat Gas", "LFG", "Biomass", "Petroleum-based"),
       col = c("cadetblue1", "blue", "orange", "red", "magenta", "darkgreen", "gold3", "green", "gray"), pch = 16)
}
