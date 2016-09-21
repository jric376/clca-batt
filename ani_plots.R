# Plots + animations

# All plots and animations based on custom objects
# are created here. Objects are imported as needed

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("animation")
library("cowplot")
library("dplyr")
library("futile.logger")
library("gganimate")
library("ggplot2")

### The following three lines are used for loading time-series scripts
src_list = list.files(pattern = "*load.R", full.names = TRUE)
source(src_list[1])
# "bldg_load", "grid_load", "pv_load"

### This is for loading the dispatch curve script
# source("dispatch_curve.R")
if (!dir.exists(file.path("outputs\\plots"))) {
  dir.create(file.path("outputs\\plots"))
}

test_bldg <- get_bldg()
# test_grid <- get_grid()
# test_pv <- get_pv()
# test_disp <- get_disp()

get_ani_bldg <- function(bldg) {
  full_df <- bldg$get_base_ts()
  full_df$run<- 1
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,bldg$get_ts_count())) {
    
    rand_kw <- as.data.frame(bldg$get_ts_df(i))
    rand_kw$run <- i
    rand_kw$week <- format(rand_kw$date_time, "%U")
    full_df <- rbind(full_df, rand_kw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt.bare <- ggplot(data = jul_df, aes(date_time, kw)) +
                    geom_point(alpha = 1/2) +
                    labs(
                          x = "",
                          y = "kW",
                          title = "Office 5min Elecricity Profile - Trial"
                          ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 16),
                          axis.text.x = element_text(size = 14),
                          axis.text.y = element_text(size = 14))
  
  jul_plt <- jul_plt.bare + geom_line(
                                      data = subset(jul_df, jul_df$run == 1),
                                      colour = "#e31a1c", size = 1)
  
  ggsave(filename = "outputs\\plots\\bldg_load.png",
         width = 10, height = 6.25, units = "in")
  
  ani_jul_plt <- jul_plt.bare + geom_line(
                                          data = subset(jul_df, jul_df$run == 1),
                                          colour = "gray65") +
                                geom_point(
                                            mapping = aes(frame = run),
                                            colour = "#e31a1c", size = 1.5)
  
  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(ani_jul_plt, "outputs\\plots\\bldg_load.gif")
}
get_ani_grid <- function(grid) {full_df <- grid$get_base_ts()
  full_df$run<- 1
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,grid$get_ts_count())) {
    
    rand_mw <- as.data.frame(grid$get_ts_df(i))
    rand_mw$run <- i
    rand_mw$week <- format(rand_mw$date_time, "%U")
    full_df <- rbind(full_df, rand_mw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt.bare <- ggplot(data = jul_df, aes(date_time, mw)) +
                    geom_point(alpha = 1/2) +
                    labs(
                          x = "",
                          y = "MW",
                          title = "NYISO 5min Load Profile - Trial"
                    ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 16),
                          axis.text.x = element_text(size = 14),
                          axis.text.y = element_text(size = 14))
  
  jul_plt <- jul_plt.bare + geom_line(
                                      data = subset(jul_df, jul_df$run == 1),
                                      colour = "#e31a1c", size = 1)
  
  ggsave(filename = "outputs\\plots\\grid_load.png",
         width = 10, height = 6.25, units = "in")
  
  ani_jul_plt <- jul_plt.bare + geom_line(
                                          data = subset(jul_df, jul_df$run == 1),
                                          colour = "gray65") +
                                geom_point(
                                          mapping = aes(frame = run),
                                          colour = "#e31a1c", size = 1.5)
  
  
  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(ani_jul_plt, "outputs\\plots\\grid_load.gif")}
get_ani_pv <- function(pv) {
  full_df <- bldg$get_base_ts()
  full_df$run<- 1
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,bldg$get_ts_count())) {
    # kwh <- as.vector(bldg$get_ts_df(i))$kwh
    rand_kw <- as.data.frame(bldg$get_ts_df(i))
    rand_kw$run <- i
    rand_kw$week <- format(rand_kw$date_time, "%U")
    full_df <- rbind(full_df, rand_kw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt.bare <- ggplot(
                          data = jul_df,
                          aes(date_time, kw)) +
                          geom_point(alpha = 1/2) +
                          labs(
                                x = "",
                                y = "kW",
                                title = "PV 5min Generation Profile - Trial"
                          ) +
                          theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                          theme(panel.grid.major = element_line(colour = "gray85")) +
                          theme(panel.grid.minor = element_line(colour = "gray85")) +
                          theme(text = element_text(size = 16),
                                axis.text.x = element_text(size = 14),
                                axis.text.y = element_text(size = 14))
                        
  jul_plt <- jul_plt.bare + geom_line(
                                      data = subset(jul_df, jul_df$run == 1),
                                      colour = "e31a1c", size = 1)
  
  ggsave(filename = "outputs\\plots\\pv_load.png",
         width = 10, height = 6.25, units = "in")
  
  ani_jul_plt <- jul_plt.bare + geom_line(
                                          data = subset(jul_df, jul_df$run == 1),
                                          colour = "gray65") +
                                geom_point(
                                          mapping = aes(frame = run),
                                          colour = "#e31a1c", size = 1.5)
  
  
  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(ani_jul_plt, "outputs\\plots\\pv_load.gif")
}
get_ani_disp <- function(disp, runs = 20) {
  full_df <- disp$get_dispatch()
  full_df$run<- 1
  
  for (i in seq.int(2,runs)) {
    
    disp$stochastize_costs()
    rand_disp <- disp$get_dispatch()
    rand_disp$run <- i
    full_df <- rbind(full_df, rand_disp)
  }
  
  disp_plt.bare <- ggplot(data = full_df,
                          mapping = aes(x = cumul_cap)) +
                          labs(x = "Cumulative Capacity (MW)") +
                          theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                          theme(text = element_text(size = 16),
                                axis.text.x = element_text(size = 14),
                                axis.text.y = element_text(size = 14)) +
                          theme(legend.position = c(0.30, 0.70), legend.box = "horizontal",
                                legend.background = element_rect(fill = "white", colour = "gray75")) +
                          background_grid(major = "xy", minor = "none",
                                          size.major = 0.5, colour.major = "gray85")


  disp_cost <- disp_plt.bare + labs(
                                    y = "Marg Cost ($ / kWh)",
                                    title = "NYISO Dispatch Curve") +
                                    expand_limits(y = c(0, 0.32)) +
                                geom_errorbar(
                                              data = subset(full_df, full_df$run == 1),
                                              mapping = aes(
                                                  y = MC_rand, size = namepcap,
                                                  ymax = MC_rand + se, ymin = MC_rand - se),
                                              width = 50, colour = "black", size = 1) +
                                geom_point(
                                            data = subset(full_df, full_df$run == 1),
                                            mapping = aes(
                                                y = MC_rand, size = namepcap,
                                                fill = fuel_type),
                                            alpha = 1/1.2,
                                            colour = "gray35", shape = 21) +
                                scale_size(
                                            name = "Capacity (MW)",
                                            breaks = c(250,500,1000,2000),
                                            range = c(3,15)) +
                                scale_fill_brewer(
                                                  name = "Fuel", type = "div", palette = "Set1",
                                                  guide = guide_legend(override.aes = list(alpha = 1, size = 5)))
  
  disp_emish <- disp_plt.bare + labs(
                                      y = "Emissions Factor (lb CO2eq / MWh)",
                                      title = "NYISO Dispatch EF Curve") +
                                expand_limits(y = c(0, 1500)) +
                                geom_point(
                                            data = subset(full_df, full_df$run == 1),
                                            mapping = aes(
                                                          y = cumul_plc2erta, size = wtd_plc2erta,
                                                          fill = fuel_type),
                                            alpha = 1/1.2,
                                            colour = "gray35", shape = 21) +
                                scale_size(
                                            name = "lb CO2eq / h",
                                            breaks = c(0,5E5,2E6,8E6),
                                            range = c(3,15)) +
                                scale_fill_brewer(guide = "none", palette = "Set1")
                                                  # name = "Fuel", type = "div", palette = "Set1",      
                                                  # guide_legend(override.aes = list(alpha = 1, size = 5)))
                              
  disp_plt <- plot_grid(disp_cost, disp_emish,
                        labels = c("A","B"),
                        nrow = 2, align = "v")
  
  ### For saving combined plot
  # save_plot(filename = "outputs\\plots\\disp_nyiso_combined.png",
  #           disp_plt, ncol = 1, nrow = 2,
  #           base_height = 6.25, base_width = 10)
  
  ### For saving individual ggplot objects
  # ggsave(disp_cost, filename = "outputs\\plots\\disp_nyiso.png",
  #        width = 10, height = 6.25, units = "in")
  # ggsave(disp_emish, filename = "outputs\\plots\\disp_nyiso_emish.png",
  #        width = 10, height = 6.25, units = "in")

  ani_disp_cost <- disp_plt.bare + labs(
                                        y = "Marg Cost ($ / kWh)",
                                        title = "NYISO Dispatch Curve") +
                                        expand_limits(y = c(0, 0.32)) +
                                    geom_point(
                                              mapping = aes(y = MC_rand, size = namepcap),
                                              alpha = 1/2, color = "gray35") +
                                    geom_line(
                                              mapping = aes(
                                                            y = MC_rand, size = namepcap,
                                                            frame = run),
                                              size = 1, colour = "gray65") +
                                    geom_point(
                                                mapping = aes(
                                                    y = MC_rand, size = namepcap,
                                                    fill = fuel_type, frame = run),
                                                colour = "black", shape = 21) +
                                    scale_size(
                                                name = "Capacity (MW)",
                                                breaks = c(250,500,1000,2000),
                                                range = c(3,15)) +
                                    scale_fill_brewer(
                                      name = "Fuel", type = "div", palette = "Set1",
                                      guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                                    theme(
                                      legend.position = c(0.30, 0.70), legend.box = "horizontal",
                                      legend.background = element_rect(colour = "gray75"))
  
  ani_disp_emish <- disp_plt.bare + labs(
                                          y = "Emissions Factor (lb CO2eq / MWh)",
                                          title = "NYISO Dispatch EF Curve") +
                                geom_point(
                                                mapping = aes(y = cumul_plc2erta, size = wtd_plc2erta),
                                                alpha = 1/2, color = "gray35") +
                                geom_line(
                                          mapping = aes(
                                                        y = cumul_plc2erta, size = wtd_plc2erta,
                                                        frame = run),
                                          size = 1, colour = "gray65") +
                                geom_point(
                                          aes(
                                              y = cumul_plc2erta, size = wtd_plc2erta,
                                              fill = fuel_type, frame = run),
                                          colour = "black", shape = 21) +
                                scale_size(
                                            name = "lb CO2eq / h",
                                            breaks = c(0,5E5,2E6,8E6),
                                            range = c(3,15)) +
                                scale_fill_brewer(
                                          name = "Fuel", type = "div", palette = "Set1",
                                          guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                                theme(
                                      legend.position = c(0.30, 0.70), legend.box = "horizontal",
                                      legend.background = element_rect(colour = "gray75"))


  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  # gg_animate(ani_disp_cost, "outputs\\plots\\disp_nyiso_cost.gif")
  # gg_animate(ani_disp_emish, "outputs\\plots\\disp_nyiso_emish.gif")
  
  return(full_df)
}
get_bldg_comp <- function() {
  bldg_df <- read.csv("inputs\\bldg_summ.csv",
                      stringsAsFactors = FALSE)
  bldg_df$ci <- bldg_df$kwh*0.05
  bldg_lvls <- c("Heating", "Cooling",
                 "Int. Lighting", "Ext. Lighting",
                 "Int. Equipment", "Fans")
  bldg_df$use_cat <- with(bldg_df,
                          factor(use_cat,
                                 levels = bldg_lvls)
  )
  
  summ_plt <- ggplot(data = bldg_df,
                     mapping = aes(x = use_cat, y = kwh,
                                   colour = src)) +
    scale_colour_manual(name = "Source",
                        values = c("#bf812d",
                                   "#01665e")) +
    geom_errorbar(data = subset(bldg_df,
                                src == "DOE" &
                                  bldg_type == "Office"),
                  aes(ymax = kwh + ci, ymin = kwh - ci),
                  colour = "black", width = 0.3) +
    geom_point(data = subset(bldg_df,
                             src == "DOE" &
                               bldg_type == "Office"),
               size = 5)  +
    geom_point(data = subset(bldg_df,
                             src == "model" &
                               bldg_type == "Office"),
               size = 2) +
    # facet_wrap(~bldg_type, switch = "x") +
    labs(x = "",
         y = "kWh",
         title = "Office Annual Electricity Usage Validation") +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    theme(legend.box = "horizontal",
          legend.background = element_rect(colour = "gray75")) +
    expand_limits(y = c(0,310000))
  
  ggsave(filename = "outputs\\plots\\office_compare.png",
         width = 10, height = 6.25, units = "in")
}

bldg_gif <- get_ani_bldg(test_bldg)
# grid_gif <- get_ani_grid(test_grid)
# pv_gif <- get_ani_pv(test_pv)
# disp_gif <- get_ani_disp(test_disp)