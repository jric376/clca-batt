# Plots + animations

# All plots and animations based on custom objects
# are created here. Objects are imported as needed

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("animation")
library("futile.logger")
library("gganimate")
library("ggplot2")
src_list = list.files(pattern = "*load.R", full.names = TRUE)
# "bldg_load", "grid_load", "pv_load"
source(src_list[2])

# test_bldg <- get_test_bldg()
# test_grid <- get_test_grid()
# test_pv <- get_test_pv()

get_ani_bldg <- function(bldg) {
  full_df <- bldg$get_base_ts()
  full_df$run<- as.character(1)
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,bldg$get_ts_count())) {
    
    rand_kw <- as.data.frame(bldg$get_ts_df(i))
    rand_kw$run <- as.character(i)
    rand_kw$week <- format(rand_kw$date_time, "%U")
    full_df <- rbind(full_df, rand_kw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt <- ggplot(data = jul_df, aes(date_time, kw)) +
                    geom_point() +
                    geom_line(data = subset(jul_df, jul_df$run == 1),
                                colour = "white", size = 1) +
                    labs(
                          x = "",
                          y = "kW",
                          title = "July - 5min Office Elecricity Profile - Trial"
                          ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 14))
  ggsave(filename = "outputs\\plots\\bldg_load.png",
         width = 10, height = 6.25, units = "in")
  
  jul_plt <- jul_plt + geom_point(mapping = aes(frame = run),
                                colour = "#00CC66", size = 1.5,
                                alpha = 1/1.5)
  

  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(jul_plt, "outputs\\plots\\bldg_load.gif")
}
get_ani_grid <- function(grid) {
  full_df <- grid$get_base_ts()
  full_df$run<- as.character(1)
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,grid$get_ts_count())) {
    
    rand_mw <- as.data.frame(grid$get_ts_df(i))
    rand_mw$run <- as.character(i)
    rand_mw$week <- format(rand_mw$date_time, "%U")
    full_df <- rbind(full_df, rand_mw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt <- ggplot(data = jul_df, aes(date_time, mw)) +
                    geom_point() +
                    geom_line(data = subset(jul_df, jul_df$run == 1),
                              colour = "white", size = 1) +
                    labs(
                          x = "",
                          y = "MW",
                          title = "July - 5min NYISO Load Profile - Trial"
                        ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 14))
  ggsave(filename = "outputs\\plots\\grid_load.png",
         width = 10, height = 6.25, units = "in")
  
  jul_plt <- jul_plt + geom_point(mapping = aes(frame = run),
                                  colour = "#00CC66", size = 1.5,
                                  alpha = 1/1.5)
  
  
  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(jul_plt, "outputs\\plots\\grids_load.gif")
}
get_ani_pv <- function(bldg) {
  full_df <- bldg$get_base_ts()
  full_df$run<- as.character(1)
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,bldg$get_ts_count())) {
    # kwh <- as.vector(bldg$get_ts_df(i))$kwh
    rand_kw <- as.data.frame(bldg$get_ts_df(i))
    rand_kw$run <- as.character(i)
    rand_kw$week <- format(rand_kw$date_time, "%U")
    full_df <- rbind(full_df, rand_kw)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt <- ggplot(data = jul_df, aes(date_time, kw)) +
                    geom_point() +
                    geom_line(data = subset(jul_df, jul_df$run == 1),
                              colour = "white", size = 1) +
                    labs(
                          x = "",
                          y = "kW",
                          title = "July - 5min PV Generation Profile - Trial"
                        ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 14))
  ggsave(filename = "outputs\\plots\\pv_load.png",
         width = 10, height = 6.25, units = "in")
  
  jul_plt <- jul_plt + geom_point(mapping = aes(frame = run),
                                  colour = "#00CC66", size = 1.5,
                                  alpha = 1/1.5)
  
  
  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  ani_jul_plt <- gg_animate(jul_plt, "outputs\\plots\\pv_load.gif")
}

# bldg_gif <- get_ani_bldg(test_bldg)
# grid_gif <- get_ani_grid(test_grid)
# pv_gif <- get_ani_pv(test_pv)

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
<<<<<<< HEAD
                      # facet_wrap(~bldg_type, switch = "x") +
=======
>>>>>>> 26cbe20243554f28405677be84cc6308158d02c5
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
get_bldg_comp()