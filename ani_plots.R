# Plots + animations

# All plots and animations based on custom objects
# are created here. Objects are imported as needed

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
library("animation")
library("cowplot")
library("data.table")
library("plyr")
library("dplyr")
library("futile.logger")
library("gganimate")
library("ggplot2")
library("ggrepel")
library("reshape2")
library("scales")
library("tidyr")

# if (!dir.exists(file.path("outputs\\plots"))) {
#   dir.create(file.path("outputs\\plots"))
# }
cbb_qual <- c("#E69F00", "#999999","#CC79A7", "#009E73", "#F0E442",
              "#000000", "#0072B2", "#D55E00", "#56B4E9")

get_bldg_ani <- function(copies) {
  source("bldg_load.R")
  bldg <- get_bldg(run_id = "plot", copies = copies, type = "office")
  full_df <- bldg$get_base_ts()
  full_df$run <- 1
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
get_grid_ani <- function(copies) {
  source("grid_load.R")
  get_grid(run_id = "plot", copies = copies, terr = "nyiso")
  full_df <- grid$get_base_ts()
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
get_pv_ani <- function(copies) {
  source("pv_load.R")
  pv <- get_pv(run_id = "plot", copies = copies, type = "office")
  full_df <- pv$get_base_ts()
  full_df$run<- 1
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,pv$get_ts_count())) {
    # kwh <- as.vector(pv$get_ts_df(i))$kwh
    rand_kw <- as.data.frame(pv$get_ts_df(i))
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
get_disp_ani <- function(runs = 20, save = FALSE) {
  source("dispatch_curve.R")
  
  disp_meta = list(
    "name" = "Doris the Dispatch",
    "run_id" = "plot",
    "ctrl_id" = "plot"
  )
  disp <- disp_curv$new(meta = disp_meta,
                        terr = "nyiso")
  full_df <- disp$get_dispatch()
  full_df$run <- 1
  
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
                          theme(legend.box = "horizontal",
                                legend.background = element_rect(colour = "gray75", fill = alpha("gray85", 1/4))) +
                          background_grid(major = "xy", minor = "none",
                                          size.major = 0.5, colour.major = "gray85")


  disp_cost <- disp_plt.bare + labs(y = "Marg. Cost ($ / kWh)") +
                                expand_limits(y = c(0, 0.32)) +
                                geom_errorbar(data = subset(full_df, full_df$run == 1),
                                              mapping = aes(
                                                  y = MC_rand, size = namepcap,
                                                  ymax = MC_rand + se, ymin = MC_rand - se),
                                              width = 50, colour = "black", size = 1) +
                                geom_point(data = subset(full_df, full_df$run == 1),
                                            mapping = aes(
                                                y = MC_rand, size = namepcap,
                                                fill = fuel_type),
                                            alpha = 1/1.2,
                                            colour = "gray35", shape = 21) +
                                scale_size(name = bquote(scriptstyle(MW[plant])),
                                            breaks = c(250,500,1000,2000),
                                            range = c(3,15)) +
                                scale_fill_manual(name = NULL, values = cbb_qual,
                                                  guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                                theme(legend.position = c(0.25, 0.70))
  
  disp_emish <- disp_plt.bare + labs(y = bquote("lb"~ CO[scriptstyle(2)]~ "eq / MWh")) +
                                expand_limits(y = c(0, 1500)) +
                                geom_point(data = subset(full_df, full_df$run == 1),
                                            mapping = aes(
                                                          y = cumul_plc2erta, size = wtd_plc2erta,
                                                          fill = fuel_type),
                                            alpha = 1/1.2,
                                            colour = "gray35", shape = 21) +
                                scale_size(name = bquote(scriptstyle("lb"~ CO[scriptscriptstyle(2)]~ "eq / h")),
                                            breaks = c(0,5E5,2E6,8E6),
                                            range = c(1,15)) +
                                scale_fill_manual(guide = "none", values = cbb_qual) +
                                theme(legend.position = c(0.333, 0.70))
                              
  disp_plt <- plot_grid(disp_cost, disp_emish,
                        labels = c("A","B"),
                        nrow = 2, align = "v")
  
  title <- ggdraw() + draw_label("NYISO Dispatch Curve and Cumulative Emissions Factor",
                                 fontface = "bold")
  disp_plt <- plot_grid(title, disp_plt,
                        ncol = 1, rel_heights = c(0.075,1))
  
  ### For saving combined plot
  if (save) {
    save_plot(filename = "outputs\\plots\\disp_nyiso_combined.png",
              disp_plt, ncol = 1, nrow = 2,
              base_height = 6.25, base_width = 10)
  }
  ### For saving individual ggplot objects
  if (save) {
    ggsave(disp_cost, filename = "outputs\\plots\\disp_nyiso.png",
           width = 10, height = 6.25, units = "in")
    # ggsave(disp_emish, filename = "outputs\\plots\\disp_nyiso_emish.png",
    #        width = 10, height = 6.25, units = "in")
  }
  ani_disp_cost <- disp_plt.bare + labs(y = "Marg. Cost ($ / kWh)",
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
                                    geom_point(mapping = aes(
                                                    y = MC_rand, size = namepcap,
                                                    fill = fuel_type, frame = run),
                                                colour = "black", shape = 21) +
                                    scale_size(name = bquote(scriptstyle(MW[plant])),
                                                breaks = c(250,500,1000,2000),
                                                range = c(3,15)) +
                                    scale_fill_manual(name = NULL, values = cbb_qual,
                                                      guide = guide_legend(override.aes = list(alpha = 1,
                                                                                               size = 5))) +
                                    theme(legend.position = c(0.25, 0.70), legend.box = "horizontal",
                                          legend.background = element_rect(colour = "gray75",
                                                                           fill = alpha("gray85", 1/2)))
  
  ani_disp_emish <- disp_plt.bare + labs(y = bquote("lb"~ CO[scriptstyle(2)]~ "eq / MWh"),
                                         title = "NYISO Cumulative Emissions Factor") +
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
                                scale_size(name = bquote("lb"~ CO[scriptstyle(2)]~ "eq / h"),
                                            breaks = c(0,5E5,2E6,8E6),
                                            range = c(3,15)) +
                                scale_fill_manual(name = NULL, values = cbb_qual,
                                                  guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                                theme(legend.position = c(0.26, 0.70), legend.box = "horizontal",
                                      legend.background = element_rect(colour = "gray75",
                                                                       fill = alpha("gray85", 1/4)))

  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  if (save) {
    gg_animate(ani_disp_cost, "outputs\\plots\\disp_nyiso_cost.gif")
    gg_animate(ani_disp_emish, "outputs\\plots\\disp_nyiso_emish.gif")
  }
  
  return(full_df)
}
get_disp_grid_stats <- function() {
  return(0)
}
get_disp_w_donut <- function(runs = 20, save = FALSE) {
  source("dispatch_curve.R")
  source("grid_load.R")
  
  grid <- get_grid(run_id = "plot", copies = floor((runs-1)/2), terr = "nyiso")
  
  full_grid <- grid$get_base_ts()
  full_grid$week <- format(full_grid$date_time, "%U")
  
  for (i in seq.int(2,grid$get_ts_count())) {
    
    rand_mw <- as.data.frame(grid$get_ts_df(i))
    rand_mw$week <- format(rand_mw$date_time, "%U")
    full_grid <- rbind(full_grid, rand_mw)
  }
  rm(rand_mw)
  full_grid <- summarise_if(group_by(full_grid, date_time),
                            is.numeric, .funs = c("mean", "sd"))
  disp_meta = list(
    "name" = "Doris the Dispatch",
    "run_id" = "plot",
    "ctrl_id" = "plot"
  )
  disp <- disp_curv$new(meta = disp_meta,
                        terr = "nyiso")
  full_disp <- disp$get_dispatch()
  
  for (j in seq.int(2,runs)) {
    
    disp$stochastize_costs()
    rand_disp <- disp$get_dispatch()
    full_disp <- rbind(full_disp, rand_disp)
  }
  rm(rand_disp)
  full_disp <- select(full_disp, namepcap, cumul_cap, fuel_type) %>%
                mutate(bin = cumul_cap%/%(max(full_disp$cumul_cap)/3),
                       fuel_type = factor(fuel_type))
  
  hist_df <- group_by(full_disp, bin, fuel_type) %>%
                select(-cumul_cap) %>%
                summarise_at(contains("namepcap"), sum) %>%
                complete(bin, fuel_type, fill = list (namepcap = 0)) %>%
                group_by(bin, fuel_type) %>%
                summarise_at(contains("namepcap"), sum) %>%
                arrange(fuel_type) %>%
                mutate(c_mw = ifelse(fuel_type == lag(fuel_type),
                                     ifelse(namepcap == 0, 0, 1), 0))
                # NEED TO SHOW FRACTION OF EACH FUEL TYPE AT EACH CUMUL_CAP
                # group_by(bin) %>%
                # mutate(frac = namepcap / sum(namepcap)) %>%
                # group_by(bin) %>%
                # mutate(frac_bin = frac / sum(frac))
  
  # for (k in )
    
  
  disp_plt.bare <- ggplot(data = full_disp,
                          mapping = aes(x = cumul_cap)) +
                      labs(x = "Cumulative Capacity (MW)") +
                      theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                      theme(text = element_text(size = 16),
                            axis.text.x = element_text(size = 14),
                            axis.text.y = element_text(size = 14)) +
                      theme(legend.box = "horizontal",
                            legend.background = element_rect(colour = "gray75", fill = alpha("gray85", 1/4))) +
                      background_grid(major = "xy", minor = "none",
                                      size.major = 0.5, colour.major = "gray85")
  
  
  disp_cost <- disp_plt.bare + labs(y = "Marg. Cost ($ / kWh)") +
                  expand_limits(y = c(0, 0.32)) +
                  geom_errorbar(data = subset(full_disp, full_disp$run == 1),
                                mapping = aes(
                                  y = MC_rand, size = namepcap,
                                  ymax = MC_rand + se, ymin = MC_rand - se),
                                width = 50, colour = "black", size = 1) +
                  geom_point(data = subset(full_disp, full_disp$run == 1),
                             mapping = aes(
                               y = MC_rand, size = namepcap,
                               fill = fuel_type),
                             alpha = 1/1.2,
                             colour = "gray35", shape = 21) +
                  scale_size(name = bquote(scriptstyle(MW[plant])),
                             breaks = c(250,500,1000,2000),
                             range = c(3,15)) +
                  scale_fill_manual(name = NULL, values = cbb_qual,
                                    guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                  theme(legend.position = c(0.25, 0.70))
}
get_isoterr_plots <- function(terr = "nyiso", save = FALSE) {
  source("dispatch_curve.R")
  # CURRENTLY DROPS TOP AND BOTTOM 1% of EMISSIONS RATES
  disp_meta = list(
    "name" = "Doris the Dispatch",
    "run_id" = "plot",
    "ctrl_id" = "plot"
  )
  disp <- disp_curv$new(meta = disp_meta,
                        terr = "nyiso")
  full_df <- disp$get_dispatch()
  disp_plot.cap <- ggplot(data = full_df,
                          mapping = aes(x = factor(fuel_type),
                                        fill = factor(fuel_type))) +
                      geom_boxplot(aes(y = namepcap),
                                   varwidth = TRUE,
                                   outlier.shape = NA) +
                      geom_jitter(aes(y = namepcap, fill = factor(fuel_type)),
                                 shape = 21, alpha = 1/2,
                                 position = position_jitter(w = 0.2, h = 0.2)) +
                      # coord_cartesian(ylim = quantile(full_df$namepcap,
                      #                                 c(0.0, 0.99))) +
                      scale_y_log10() +
                      scale_fill_manual(name = "Fuel", values = cbb_qual) +
                      labs(x = "",
                           y = "MW") +
                      theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                      theme(panel.grid.major = element_line(colour = "gray85")) +
                      theme(panel.grid.minor = element_line(colour = "gray85")) +
                      theme(legend.position = "none")
  
  emitting_plants <- filter(full_df, plc2erta > 0)
  disp_plot.plc2erta <- ggplot(data = emitting_plants,
                                mapping = aes(x = factor(fuel_type),
                                              fill = factor(fuel_type))) +
                          geom_boxplot(aes(y = plc2erta),
                                       varwidth = TRUE,
                                       outlier.shape = NA) +
                          geom_jitter(aes(y = plc2erta, fill = factor(fuel_type)),
                                     shape = 21, alpha = 1/2,
                                     position = position_jitter(w = 0.2, h = 0.2)) +
                          coord_cartesian(ylim = quantile(emitting_plants$plc2erta,
                                                          c(0.01, 0.99))) +
                          scale_x_discrete(drop = FALSE) +
                          scale_y_log10() +
                          scale_fill_manual(name = "Fuel",
                                            values = c(cbb_qual[1],cbb_qual[2],
                                                       cbb_qual[5],cbb_qual[7])) +
                          labs(x = "",
                               y = bquote("lb"~ CO[scriptstyle(2)]~ "eq / MWh")) +
                          theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                          theme(panel.grid.major = element_line(colour = "gray85")) +
                          theme(panel.grid.minor = element_line(colour = "gray85")) +
                          theme(legend.position = "none")
  disp_plot.plc2erta <- plot_grid(ggdraw(),disp_plot.plc2erta,ggdraw(),
                                  nrow = 1, align = "h",
                                  rel_widths = c(0.12,1,0.28))
  
  isoterr_boxplot <- plot_grid(disp_plot.cap, disp_plot.plc2erta,
                            labels = c("A","B"),
                            nrow = 2, align = "v",
                            rel_heights = c(1, 0.75))
  title <- ggdraw() + draw_label("NYISO Plant-level Capacity and Emissions Factors",
                                 fontface = "bold")
  isoterr_boxplot <- plot_grid(title, isoterr_boxplot,
                                ncol = 1, rel_heights = c(0.075, 1))
  
  if (save) {
      # ggsave(paste0("outputs\\plots\\", terr, "_namepcap.png"),
      #        disp_plot.cap,
      #        width = 10, height = 6.25, units = "in")
      # ggsave(paste0("outputs\\plots\\", terr, "_plc2erta.png"),
      #        disp_plot.plc2erta,
      #        width = 10, height = 6.25, units = "in")
      save_plot(filename = paste0("outputs\\plots\\", terr, "_cap_co2eq.png"),
                isoterr_boxplot, ncol = 1, nrow = 2,
                base_height = 6.25, base_width = 10)
    }
  
  return(isoterr_boxplot)
}
get_bldg_comp <- function() {
  bldg_df <- read.csv("inputs\\bldg_summ.csv",
                      stringsAsFactors = FALSE)
  bldg_df$ci <- bldg_df$kwh*0.05
  stop("Need to account for different cats that show in hospital validation")
  bldg_lvls <- c("Heating", "Cooling",
                 "Int. Lighting", "Ext. Lighting",
                 "Int. Equipment", "Fans")
  bldg_df$use_cat <- with(bldg_df,
                          factor(use_cat,
                                 levels = bldg_lvls)
  )
  
  summ_plot <- ggplot(data = bldg_df,
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
         summ_plot,
         width = 10, height = 6.25, units = "in")
}
get_bldg_summ <- function(type, copies) {
  source("bldg_load.R")
  rowSds <- function(x) {
    sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
  }
  suffixes <- letters[1:(copies+1)]
  bldg <- get_bldg(run_id = "plot", type = type, copies = copies)
  full_df <- bldg$get_ts_df()
  for (i in 1:(copies+1)) {
    dots <- names(full_df[[i]])
    col.names <- c("date_time",
                   paste0("kw_", suffixes[i]),
                   paste0("kwh_", suffixes[i]))
    full_df[[i]] <- mutate_(full_df[[i]],
                            .dots = setNames(dots, col.names)) %>%
      select(-kw,-kwh)
  }
  full_df <- Reduce(function(x, y) inner_join(x, y, by = "date_time"), full_df) %>%
    na.omit()
  
  summ_df <- mutate(full_df, kw_mean = rowMeans(select(full_df, contains("kw_"))),
                    kw_sd = rowSds(select(full_df, contains("kw_"))),
                    kwh_mean = rowMeans(select(full_df, contains("kwh_"))),
                    kwh_sd = rowSds(select(full_df, contains("kwh_"))),
                    hr = as.numeric(strftime(date_time, format = "%H")),
                    day_ind = as.numeric(strftime(date_time, format = "%j")),
                    dayhr_ind = day_ind + as.numeric(hr/24))
  
  return(summ_df)
}
get_bldg_ldc <- function(type, copies, save = FALSE) {
  
  summ_df <- get_bldg_summ(type, copies)
  
  ldc_df <- summ_df %>%
              select(dayhr_ind, kw_mean, kw_sd) %>%
              group_by(dayhr_ind) %>%
              summarise_if(is.numeric, mean)
  ldc_hist = hist(ldc_df$kw_mean, breaks = floor(max(ldc_df$kw_mean)), plot = FALSE)
  ldc_df = data.frame(c(0, ldc_hist$counts),
                       ldc_hist$breaks)
  colnames(ldc_df) = c("hrs", "kw")
  ldc_df$cumul_hrs = rev(cumsum(ldc_df$hrs))
  ldc_df$rel_kw = ldc_df$kw / max(ldc_df$kw)
  ldc_df$hrs_diff = c(0, diff(ldc_df$hrs)) / 8760

  ldc_plot <- ggplot(data = ldc_df,
                     mapping = aes(x = cumul_hrs)) +
                geom_line(aes(y = rel_kw), size = 1.1) +
                labs(x = "Hours of Load",
                     y = bquote("kW /" ~kW[max]),
                     title = "Office Load Duration Curve") +
                theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                theme(panel.grid.major = element_line(colour = "gray85")) +
                theme(panel.grid.minor = element_line(colour = "gray85"))
  
  if (save) {
    ggsave(filename = paste0("outputs\\plots\\", type, "_ldc.png"),
           ldc_plot,
           width = 10, height = 6.25, units = "in")
  }
  
  return(ldc_plot)
}
get_bldg_heatmap <- function(type, copies, save = FALSE) {
  
  summ_df <- get_bldg_summ(type, copies)
  
  heatmap_df <- summ_df %>%
                  select(date_time, hr, dayhr_ind, contains("_mean"), contains("_sd")) %>%
                  group_by(dayhr_ind) %>%
                  summarise_if(is.numeric, mean) %>%
                  mutate(day = (floor(dayhr_ind)+4)%%7) %>% # offsetting days for graph
                  select(-dayhr_ind) %>%
                  group_by(day, hr) %>%
                  summarise_all(mean) %>%
                  select(day, hr, contains("kw_"))
  heatmap_df.m <- melt(heatmap_df,
                        id.vars = c("day","hr"),
                        measure.vars = c("kw_mean","kw_sd"))
  heatmap_df.s <- ddply(heatmap_df.m, .(variable), transform, rescale = scale(value))
  heatmap_mean <- filter(heatmap_df.s, variable == "kw_mean")
  heatmap_sd <- filter(heatmap_df.s, variable == "kw_sd")
  
  day_labels <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
  hr_labels <- unlist(lapply(seq(3,21,3), function(x) ifelse(x>10, paste0(x, ":00"),
                                                             paste0("0", x, ":00"))))
  
  mean_plot <- ggplot(heatmap_mean, aes(y = day, x = hr)) + 
                  geom_tile(aes(fill = value), colour = "gray80") +
                  scale_x_continuous(breaks = seq(2,20,3),
                                     labels = hr_labels,
                                     expand=c(0,0)) +
                  scale_y_continuous(breaks = seq(0,6,1),
                                     labels = day_labels,
                                     expand=c(0,0)) +
                  scale_fill_gradient2(name = bquote(bar(kW)), low = "#7b3294",
                                       mid = "#f7f7f7", high = "#008837",
                                       midpoint = 10) +
                  labs(x = "",
                       y = "") +
                  theme(panel.background = element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.y = element_text(angle = 33, hjust = 1),
                        axis.text.x = element_text(angle = 33, vjust = 1, hjust = 1),
                        axis.title.x = element_blank())
  
  sd_plot <- ggplot(heatmap_sd, aes(y = day, x = hr)) + 
                geom_tile(aes(fill = value), colour = "gray80") +
                scale_x_continuous(breaks = seq(2,20,3),
                                   labels = hr_labels,
                                   expand=c(0,0)) +
                scale_y_continuous(breaks = seq(0,6,1),
                                   labels = day_labels,
                                   expand=c(0,0)) +
                scale_fill_gradient2(name = bquote(sigma[scriptscriptstyle(kW)]),
                                     low = "#7b3294", mid = "#f7f7f7", high = "#008837",
                                     midpoint = 0.75) +
                labs(x = "",
                     y = "") +
                theme(panel.background = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_text(angle = 33, vjust = 1, hjust = 1))
  
  heatmap_plot <- plot_grid(mean_plot, sd_plot,
                            labels = c("A","B"),
                            nrow = 2, align = "v",
                            rel_heights = c(1, 0.333))
  title <- ggdraw() + draw_label("Office Weekly Load Profile",
                                 fontface = "bold")
  heatmap_plot <- plot_grid(title, heatmap_plot,
                            ncol = 1, rel_heights = c(0.05, 1))
  
  if (save) {
    # ggsave(paste0("outputs\\plots\\", type, "_heatmap_mean.png"),
    #        mean_plot,
    #        width = 10, height = 6.25, units = "in")
    # ggsave(paste0("outputs\\plots\\", type, "_heatmap_sd.png"),
    #        sd_plot,
    #        width = 10, height = 6.25, units = "in")
    save_plot(filename = paste0("outputs\\plots\\", type, "_heatmap.png"),
              heatmap_plot, ncol = 1, nrow = 2,
              base_height = 6.25, base_width = 10)
  }

  return(heatmap_plot)
}
get_kt_dist <- function(which_df, save = FALSE) {
  if(which_df == "nyc_nsrdb") {
    solar_df = read.csv("inputs\\solar_nsrdb_2014.csv") %>%
      mutate(df = "NYC") %>%
      select(-X)
    cbb_qual = cbb_qual[2:3]
    label_scale = 2
    y_max = 0.75
  }
  else {
    cove = read.csv("inputs\\solar_bsrn_cove.csv") %>%
      mutate(date_time = strftime(as.POSIXct(date_time), format = "%Y-%m-%d"),
             df = "COVE")
    larc = read.csv("inputs\\solar_bsrn_larc.csv") %>%
      mutate(date_time = strftime(as.POSIXct(date_time), format = "%Y-%m-%d"),
             df = "LARC")
    solar_df = bind_rows(cove, larc) %>%
      mutate(date_time = as.factor(date_time)) %>%
      group_by(dayhr_ind, date_time, weather, df) %>%
      summarise(kt.bar = mean(kt.bar),
                kt.til = mean(kt.til))
    cbb_qual = cbb_qual[2:3]
    label_scale = 5
    y_max = 0.3
  }
  sample_days = sample_n(group_by(solar_df, weather), 2)
  kt_plot = ggplot(solar_df, aes(x = kt.bar, y = kt.til)) +
    expand_limits(x = c(0, 1.3),
                  y = c(0,y_max)) + 
    geom_point(aes(fill = weather, alpha = 1/3, shape = df),
               size = 3) +
    geom_label_repel(data = sample_days,
                    aes(label = strftime(date_time, format = "%Y-%m-%d"),
                        alpha = 1/3),
                    colour = "black", fontface = "bold", size = 4,
                    box.padding = unit(0.8, "lines"),
                    point.padding = unit(1, "lines"), segment.size = 1.25,
                    arrow = arrow(length = unit(0.01, "npc")),
                    force = 5,
                    max.iter = 30,
                    nudge_y = 0.4*sample_days$kt.til*label_scale,
                    nudge_x = ifelse(sample_days$kt.bar > 0.65,
                                     -0.1*sample_days$kt.bar,
                                     -0.2*sample_days$kt.bar)) +
    scale_fill_manual(name = NULL, values = cbb_qual) +
    scale_shape_manual(name = NULL, values = c(21,23,19)) +
    guides(alpha = "none", size = "none",
           fill = guide_legend(override.aes = list(shape = 19,
                                                   colour = cbb_qual,
                                                   size = 4))) +
    labs(
      x = bquote("Daily mean of hourly avg" ~k[t]),
      y = bquote("Daily variation of hourly avg" ~k[t])
    ) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    theme(text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
    theme(legend.position = c(0.2, 0.8), legend.box = "vertical",
          legend.background = element_rect(fill = "white", colour = "gray75"))
  
  if(save) {
    ggsave(filename = paste0("outputs\\plots\\kt_dist_", which_df, ".png"),
           kt_plot, 
           width = 10, height = 6.25, units = "in")
  }
  
  return(kt_plot)
}
get_markov_sample <- function(sample, save = FALSE) {
  
  sample_colors = c("COVE 1min" = cbb_qual[9],
                    "LARC 1min" = cbb_qual[8],
                    "NSRDB 1hr" = cbb_qual[6])
  mC_plt.bare <- ggplot(data = sample,
                          mapping = aes(x = as.factor(date_time)), alpha = 1/2) +
                  scale_x_datetime(date_breaks = "1 day",
                               date_labels = "%m-%d") +
                  theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                  theme(text = element_text(size = 16),
                        axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 14)) +
                  theme(legend.box = "horizontal",
                        legend.background = element_rect(fill = "white", colour = "gray75")) +
                  background_grid(major = "xy", minor = "none",
                                size.major = 0.5, colour.major = "gray85")
  
  
  mC_plt.kt <- mC_plt.bare +
                  geom_line(aes(date_time, kt_1min.larc_1, colour = "LARC 1min")) +
                  geom_line(aes(date_time, kt_1min.cove_1, colour = "COVE 1min")) +
                  geom_line(aes(date_time, kt, colour = "NSRDB 1hr"),
                            alpha = 1/1.2) +
                  scale_colour_manual(name = NULL, values = sample_colors,
                                      guide = guide_legend(override.aes = list(size = 3))) +
                  labs(x = NULL,
                       y = bquote(k[t]))
  
  mC_plt.ghi <- mC_plt.bare +
                  geom_line(aes(date_time, ghi_1min.larc_1, colour = "LARC 1min")) +
                  geom_line(aes(date_time, ghi_1min.cove_1, colour = "COVE 1min")) +
                  geom_line(aes(date_time, ghi, colour = "NSRDB 1hr"),
                            alpha = 1/1.2) +
                  scale_colour_manual(name = NULL, values = sample_colors,
                                      guide = guide_legend(override.aes = list(size = 2))) +
                  labs(x = NULL,
                       y = bquote("GHI (W /" ~m^2~")"))
  
  grobs <- ggplotGrob(mC_plt.ghi)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
  
  mC_plt.combined <- plot_grid(mC_plt.kt + theme(legend.position= "none"),
                               NULL,
                               mC_plt.ghi + theme(legend.position = "none"), 
                               labels = c("A","","B"),
                               nrow = 3, 
                               rel_heights = c(1,0.3,1),
                               align = "v") + 
                        draw_grob(legend, 0, 1/2.3, 1, .3/2.3)
  
  ### For saving combined plot
  if (save) {
    save_plot(filename = "outputs\\plots\\markov_sample.png",
              mC_plt.combined, ncol = 1, nrow = 2,
              base_height = 6.25, base_width = 10)
  }
  return(mC_plt.combined)
}
get_markov_freqpoly <- function(mC_freq, save = FALSE) {
  
  markov_colors = c("cove model" = "#ca0020", "cove" = "#f4a582",
                    "larc" = "#92c5de", "larc model" = "#0571b0")
  mC_freq.plot <- ggplot(data = mC_freq) + 
                    geom_freqpoly(aes(cove_var, colour = "cove"), binwidth = 10,
                                  size = 1.5) + 
                    geom_freqpoly(aes(larc_var, colour = "larc"),binwidth = 10,
                                  size = 1.5) +
                    geom_freqpoly(aes(markov_var.cove, colour = "cove model"), binwidth = 10,
                                  size = 1.5) +
                    geom_freqpoly(aes(markov_var.larc, colour = "larc model"), binwidth = 10,
                                  size = 1.5) +
                    scale_x_continuous(limits = c(0,100)) +
                    scale_y_log10() +
                    scale_colour_manual(name = "Dataset", values = markov_colors) + 
                    labs(
                      x = bquote(e[var]),
                      y = "Counts",
                      title = "Comparing Normalized 1min Solar GHI Variance"
                    ) +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85")) +
                    theme(text = element_text(size = 16),
                          axis.text.x = element_text(size = 12),
                          axis.text.y = element_text(size = 12)) +
                    theme(legend.position = c(0.8, 0.8), legend.box = "vertical",
                          legend.background = element_rect(fill = "white", colour = "gray75"))
  
  if(save) {
    ggsave(filename = paste0("outputs\\plots\\mC_freq.png"),
           mC_freq.plot, 
           width = 10, height = 6.25, units = "in")
  }
  
  return(mC_freq.plot)
}