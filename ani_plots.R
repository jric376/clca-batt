# Plots + animations

# All plots and animations based on custom objects
# are created here. Objects are imported as needed

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
library("animation")
library("cowplot")
library("data.table")
library("dplyr")
library("futile.logger")
library("gganimate")
library("ggplot2")
library("ggrepel")
library("tidyr")

# if (!dir.exists(file.path("outputs\\plots"))) {
#   dir.create(file.path("outputs\\plots"))
# }
cbb_qual <- c("#E69F00", "#999999","#CC79A7", "#009E73", "#F0E442",
              "#000000", "#0072B2", "#D55E00", "#56B4E9")

get_ani_bldg <- function(cop) {
  source("bldg_load.R")
  bldg <- get_bldg(run_id = "plot", copies = cop, type = "office")
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
get_ani_grid <- function(copies) {
  source("grid_load.R")
  get_grid(run_id = "plot", copies = cop, terr = "nyiso")
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
get_ani_pv <- function(cop) {
  source("pv_load.R")
  pv <- get_pv(run_id = "plot", copies = cop, type = "office")
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
get_ani_disp <- function(runs = 20) {
  source("dispatch_curve.R")
  disp <- get_disp("plot","plot","nyiso")
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
                                legend.background = element_rect(fill = "white", colour = "gray75"))# +
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
                                scale_fill_manual(
                                                  name = "Fuel", values = cbb_qual,
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
                                scale_fill_manual(guide = "none", values = cbb_qual)
                              
  disp_plt <- plot_grid(disp_cost, disp_emish,
                        labels = c("A","B"),
                        nrow = 2, align = "v")
  
  ### For saving combined plot
  save_plot(filename = "outputs\\plots\\disp_nyiso_combined.png",
            disp_plt, ncol = 1, nrow = 2,
            base_height = 6.25, base_width = 10)
  
  ### For saving individual ggplot objects
  ggsave(disp_cost, filename = "outputs\\plots\\disp_nyiso.png",
         width = 10, height = 6.25, units = "in")
  ggsave(disp_emish, filename = "outputs\\plots\\disp_nyiso_emish.png",
         width = 10, height = 6.25, units = "in")

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
                                    scale_fill_manual(
                                      name = "Fuel", values = cbb_qual,
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
                                scale_fill_manual(
                                          name = "Fuel", values = cbb_qual,
                                          guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                                theme(
                                      legend.position = c(0.30, 0.70), legend.box = "horizontal",
                                      legend.background = element_rect(colour = "gray75"))


  ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
  gg_animate(ani_disp_cost, "outputs\\plots\\disp_nyiso_cost.gif")
  gg_animate(ani_disp_emish, "outputs\\plots\\disp_nyiso_emish.gif")
  
  return(full_df)
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
  
  sample_colors = c("1min" = cbb_qual[9], "1hr" = cbb_qual[6])
  mC_plt.bare <- ggplot(data = sample,
                          mapping = aes(x = date_time)) +
                  theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                  theme(text = element_text(size = 16),
                        axis.text.x = element_text(size = 14),
                        axis.text.y = element_text(size = 14)) +
                  theme(legend.box = "horizontal",
                        legend.background = element_rect(fill = "white", colour = "gray75")) +
                  background_grid(major = "xy", minor = "none",
                                size.major = 0.5, colour.major = "gray85")
  
  
  mC_plt.kt <- mC_plt.bare +
                  geom_line(aes(date_time, kt_1min.scl, colour = "1min")) +
                  geom_line(aes(date_time, kt, colour = "1hr"),
                            alpha = 1/1.5, size = 1.1) +
                  scale_colour_manual(name = NULL, values = sample_colors,
                                      guide = guide_legend(override.aes = list(size = 3))) +
                  labs(x = NULL,
                       y = bquote(k[t]))
  
  mC_plt.ghi <- mC_plt.bare +
                  geom_line(aes(date_time, ghi_1min.scl, colour = "1min")) +
                  geom_line(aes(date_time, ghi, colour = "1hr"),
                            alpha = 1/1.5, size = 1.1) +
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
                               rel_heights = c(1,0.25,1),
                               align = "v") + 
                        draw_grob(legend, 0, 1/2.25, 1, .25/2.25)
  
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
                    geom_freqpoly(aes(cove_var, colour = "cove"), binwidth = 5,
                                  size = 1.5) + 
                    geom_freqpoly(aes(larc_var, colour = "larc"),binwidth = 5,
                                  size = 1.5) +
                    geom_freqpoly(aes(markov_var_cove, colour = "cove model"), binwidth = 5,
                                  size = 1.5) + 
                    geom_freqpoly(aes(markov_var_larc, colour = "larc model"), binwidth = 5,
                                  size = 1.5) +
                    scale_x_continuous(limits = c(0,150)) +
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

# bldg_gif <- get_ani_bldg(20)
# grid_gif <- get_ani_grid(20)
# pv_gif <- get_ani_pv(20)
# disp_gif <- get_ani_disp()