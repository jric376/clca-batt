# Plots + animations

# Scripts, objects, and simulation results
# are imported as needed within each fn.

library("data.table")
library("plyr")
library("dplyr")
library("lubridate")
library("imputeTS")
library("reshape2")
library("scales")
library("tidyr")
library("foreach")
library("iterators")
library("doSNOW")
library("futile.logger")

library("cowplot")
library("grid"); library("gridExtra")
library("ggplot2"); library("ggmap"); library("ggrepel")

# library("animation") 
# library("gganimate") # not updated for R 3.3.3
# gganimate code blocks are commented out

# if (!dir.exists(file.path("outputs/plots"))) {
#   dir.create(file.path("outputs/plots"))
# }

# Color pallettes for use in plotting
cbb_qual <- c("#E69F00", "#999999","#CC79A7", "#009E73", "#F0E442",
              "#000000", "#0072B2", "#D55E00", "#56B4E9")

cbb_qual.n <- c("Biomass" = "#E69F00", "Coal-based" = "#999999",
                "Hydro" = "#CC79A7", "Landfill Gas" = "#009E73",
                "Nat. Gas" = "#F0E442", "Nuclear" = "#000000", 
                "Petro-fuels" = "#0072B2", "PV" = "#D55E00",
                "Wind" = "#56B4E9")
cbb_qual.enduse <- c("Ext. Equipment" = "#E69F00", "Int. Equipment" = "#999999",
                     "Ext. Lighting" = "#CC79A7", "Int. Lighting" = "#009E73",
                     "Refrigeration" = "#D55E00", "Space Cooling" = "#F0E442",
                     "Space Heating" = "#0072B2")

# Transformations / unit conversions for use
# in data cleaning and plotting
asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}
to_kg <- function(lb_val) {
  return(lb_val/2.205)
}
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - 7*lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - 1*lwidth, lwidth)))
  # grid.newpage()
  # grid.draw(combined)
  return(combined)
} # adapted from https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

# SAMPLE SET OF RUN RESULTS NAMES, NOT FOR RE-USE
{
apts_runs <- c("apts_lion_02_075_by005",
               "apts_pba_02_075_by005",
               "apts_nas_02_075_by005",
               "apts_vrf_02_075_by005") 
market_runs <- c("supermarket_lion_02_075_by005",
                 "supermarket_pba_02_075_by005",
                 "supermarket_nas_02_075_by005",
                 "supermarket_vrf_02_075_by005") 
office_runs <- c("office_lion_02_075_by005",
                 "office_pba_02_075_by005",
                 "office_nas_02_075_by005",
                 "office_vrf_02_075_by005")
hospital_runs <- c("hospital_lion_02_075_by005",
                   "hospital_pba_02_075_by005",
                   "hospital_nas_02_075_by005",
                   "hospital_vrf_02_075_by005")
all_runs <- unlist(c(apts_runs, market_runs,
                     office_runs, hospital_runs))
}

# Data prep functions
summarise_run_costs <- function(df){
  output <- mutate(df,
                   tac_lo = lsc_lo + pv_levcost_lo + dr_cost,
                   tac_hi = lsc_hi + pv_levcost_hi + dr_cost,
                   prof_lo = (control_cost - tac_lo)*life_avg,
                   prof_hi = (control_cost - tac_hi)*life_avg,
                   prof_lo_n = prof_lo / func_unit,
                   prof_hi_n = prof_hi / func_unit)
  
  return(output)
}
summarise_run_plc2e <- function(df){
  output <- mutate(df,
                   control_plc2erta = control_plc2erta*life_avg,
                   dr_plc2erta = dr_plc2erta*life_avg,
                   net_plc2erta = (batt_plc2erta + pv_plc2erta +
                                     dr_plc2erta - control_plc2erta),
                   plc2erta_n = to_kg(net_plc2erta / func_unit))
  
  return(output)
}
get_combined_runs <- function(runs) {
  
  results <- data.frame()
  for (runs in runs) {
    results_filenm <- paste(runs, "run_results.csv", sep = "_")
    results.0 <- fread(paste0("outputs/", runs, "/", results_filenm)) %>%
      select(-V1) %>%
      as.data.frame()
    results <- rbind.data.frame(results, results.0)
  }
  
  return(results)
}
get_run_results <- function(runs, fu = "all", prof_lo_lim = FALSE,
                            save = FALSE, save_id = NULL) { # specify functional unt as needed, all or ESS_only
  
  tryCatch({
    
    if (length(runs) > 1) {
      # accommodates combined run_ids from get_combined_runs
      results <- get_combined_runs(runs)
    }
    
    else {
      results_filenm <- paste(runs, "run_results.csv", sep = "_")
      results <- fread(paste0("outputs/", runs, "/", results_filenm)) %>%
        select(-V1) %>%
        as.data.frame()
    }
    # degrad_factor <-  0.9 # assumes PV declines by 1% every year, so by yr 20 it is at 80% orig. cap.
    degrad_factor <- 1
    results <- results %>%
      mutate(bldg = ifelse(bldg == "apt", "Apartments",
                           ifelse(bldg == "office", "Office",
                                  ifelse(bldg == "supermarket", "Supermarket",
                                         "Hospital"))),
             batt_type = ifelse(batt_type == "li_ion", "Li-ion",
                                ifelse(batt_type == "nas", "NaS",
                                       ifelse(batt_type == "pb_a","Pb-a","VRF"))),
             life_avg = (life_hi + life_lo)/2,
             pv_frac = pv_kwh / (pv_kwh + (batt_cap*batt_cyceq)),
             pv_kwh = pv_kwh*degrad_factor*life_avg,
             batt_kwh = batt_cap*batt_cyceq*life_avg,
             pvfrac_kwh = pv_kwh*pv_frac,
             battfrac_kwh = batt_kwh*(1-pv_frac),
             func_unit = pvfrac_kwh + battfrac_kwh,
             alt_func = kwh*life_avg,
             unmet_frac = unmet_kwh / kwh) %>% 
      mutate(bldg = factor(bldg, levels = c("Apartments", "Office",
                                            "Supermarket", "Hospital")))
    
    if(fu == "alt") {
      results <- mutate(results, func_unit = alt_func)
    }
    
    results <- results %>%
      summarise_run_costs() %>% 
      summarise_run_plc2e()
    
    if (is.numeric(prof_lo_lim)) {
      results <- filter(results, prof_lo_n > prof_lo_lim)
    }
    
    results.summ <- select(results, -ts_num) %>%
      group_by(bldg, dmd_frac, batt_type) %>%
      summarise_if(is.numeric, .funs = c("mean", "sd"))
    
    cost_mean <- results.summ %>%
      group_by(dmd_frac, batt_type) %>%
      select(dmd_frac, batt_type, ends_with("mean")) %>% 
      select(dmd_frac, batt_type,
             contains("lsc"),
             contains("dr_cost"),
             contains("control_cost"),
             contains("levcost"),
             contains("tac"),
             contains("prof"))
    cost_mean <- cost_mean %>%
      gather(cat, value, -dmd_frac, -batt_type) %>%
      rename(mean = value) %>%
      arrange(batt_type, dmd_frac) %>%
      ungroup()
    cost_sd <- results.summ %>%
      group_by(dmd_frac, batt_type) %>%
      select(dmd_frac, batt_type, ends_with("sd")) %>% 
      select(dmd_frac, batt_type,
             contains("lsc"),
             contains("dr_cost"),
             contains("control_cost"),
             contains("levcost"),
             contains("tac"),
             contains("prof"))
    cost_sd <- cost_sd %>%
      gather(cat, value, -dmd_frac, -batt_type) %>%
      rename(sd = value) %>%
      arrange(batt_type, dmd_frac) %>%
      ungroup() %>%
      select(sd)
    costs <- cbind.data.frame(cost_mean, cost_sd) %>%
      mutate(label = ifelse(grepl("lsc_", cat), "Batt",
                            ifelse(grepl("*_cost_*", cat), "Grid / Grid + DR",
                                   ifelse(grepl("*levcost_", cat), "PV",
                                          ifelse(grepl("tac_", cat), "TAC",
                                                 ifelse(grepl("*_n_", cat), "Pr/Thru",
                                                        "Pr"))))),
             hi_lo = ifelse(grepl("_hi_", cat), "hi",
                            ifelse(grepl("_lo_", cat), "lo",
                                   ifelse(grepl("control_*", cat), "hi",
                                          ifelse(grepl("dr_*", cat), "lo", "none"))))) %>%
      filter(cat != "P_n") %>%
      select(-cat)
    
    plc2e_mean <- results.summ %>%
      group_by(dmd_frac, batt_type) %>%
      select(dmd_frac, batt_type,
             ends_with("mean")) %>%
      select(dmd_frac, batt_type,
             contains("plc2erta")) %>%
      select(dmd_frac, batt_type,
             contains("control"),
             contains("dr"),
             contains("pv"),
             contains("batt"),
             contains("net"),
             contains("rta_n"))
    plc2e_mean <- plc2e_mean %>%
      gather(cat, value, -dmd_frac, -batt_type) %>%
      rename(mean = value)
    plc2e_sd <- results.summ %>%
      group_by(dmd_frac, batt_type) %>%
      select(dmd_frac, batt_type,
             ends_with("sd")) %>%
      select(dmd_frac, batt_type,
             contains("plc2erta")) %>%
      select(dmd_frac, batt_type,
             contains("control"),
             contains("dr"),
             contains("pv"),
             contains("batt"),
             contains("net"),
             contains("rta_n"))
    plc2e_sd <- plc2e_sd %>%
      gather(cat, value, -dmd_frac, -batt_type) %>%
      rename(sd = value) %>%
      ungroup() %>%
      select(sd)
    plc2e <- cbind.data.frame(plc2e_mean, plc2e_sd) %>%
      mutate(label = ifelse(grepl("control_*", cat), "Grid",
                            ifelse(grepl("dr_*", cat), "Grid + DR",
                                   ifelse(grepl("pv_*", cat), "PV",
                                          ifelse(grepl("net_*", cat), "GHGann",
                                                 ifelse(grepl("batt_*", cat), "Batt",
                                                        "GHG/Thru")))))) %>%
      select(-cat)
    
    results.list <- list("df" = results,
                         "summ" = results.summ,
                         "costs" = costs,
                         "plc2e" = plc2e)
    if(save & is.character(save_id)) {
      saveRDS(results.list, paste0("outputs/",save_id,".rds"))
    }
  },
  error = function(e) return(e))
}

# Plotting functions
get_ts_sampwk <- function(ts, copies, animate = FALSE, save = FALSE) {
  if(ts == "bldg") {
    source("scripts/bldg_load.R")
    ts_obj <- get_bldg(run_id = "plot", copies = copies, type = "office")
    col_nm <- "kw."
  } else {
    if(ts == "grid") {
      source("scripts/grid_load.R")
      ts_obj <- get_grid(run_id = "plot", copies = copies, terr = "nyiso")
      col_nm <- "Mw."
    } else {
      source("scripts/pv_load.R")
      ts_obj <- get_pv(run_id = "plot", copies = copies, type = "office")
      col_nm <- "kw."
    }
  }
  file_nm <- paste0(ts, "_")
  jul_df <- ts_obj$get_ts_df("full") %>% 
    select(date_time, contains(col_nm)) %>% 
    mutate(wk = week(date_time)) %>% 
    filter(wk == 29) %>% # mid-July, when elec demand peaks
    mutate(load.mean = rowMeans(select(., contains(col_nm)))) %>% 
    gather(copy, load, contains(col_nm), load.mean) %>% 
    mutate(is_mean = as.factor(ifelse(copy == "load.mean", 1, 0)))
    
  jul_plt <- ggplot(jul_df, aes(x = date_time, y = load,
                                colour = is_mean, size = is_mean)) +
    geom_line() +
    scale_colour_manual(labels = c("randomized", "mean"),
                        values = cbb_qual[c(6,4)]) +
    scale_size_manual(values = c(1,1.5)) +
    labs(x = "", y = gsub("w\\.", "W", col_nm), colour = NULL, size = NULL) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray85"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none")
  
  if(save) {
    ggsave(filename = paste0("outputs/plots/",
                             file_nm,
                             "load.png"),
           jul_plt,
           width = 10, height = 6.25)
  }
  
  if(animate) {
    # ani_jul_plt <- jul_plt + 
    #   geom_point(aes(frame = run),
    #              colour = "#e31a1c",
    #              size = 1.5)
    # 
    # ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
    # ani_jul_plt <- gg_animate(ani_jul_plt, "outputs/plots/bldg_load.gif")
    # return(list(jul_plt, ani_jul_plt))
  } else return(jul_plt)
}
get_disp_plot <- function(runs = 20, animate = FALSE, save = FALSE) {
  source("scripts/dispatch_curve.R")
  
  disp <- get_test_curve("nyiso")
  full_df <- disp$get_dispatch()
  full_df$run <- 1
  
  for (i in seq.int(2,runs)) {
    
    disp$stochastize_costs(FALSE)
    rand_disp <- disp$get_dispatch()
    rand_disp$run <- i
    full_df <- rbind(full_df, rand_disp)
  }
  full_df <- mutate(full_df, plc2erta = to_kg(plc2erta),
                             cumul_plc2erta = to_kg(cumul_plc2erta),
                             wtd_plc2erta = to_kg(wtd_plc2erta))
  single_run <- filter(full_df, run == sample(runs, 1))
  
  disp_plt.bare <- ggplot(data = full_df,
                          mapping = aes(x = cumul_cap)) +
    labs(x = "Cumulative Capacity (MW)") +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.box = "horizontal",
          legend.background = element_rect(colour = "gray75",
                                           fill = "gray85")) +
    background_grid(major = "xy", minor = "none",
                    size.major = 0.5, colour.major = "gray85")
  
  # title <- ggdraw() + draw_label("NYISO Dispatch Curve and Cumulative Emissions Factor",
  #                                fontface = "bold")
  
  disp_cost_spread <- disp_plt.bare +
                    labs(y = "Marg. Cost ($ / kWh)") +
                    expand_limits(y = c(0, 0.32)) +
                    geom_point(aes(y = MC_rand,
                                   size = namepcap,
                                   colour = fuel_type
                                  ),
                               alpha = 1/3) +
                    geom_point(data = single_run,
                               aes(y = MC_rand,
                                   size = namepcap,
                                   fill = fuel_type
                                  ),
                               colour = "black",
                               shape = 21) +
                    scale_size(name = bquote(scriptstyle(MW[plant])),
                               breaks = c(250,500,1000,2000),
                               range = c(3,20)) +
                    scale_fill_manual(name = NULL, values = cbb_qual,
                                      guide = guide_legend(override.aes = list(alpha = 1,
                                                                               size = 5))) +
                    scale_colour_manual(name = NULL,
                                        values = cbb_qual,
                                        guide = "none") +
                    theme(legend.position = c(0.25, 0.70),
                          legend.box = "horizontal",
                          legend.background = element_rect(colour = "gray75",
                                                           fill = "gray90"))
  
  disp_emish_spread <- disp_plt.bare +
                        labs(y = bquote("kg"~ CO[scriptstyle(2)]~ "eq / MWh")) +
                        expand_limits(y = c(0, 0.32)) +
                        geom_point(aes(y = cumul_plc2erta,
                                       size = wtd_plc2erta,
                                       colour = fuel_type
                                      ),
                                   alpha = 1/3) +
                        geom_point(data = single_run,
                                   aes(y = cumul_plc2erta,
                                       size = wtd_plc2erta,
                                       fill = fuel_type
                                      ),
                                   colour = "black",
                                   shape = 21) +
                        scale_size(name = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / h")),
                                   breaks = c(0,3E5,3E6),
                                   range = c(4,15)) +
                        scale_fill_manual(name = NULL,
                                          values = cbb_qual,
                                          guide = "none") +
                        scale_colour_manual(name = NULL,
                                          values = cbb_qual,
                                          guide = "none") +
                        theme(legend.position = c(0.25, 0.70),
                              legend.box = "horizontal",
                              legend.background = element_rect(colour = "gray75",
                                                               fill = "gray90"))
  
  disp_plt <- plot_grid(disp_cost_spread, disp_emish_spread,
                        labels = c("A","B"),
                        nrow = 2, align = "v")
  # disp_plt <- ggplot() + 
  #   annotation_custom(
  #     grid.arrange(disp_cost_spread, disp_emish_spread,
  #                   ncol = 1)
  #     )
  
  ### For saving combined plot
  if (save) {
    ggsave(filename = "outputs/plots/disp_nyiso_combined.png",
           disp_plt, height = 12.5, width = 10)
  }
  ### For saving individual ggplot objects
  if (save) {
    ggsave(disp_cost_spread, filename = "outputs/plots/disp_nyiso_cost_spread.png",
           width = 10, height = 6.25, units = "in")
    ggsave(disp_emish_spread, filename = "outputs/plots/disp_nyiso_emish_spread.png",
           width = 10, height = 6.25, units = "in")
  }
  if (animate) {
    ani_disp_cost <- disp_plt.bare +
                      labs(y = "Marg. Cost ($ / kWh)",
                           title = "NYISO Dispatch Curve") +
                      expand_limits(y = c(0, 0.32)) +
                      geom_point(aes(y = MC_rand,
                                     size = namepcap
                                     ),
                                  alpha = 1/2,
                                  color = "gray35") +
                      geom_line(aes(y = MC_rand,
                                    size = namepcap,
                                    frame = run
                                    ),
                                size = 1,
                                colour = "gray65") +
                      geom_point(aes(y = MC_rand,
                                     size = namepcap,
                                     fill = fuel_type,
                                     frame = run
                                     ),
                                  colour = "black", shape = 21) +
                      scale_size(name = bquote(scriptstyle(MW[plant])),
                                  breaks = c(250,500,1000,2000),
                                  range = c(3,20)) +
                      scale_fill_manual(name = NULL, values = cbb_qual,
                                        guide = guide_legend(override.aes = list(alpha = 1,
                                                                                 size = 5))) +
                      theme(legend.position = c(0.25, 0.70),
                            legend.box = "horizontal",
                            legend.background = element_rect(colour = "gray75",
                                                             fill = alpha("gray85",
                                                                          1/2)))
    
    ani_disp_emish <- disp_plt.bare +
                        labs(y = bquote("kg"~ CO[scriptstyle(2)]~ "eq / MWh"),
                                 title = "NYISO Cumulative Emissions Factor") +
                        geom_point(aes(y = cumul_plc2erta,
                                       size = wtd_plc2erta
                                       ),
                                    alpha = 1/2,
                                    color = "gray35") +
                        geom_line(aes(y = cumul_plc2erta,
                                      size = wtd_plc2erta,
                                      frame = run
                                      ),
                                  size = 1,
                                  colour = "gray65") +
                        geom_point(aes(y = cumul_plc2erta,
                                       size = wtd_plc2erta,
                                       fill = fuel_type,
                                       frame = run
                                       ),
                                    colour = "black",
                                    shape = 21) +
                        scale_size(name = bquote("kg"~ CO[scriptstyle(2)]~ "eq / h"),
                                    breaks = c(0,3E5,3E6),
                                    range = c(4,15)) +
                        scale_fill_manual(name = NULL, values = cbb_qual,
                                          guide = guide_legend(override.aes = list(alpha = 1, size = 5))) +
                        theme(legend.position = c(0.26, 0.70), legend.box = "horizontal",
                              legend.background = element_rect(colour = "gray75",
                                                               fill = alpha("gray85", 1/4)))
  
    ani.options(outdir = getwd(), ani.width = 960, ani.height = 600)
    if (save) {
      gg_animate(ani_disp_cost, "outputs/plots/disp_nyiso_cost.gif")
      gg_animate(ani_disp_emish, "outputs/plots/disp_nyiso_emish.gif")
    }
  }
  
  return(disp_plt)
}
get_isoterr_donuts <- function(runs = 20, terr = "nyiso", save = FALSE) {
  source("scripts/dispatch_curve.R")
  
  disp <- get_test_curve(terr)
  full_disp <- disp$get_dispatch()
  
  for (j in seq.int(2,runs)) {
    disp$stochastize_costs(hold_seed = TRUE)
    rand_disp <- disp$get_dispatch()
    full_disp <- rbind(full_disp, rand_disp)
  }
  
  bins <- c("12000" = 12000,"15000" = 15000,"22500" = 22500) # lcolorbar lo-hi + max val from non-avg'd
  full_disp <- full_disp %>%
                  mutate(bin = ifelse(cumul_cap <= bins[1], 1,
                                      ifelse(cumul_cap <= bins[2], 2,
                                             ifelse(cumul_cap <= bins[3], 3,
                                                    -99))),
                         fuel_type = factor(fuel_type)) %>%
                  filter(bin != -99)
  plant_caps <- full_disp %>%
                    group_by(orispl) %>%
                    summarise(namepcap = mean(namepcap), cumul_cap = median(cumul_cap))
                  
  donut_df <- select(full_disp, orispl, fuel_type:bin) %>%
                left_join(plant_caps, by = "orispl") %>%
                group_by(orispl, fuel_type) %>%
                summarise_if(is.numeric, mean) %>%
                mutate(bin = round(bin)) %>%
                group_by(bin, fuel_type) %>%
                select(-orispl, -cumul_cap) %>%
                summarise_at(contains("namepcap"), sum) %>%
                complete(bin, fuel_type, fill = list (namepcap = 0)) %>%
                ungroup() %>%
                group_by(fuel_type) %>%
                mutate(c_mw = cumsum(namepcap)) %>%
                group_by(bin) %>%
                mutate(bin_frac = namepcap / sum(namepcap),
                        tot_frac = c_mw / sum(c_mw)) %>%
                select(bin, fuel_type, namepcap, tot_frac)
  
  donut_plots <- list()
  for (k in 1:length(bins)) {
    donut_df.0 <- filter(donut_df, bin == k, tot_frac > 0)
    donut_df.0 <- donut_df.0 %>%
                    arrange(tot_frac) %>%
                    mutate(ymax = cumsum(tot_frac),
                           ymin = c(0, head(ymax, n = -1)))
    cap_label <- paste0("~", as.character(bins[k]), " MW")
    donut_plot.0 <- ggplot(donut_df.0, aes(fill=fuel_type,
                                           ymax=ymax, ymin=ymin,
                                           xmax=4, xmin=2)) +
      geom_rect(colour = "gray75", size = 0.25) +
      coord_polar(theta="y") +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(name = NULL, values = cbb_qual.n,
                        guide = guide_legend(nrow = 1)) +
      xlim(c(0, 4)) +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.title = element_blank(),
            legend.direction = "horizontal",
            legend.position = "bottom") +
      annotate("text", x = 0, y = 0, 
               label = cap_label,
               fontface = 2, size = 4.25) +
      labs(title="")
    if (k == length(bins)) {
      legend <- get_legend(donut_plot.0)
    }
    
    donut_plots[[length(donut_plots)+1]] <- donut_plot.0 +
      theme(legend.position = "none")
  }
  donut_plots <- plot_grid(plotlist = donut_plots,
                            nrow = 1, align = "h")
  # donut_plots <- ggplot() + 
  #   annotation_custom(grid_arrange_shared_legend(donut_plots[[1]],
  #                                                donut_plots[[2]],
  #                                                donut_plots[[3]],
  #                                                nrow = 1, position = "bottom")) +
  #   theme(panel.background = element_blank())
  # donut_plots <- grid.arrange(donut_plots, legend, nrow = 2,
  #                             heights = c(1, 0.25))
  
  # title <- ggdraw() + draw_label(paste(terr, "Energy Mix (2014)"), fontface = "bold")
  donut_plots <- plot_grid(donut_plots, legend,
                              nrow = 2,
                              rel_heights = c(1, 0.25))
  
  if (save) {
    ggsave(filename = paste0("outputs/plots/", terr, "_donut.png"),
           donut_plots, width = 10, height = 4)
  }
  return(donut_plots)
}
get_run_prof_plc2e <- function(run_results, run_id, save = FALSE) {
  
  df <- run_results$df
  costs <- run_results$costs
  plc2e <- run_results$plc2e
  summ <- run_results$summ
  
  sample_sims <- filter(df,
                        dmd_frac == 0.2 | dmd_frac == 0.45 | dmd_frac == 0.7,
                        prof_lo_n > -1) %>%
    group_by(bldg, dmd_frac) %>%
    sample_n(1, replace = TRUE) %>% 
    ungroup()
  
  if (length(unique(summ$bldg)) == 1) {
    size_plot <- ggplot(data = summ, mapping = aes(x = dmd_frac)) +
      facet_wrap( ~ batt_type, nrow = 1) +
      geom_line(aes(y = batt_cap_mean,
                    colour = batt_cap_mean),
                size = 1.5) +
      labs(x = NULL,
           y = bquote(scriptstyle(NP[scriptscriptstyle(ESS)](kWh)))) +
      scale_y_log10(breaks = c(1,10,100,1E3,1E4,1E5)) +
      scale_shape_identity() +
      scale_fill_gradient2(name = bquote(scriptstyle(Thru[scriptscriptstyle(tot)](kWh))),
                           low = "#253494",
                           mid = "#41b6c4", high = "#ffffcc",
                           midpoint = 0) +
      expand_limits(x = c(0.2,0.75)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.text = element_text(size = 8),
            legend.box = "horizontal",
            legend.position = "none",
            panel.background = element_rect(colour = "gray75", fill = "gray80"),
            panel.grid.major = element_line(colour = "gray85"),
            panel.grid.minor = element_line(colour = "gray85"),
            strip.text.y = element_text(face = "bold"))
    
    prof_plot <- ggplot(data = summ,
                        mapping = aes(x = dmd_frac)) +
      facet_wrap( ~ batt_type, nrow = 1) +
      geom_ribbon(aes(ymin = prof_lo_n_mean,
                      ymax = prof_hi_n_mean),
                  alpha = 1/3) +
      geom_line(aes(y = prof_hi_n_mean,
                    colour = prof_hi_n_mean),
                size = 1.5) +
      geom_line(aes(y = prof_lo_n_mean,
                    colour = prof_lo_n_mean),
                size = 1.5) +
      labs(x = NULL,
           y = bquote(scriptstyle(Pr[dr]~"/ kWh"))) +
      scale_y_continuous(trans = "asinh",
                         breaks = c(-5,-1,-0.5,0,1),
                         labels = trans_format("identity",
                                               function(x) dollar(x))) +
      scale_shape_identity() +
      scale_colour_gradient2(name = bquote(scriptstyle(Pr[dr]~"/"~Thru[scriptscriptstyle(tot)])),
                             labels = dollar,
                             breaks = c(-5000,-500,0),
                             low = "#67000d",
                             mid = "#fb6a4a", high = "#fff5f0",
                             midpoint = 0) +
      expand_limits(x = c(0.2,0.75)) +
      theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
      theme(panel.grid.major = element_line(colour = "gray85")) +
      theme(panel.grid.minor = element_line(colour = "gray85")) +
      theme(legend.text = element_text(size = 8),
            legend.box = "horizontal",
            legend.position = "none") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank())
    
    plc2e_plot <- ggplot(data = summ,
                         mapping = aes(x = dmd_frac)) +
      facet_wrap( ~ batt_type, nrow = 1) +
      geom_line(aes(y = plc2erta_n_mean,
                    colour = plc2erta_n_mean),
                size = 1.5) +
      labs(x = bquote(alpha),
           y = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / kWh"))) +
      scale_y_continuous(trans = "asinh",
                         labels=trans_format("identity", function(x) x),
                         breaks = c(5,1,0,-0.25,-0.5,-1,-5)) +
      scale_colour_gradient2(name = NULL,
                             labels = scientific,
                             breaks = c(round(min(summ$plc2erta_n_mean)*0.75, 2),
                                        round(max(summ$plc2erta_n_mean)*0.75, 2)),
                             low = "#00441b",
                             mid = "#74c476", high = "#f7fcf5",
                             midpoint = median(df$plc2erta_n)) +
      expand_limits(x = c(0.2,0.75)) +
      theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
      theme(panel.grid.major = element_line(colour = "gray85")) +
      theme(panel.grid.minor = element_line(colour = "gray85")) +
      theme(legend.text = element_text(size = 8),
            legend.box = "horizontal",
            legend.position = "none") +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank())
    
    combine_plot <- plot_grid(size_plot, prof_plot, plc2e_plot,
                              ncol = 1, align = "v")
    # combine_plot <- ggplot() +
    #   annotation_custom(
    #     grid.arrange(size_plot, prof_plot, plc2e_plot,
    #                  ncol = 1)
    #   ) + 
    #   theme(panel.background = element_blank())
  }
  
  plc2e_prof_plot <- ggplot() +
    # geom_smooth(data = summ,
    #           aes(x = -plc2erta_n_mean,
    #               y = prof_lo_n_mean,
    #               colour = batt_type),
    #           alpha = 1/2,
    #           size = 1.2,
    #           span = 0.25) +
    # geom_line(data = summ,
    #           aes(x = -plc2erta_n_mean,
    #               y = prof_hi_n_mean,
    #               colour = batt_type),
    #           alpha = 1/2,
    #           size = 1.2) +
    # geom_ribbon(data = summ,
    #             aes(x = -plc2erta_n_mean,
    #                 ymin = prof_lo_n_mean,
    #                 ymax = prof_hi_n_mean),
    #             alpha = 1/3) +
    geom_point(data = df,
               aes(x = -plc2erta_n,
                   y = prof_lo_n,
                   size = batt_cap,
                   fill = batt_type),
               shape = 21,
               alpha = 1/1.2) +
    scale_x_continuous(trans = "asinh",
                       labels=trans_format("identity", function(x) -x),
                       breaks = c(5,1,0.5,0.25,0,
                                  -0.25,-0.5,-1,-5)) +
                       # breaks = c(0.025,0.02,0,-0.1)) +
    scale_y_continuous(trans = "asinh",
                       labels= trans_format("identity", function(x) dollar(x)),
                       breaks = c(-1,-0.5,-0.1,0,0.5)) +
    labs(x = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / kWh")),
         y = bquote(scriptstyle(Pr[dr]~"/ kWh"))) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    scale_size(name = bquote(scriptstyle(NP[ESS])),
               labels = c(10,100,">1000"),
               breaks = c(10,100,1000),
               trans = "sqrt",
               range = c(1.5,9),
               guide = guide_legend(override.aes = list(shape = 21,
                                                        size = c(1.5,4,6),
                                                        fill = "black"))) +
    scale_fill_manual(name = NULL,
                      values = cbb_qual[c(3,5,7,4)],
                      labels = c("Li-ion", "NaS", "Pb-a", "VRF"),
                      guide = guide_legend(override.aes = list(colour = cbb_qual[c(3,5,7,4)],
                                                               size = 4,
                                                               alpha = 1,
                                                               linetype = 0))) +
    scale_colour_manual(name = NULL,
                        values = cbb_qual[c(3,5,7,4)],
                        labels = c("Li-ion", "NaS", "Pb-a", "VRF"))
  
  if (length(unique(summ$bldg)) > 1) {
    plc2e_prof_plot <- plc2e_prof_plot +
      facet_wrap( ~ bldg) +
      coord_cartesian(ylim = c(-0.75, max(df$prof_lo_n)), xlim =c(-0,0.75))
    plc2e_prof_plot.lines <- plc2e_prof_plot +
      geom_vline(xintercept = c(0.047,0.67),
                 lty = 2)
    plot_list <- list("no_lines" = plc2e_prof_plot,
                      "lines" = plc2e_prof_plot.lines)
  } else {
    plc2e_prof_plot <- plc2e_prof_plot +
      geom_label_repel(data = sample_sims,
                       aes(x = -plc2erta_n,
                           y = prof_lo_n,
                           label = dmd_frac),
                       colour = "black", size = 4,
                       box.padding = unit(0.8, "lines"),
                       point.padding = unit(1, "lines"),
                       arrow = arrow(length = unit(0.01, "npc")),
                       force = 5,
                       nudge_x = 0.75*sample_sims$plc2erta_n,
                       nudge_y = -0.02,
                       segment.size = 1.2)
    plot_list <- list("combine" = combine_plot,
                      "no_lines" = plc2e_prof_plot)
  }
  
  if (save) {
    if (length(unique(summ$bldg)) == 1) {
      ggsave(filename = paste0("outputs/plots/", run_id, "_grid.png"),
             combine_plot,
             height = 6.25, width = 8)
    } else {
      ggsave(filename = paste0("outputs/plots/",
                               run_id, "_litcompare_plc2e_prof.png"),
             plc2e_prof_plot.lines,
             height = 6.25, width = 8)
    }
    ggsave(filename = paste0("outputs/plots/", run_id, "_plc2e_prof.png"),
           plc2e_prof_plot,
           height = 6.25, width = 8)
  }
  
  return(plot_list)
}
get_run_levcost_delta <- function(run_results, run_id, save = FALSE) {
  
  df.0 <- run_results$df
  max_prof_lo_n <- -1
  incr <- 1
  levcost_delta_df <- data.frame()
  
  # need to separately pare down ESS and PV prices
  # want to create contour plot where
  # x-axis is reduce_frac_PV
  # y-axis is reduce_frac_ESS
  # contour is prof_lo_n_mean
  
  while(incr > 0) {
    incr <- incr - 0.005
    df <- mutate(df.0,
                 lsc_lo = incr*lsc_lo,
                 lsc_hi = incr*lsc_hi,
                 pv_levcost_lo = incr*pv_levcost_lo,
                 pv_levcost_hi = incr*pv_levcost_hi) %>% 
      summarise_run_costs() %>% 
      summarise_run_plc2e() %>% 
      group_by(bldg, dmd_frac) %>% 
      select(bldg, dmd_frac,
             contains("prof_")) %>% 
      summarise_all(.funs = c("mean", "sd"))
    
    max_prof_lo_n <-  max(df$prof_lo_n_mean)
    max_in_df <- filter(df, prof_lo_n_mean == max(prof_lo_n_mean)) %>%
                    ungroup() %>% 
                    select(bldg, prof_lo_n_mean, prof_lo_n_sd) %>% 
                    mutate(reduce_frac = 1 - incr,
                           upper = prof_lo_n_mean + prof_lo_n_sd,
                           lower = prof_lo_n_mean - prof_lo_n_sd)
    levcost_delta_df <- rbind.data.frame(levcost_delta_df, max_in_df)
  }
  
  levcost_delta_plot <- ggplot(levcost_delta_df,
                               aes(x = reduce_frac,
                                   y = prof_lo_n_mean,
                                   colour = bldg)) +
    geom_line(size = 3) +
    xlab(bquote(scriptstyle("Fractional reduction PV + ESS lev. costs"))) +
    coord_cartesian(ylim = c(-0.1,max(levcost_delta_df$prof_lo_n_mean))) +
    scale_y_continuous(name = bquote(scriptstyle(Pr["dr,max"]~"/ kWh")),
                       trans = "asinh",
                       breaks = c(-0.1,0,0.1,1),
                       labels = trans_format("identity",
                                             function(x) dollar(x))) +
    scale_colour_manual(name = NULL,
                        values = cbb_qual[c(9,2,1,4,8,3)]) +
    scale_linetype_discrete(name = NULL,
                            guide = guide_legend(override.aes = list(size = 0.75))) +
    theme(panel.background = element_rect(colour = "gray75",
                                          fill = "gray80")) +
    theme(text = element_text(size = 20),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray85"),
          legend.text = element_text(),
          legend.background = element_rect(colour = "gray75",
                                           fill = alpha("gray85", 1/2)),
          legend.box = "horizontal",
          legend.position = c(0.2,0.8))
  
  if (save) {
    ggsave(filename = paste0("outputs/plots/", run_id, "_breakeven.png"),
              levcost_delta_plot,
              height = 6.25, width = 8)
  }
  
  return(levcost_delta_plot)
}
get_run_plotsummary <- function(run_results, run_id, save = FALSE) { # returns pv frac, func_unit, unmet plots
  
  df <- run_results$df
  summ <- run_results$summ
  
  pvfrac_plot <- ggplot() +
    geom_point(data = df,
               aes(x = -plc2erta_n,
                   y = pv_frac,
                   size = batt_cap,
                   fill = batt_type),
               shape = 21,
               alpha = 1/1.2) +
    scale_x_continuous(trans = "asinh",
                       labels=trans_format("identity", function(x) -x),
                       breaks = c(1000,10,1,0,-1,-10,-1000)) +
    labs(x = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / kWh")),
         y = bquote(scriptstyle(kWh[scriptscriptstyle(PV)]~"/"~(kWh[scriptscriptstyle(PV)]~"+"~kWh[scriptscriptstyle(ESS)])))) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    scale_size(name = bquote(scriptstyle(NP[ESS])),
               breaks = c(10,1E4,1E6),
               trans = "sqrt",
               range = c(2,7),
               guide = guide_legend(override.aes = list(shape = 21,
                                                        size = c(2,4,7),
                                                        fill = "black"))) +
    scale_fill_manual(name = NULL,
                      values = cbb_qual[c(3,5,7,4)],
                      labels = c("Li-ion", "NaS", "Pb-a", "VRF"),
                      guide = guide_legend(override.aes = list(colour = cbb_qual[c(3,5,7,4)],
                                                               size = 4,
                                                               alpha = 1,
                                                               linetype = 0))) +
    scale_colour_manual(name = NULL,
                        values = cbb_qual[c(3,5,7,4)],
                        labels = c("Li-ion", "NaS", "Pb-a", "VRF"))
  
  
  funcunit_plot <- ggplot(data = df,
                          aes(x = dmd_frac)) +
    geom_smooth(aes(y = func_unit, color = "f.u.")) +
    geom_smooth(aes(y = batt_kwh, color = "ESS")) +
    geom_smooth(aes(y = pv_kwh, color = "PV")) +
    scale_y_log10() +
    scale_color_manual(name = NULL,
                       values = c("f.u." = "red",
                                  "ESS" = "blue",
                                  "PV" = "orange")) +
    labs(x = bquote(alpha),
         y = bquote(scriptstyle(Thru[scriptscriptstyle(tot)]))) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85"))

  unmetfrac_plot <- ggplot() +
    geom_point(data = df,
               aes(x = -plc2erta_n,
                   y = unmet_frac,
                   size = batt_cap,
                   fill = batt_type),
               shape = 21,
               alpha = 1/1.2) +
    scale_x_continuous(trans = "asinh",
                       labels=trans_format("identity", function(x) -x),
                       breaks = c(1000,10,1,0,-1,-10,-1000)) +
    labs(x = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / kWh")),
         y = bquote(scriptstyle(kWh[scriptscriptstyle(unmet)]~"/"~kWh[scriptscriptstyle(tot)]))) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    scale_size(name = bquote(scriptstyle(NP[ESS])),
               breaks = c(10,1E4,1E6),
               trans = "sqrt",
               range = c(2,7),
               guide = guide_legend(override.aes = list(shape = 21,
                                                        size = c(2,4,7),
                                                        fill = "black"))) +
    scale_fill_manual(name = NULL,
                      values = cbb_qual[c(3,5,7,4)],
                      labels = c("Li-ion", "NaS", "Pb-a", "VRF"),
                      guide = guide_legend(override.aes = list(colour = cbb_qual[c(3,5,7,4)],
                                                               size = 4,
                                                               alpha = 1,
                                                               linetype = 0))) +
    scale_colour_manual(name = NULL,
                        values = cbb_qual[c(3,5,7,4)],
                        labels = c("Li-ion", "NaS", "Pb-a", "VRF"))
  
  if (length(unique(summ$bldg)) > 1) {
    pvfrac_plot <- pvfrac_plot +
      facet_wrap( ~ bldg)
    funcunit_plot <- funcunit_plot +
      facet_wrap( ~ bldg)
    unmetfrac_plot <- unmetfrac_plot +
      facet_wrap( ~ bldg)
  }
  
  plot_list <- list("pvfrac" = pvfrac_plot,
                    "funcunit" = funcunit_plot,
                    "unmetfrac" = unmetfrac_plot)
  
  if (save) {
    ggsave(filename = paste0("outputs/plots/", run_id, "_pvfrac.png"),
              pvfrac_plot,
              height = 6.25, width = 8,
              units = "in")
    ggsave(filename = paste0("outputs/plots/", run_id, "_func_unit.png"),
           funcunit_plot,
           height = 6.25, width = 8,
           units = "in")
    ggsave(filename = paste0("outputs/plots/", run_id, "_unmetfrac.png"),
           unmetfrac_plot,
           height = 6.25, width = 8,
           units = "in")
  }
  
  return(plot_list)
}
get_run_barplots <- function(run_results, run_id, save = FALSE) {
  
  costs <- run_results$costs %>%
              filter(dmd_frac == 0.3 | dmd_frac == 0.5 | dmd_frac == 0.7,
                     batt_type == "VRF")
  plc2e <- run_results$plc2e %>%
              filter(dmd_frac == 0.3 | dmd_frac == 0.5 | dmd_frac == 0.7,
                     batt_type == "VRF")
  
  costs$label <- factor(costs$label, levels = c("Grid / Grid + DR",
                                                        "PV", "Batt",
                                                        "TAC", "Pr",
                                                        "Pr/Thru"))
  plc2e$label <- factor(plc2e$label, levels = c("Grid", "Grid + DR",
                                                        "PV", "Batt",
                                                        "GHGann", "GHG/Thru"))
  cost_plot <- ggplot(data = costs,
                       aes(x = label,
                           fill = hi_lo)) +
                  facet_grid(dmd_frac ~ .) +
                  geom_bar(aes(y = mean),
                           colour = "grey65",
                           position = "dodge", stat = "identity") +
                  # geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),
                  #               colour = "black", width = 0.3,
                  #               position = position_dodge(width = 0.9)) +
                  scale_x_discrete(name = NULL) +
                  scale_y_continuous(name = NULL,
                                     trans = "asinh",
                                     labels=trans_format("identity", function(x) dollar(x)),
                                     breaks = c(-1E5,-1E2,0,1E2,1E5)) +
                  scale_fill_manual(name = NULL, values = cbb_qual[c(3,4)]) +
                  theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                  theme(panel.grid.major = element_line(colour = "gray85")) +
                  theme(panel.grid.minor = element_line(colour = "gray85"))
  
  plc2e_plot <- ggplot(data = plc2e,
                       aes(x = label)) +
                  facet_grid(dmd_frac ~ .) +
                  geom_bar(aes(y = mean,
                               fill = label),
                           colour = "grey65",
                           position = "dodge", stat = "identity") +
                  labs(x = NULL,
                       y = bquote("kg"~CO[scriptscriptstyle(2)]~"eq")) +
                  scale_y_continuous(name = bquote("kg"~CO[scriptscriptstyle(2)]~"eq"),
                                     trans = "asinh",
                                     labels=trans_format("identity", function(x) x),
                                     breaks = c(-1E6,-5E3,0,5E3,1E6,1E9)) +
                  scale_fill_manual(name = NULL, values = cbb_qual[c(2,6,8,4,3,1)]) +
                  theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                  theme(panel.grid.major = element_line(colour = "gray85")) +
                  theme(panel.grid.minor = element_line(colour = "gray85")) +
                  theme(legend.position = "none")
    
  plot_list <- list("cost" = cost_plot, "plc2e" = plc2e_plot)
  
  if (save) {
    ggsave(paste0("outputs/plots/", run_id, "_cost_bar.png"),
           cost_plot,
           width = 10, height = 6.25, units = "in")
    ggsave(paste0("outputs/plots/", run_id, "_plc2e_bar.png"),
           plc2e_plot,
           width = 10, height = 6.25, units = "in")
  }
  
  return(plot_list)
}
get_run_emish_hist <- function(run_id, dmd_frac, save = FALSE) {
  path = paste0("outputs/", run_id, "/df")
  temp = list.files(path = path, full.names = TRUE)
  dmd_frac.str = paste0("ctrlr_", dmd_frac, "_")
  
  # this function assumes a time interval of 5min in time-series
  # needs to eventually be updated to handle this flexibly
  
  all_df <- read.csv(sample(temp, 1)) %>%
    mutate_if(is.factor, function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M")) %>%
    mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
           plc2e_rt = ifelse(grid_kw == bldg_kw, 0,
                             (grid_plc2erta - bldg_plc2erta)/((grid_kw - bldg_kw)*(0.001/12))),
           plc2e_rt = to_kg(plc2e_rt),
           operation = ifelse(pv_kw > 0 & batt_kw > 0, "PV + Charge",
                              ifelse(pv_kw > 0 & batt_kw < 0, "PV + Discharge",
                                     ifelse(pv_kw > 0 & batt_kw == 0, "PV",
                                            ifelse(pv_kw == 0 & batt_kw > 0, "Charge",
                                                   ifelse(pv_kw == 0 & batt_kw < 0, "Discharge",
                                                          "Nothing")))))) %>%
    select(-X,-contains("cost")) %>% 
    filter(plc2e_rt > 0,
           !(abs(plc2e_rt - median(plc2e_rt)) > 2*sd(plc2e_rt)))
  
  sample_emish_hist <- ggplot(data = all_df,
                              mapping = aes(x = plc2e_rt,
                                            fill = operation)) +
    geom_density(alpha = 1/2) +
    scale_x_continuous(name = bquote(scriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / MWh"))) +
    scale_fill_manual(name = NULL,
                      values = cbb_qual[c(9,2,1,4,8,3)],
                      guide = guide_legend(override.aes = list(colour = "white",
                                                               alpha = 1))) +
    labs(y = "Probability") +
    theme(panel.background = element_rect(fill = "gray80"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray85")) +
    theme(axis.line = element_blank(),
          axis.ticks = element_line(colour = "gray85"),
          axis.text.y = element_text(angle = 33, hjust = 1, size = 11),
          axis.text.x =  element_text(angle = 33, vjust = 1, hjust = 1))

  if (save) {
    ggsave(paste0("outputs/plots/", run_id,
                  "_frac", dmd_frac, "_samp_emish_hist.png"),
           sample_emish_hist,
           width = 10, height = 8, units = "in")
  }
  
  return(sample_emish_hist)
}
get_run_sampwks <- function(run_id, dmd_frac, save = FALSE) {
  path = paste0("outputs/", run_id, "/df")
  temp = list.files(path = path, full.names = TRUE)
  dmd_frac.str = paste0("ctrlr_", dmd_frac, "_")
  
  all_df <- read.csv(sample(temp, 1)) %>%
              mutate_if(is.factor, function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M")) %>%
              mutate(day_ind = as.numeric(strftime(date_time, format = "%j"))) %>%
              select(-X)

  fill_labels <- c("Battery", "Bldg", "Curtail", "DR",  "PV", "Unmet")
  hr_labels <- unlist(lapply(seq(6,21,3), function(x) ifelse(x>10, paste0(x, ":00"),
                                                             paste0("0", x, ":00"))))
  design_days <- c(37,202)
  design_days.df <- filter(all_df, day_ind %in% design_days) %>%
    mutate(season = ifelse(day_ind == design_days[1], "Winter", "Summer"),
           min = as.numeric(strftime(date_time, format = "%M")),
           hr = as.numeric(strftime(date_time, format = "%H")),
           dt = (hr + min/60)/24) %>%
    select(season, dt, bldg_kw, pv_kw:curtail_kw) %>%
    mutate(dr_kw = bldg_kw + batt_kw) %>%
    gather(component, load, -season, -dt)
  
  sample_wk_plot <- ggplot(data = design_days.df,
                           mapping = aes(x = dt)) +
                      facet_grid(season ~ .) +
                      stat_smooth(aes(y = load, fill = component),
                                  colour = "grey95",
                                  geom = "area", span = 0.15) +
                      scale_x_continuous(breaks = seq(0.15,1.02,0.15),
                                         labels = hr_labels,
                                         expand=c(0,0)) +
                      scale_fill_manual(name = NULL,
                                        labels = fill_labels,
                                        values = cbb_qual[c(9,2,1,4,8,6)]) +
                      labs(x = NULL,
                           y = "kW") +
                      theme(panel.background = element_rect(fill = "gray80"),
                            panel.grid.major = element_line(colour = "gray85"),
                            panel.grid.minor = element_line(colour = "gray85")) +
                      theme(axis.line = element_blank(),
                            axis.ticks = element_line(colour = "gray85"),
                            axis.text.y = element_text(angle = 33, hjust = 1, size = 11),
                            axis.text.x =  element_text(angle = 33, vjust = 1, hjust = 1))
  
  if (save) {
    ggsave(paste0("outputs/plots/", run_id, 
                  "_frac", dmd_frac, "_samp_wk.png"),
           sample_wk_plot,
           width = 10, height = 8, units = "in")
  }
  
  return(sample_wk_plot)
}
get_run_socmap <- function(run_id, dmd_fracs, save = FALSE) { # takes two dmd_fracs
  path = paste0("outputs/", run_id, "/df")
  temp = list.files(path = path, full.names = TRUE)
  frac_str.A = paste0("ctrlr_", dmd_fracs[1], "_")
  frac_str.B = paste0("ctrlr_", dmd_fracs[2], "_")
  
  chdd_df <- fread("inputs/2014_chdd.csv", header = TRUE, stringsAsFactors = FALSE) %>%
    select(date_time, cl) %>%
    mutate(day_ind = as.numeric(strftime(date_time, format = "%j"))) %>%
    select(-date_time)
  # labels match order of cluster numbers
  # in chdd_df
  cl_label <- c("Jul - Sep","Jan - Mar",
                "Apr - Jun","Oct - Dec")
  cl_label <- factor(cl_label, levels = c("Jan - Mar","Apr - Jun",
                                          "Jul - Sep","Oct - Dec"))
  
  day_labels <- c("0" = "Mon", "1" = "Tues", "2" = "Wed",
                  "3" = "Thur", "4" = "Fri", "5" = "Sat", "6" = "Sun")
  run_df.part = read.csv(temp[grepl(frac_str.A, temp)]) %>%
              mutate(dmd_frac = dmd_fracs[1])
  run_df = read.csv(temp[grepl(frac_str.B, temp)]) %>%
              mutate(dmd_frac = dmd_fracs[2]) %>% 
              rbind.data.frame(run_df.part) %>% 
              select(date_time, dmd_frac, soc) %>% 
              mutate_if(is.factor, function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M")) %>%
              mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
                     day = (as.numeric(strftime(date_time, format = "%u")) + 3)%%7,
                     hr = as.numeric(strftime(date_time, format = "%H"))) %>%
              left_join(chdd_df, by = "day_ind") %>%
              mutate(cl = na.ma(cl, k = 4),
                     cl = cl_label[cl]) %>%
              na.omit() %>% 
              select(-date_time, -day_ind) %>%
              group_by(dmd_frac, day, hr, cl) %>% 
              summarise(soc_mean = mean(soc),
                        soc_sd = sd(soc))
  
  hr_labels <- unlist(lapply(seq(3,21,9), function(x) ifelse(x>10, paste0(x, ":00"),
                                                             paste0("0", x, ":00"))))
  lgnd_txt1 = bquote(bar(SoC))
  lgnd_txt2 = bquote(sigma[scriptscriptstyle(SoC)])
  
  run_socmap <- ggplot(run_df, aes(y = day, x = hr)) + 
                facet_grid(cl ~ dmd_frac) +
                geom_tile(aes(fill = soc_mean), colour = "gray80") +
                scale_x_continuous(breaks = seq(2,20,9),
                                   labels = hr_labels,
                                   expand=c(0,0)) +
                scale_y_continuous(breaks = seq.int(0,6),
                                   labels = day_labels,
                                   expand=c(0,0)) +
                scale_fill_gradient2(name = lgnd_txt1, low = "#7b3294",
                                     mid = "#f7f7f7", high = "#008837",
                                     midpoint = mean(run_df$soc_mean),
                                     breaks = c(0,0.25,0.5,0.75,0.95)) +
                labs(x = NULL,
                     y = NULL) +
                theme(panel.background = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_text(angle = 33, hjust = 1, size = 8),
                      axis.text.x =  element_text(angle = 33, vjust = 1, hjust = 1))

  
  if (save) {
    dmd_fracs <- dmd_fracs*100
    save_plot(filename = paste0("outputs/plots/", run_id, 
                                  "fracs", dmd_fracs[1], "_",
                                  dmd_fracs[2], "_soc_map.png"),
              run_socmap,
              base_height = 8, base_width = 6)
  }
  
  return(run_socmap)
}
get_ts_summ <- function(choice, copies, emish) {
  
  rowSds <- function(x) {
    sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
  }
  
  suffixes <- letters[1:(copies+1)]
  
  if (choice != "nyiso") {
    source("scripts/bldg_load.R")
    ts <- get_bldg(run_id = "plot", type = choice, copies = copies)
  }
  if (choice == "nyiso") {
    source("scripts/grid_load.R")
    ts <- get_grid(run_id = "plot", terr = choice, copies = copies)
  }
  
  full_df <- ts$get_ts_df("full")
  
  if (choice != "nyiso") {
    summ_df <- mutate(full_df,
                      kw_mean = rowMeans(select(full_df, contains("kw."))),
                      kw_sd = rowSds(select(full_df, contains("kw."))),
                      kwh_mean = rowMeans(select(full_df, contains("kwh."))),
                      kwh_sd = rowSds(select(full_df, contains("kwh."))))
  }
  if (choice == "nyiso") {
    summ_df <- mutate(full_df,
                      date_time = as.POSIXct(date_time),
                      mw_mean = rowMeans(select(full_df, contains("mw"))),
                      mw_sd = rowSds(select(full_df, contains("mw"))))
    
    if (emish) {
      source("scripts/dispatch_curve.R")
      
      disp <- get_test_curve("nyiso")
      pkgs_to_pass = c("dplyr", "futile.logger", "imputeTS")
      
      cl <- makeCluster(3)
      registerDoSNOW(cl)
      plc2erta <- foreach(i = 1:nrow(summ_df),
                          .combine = "rbind.data.frame",
                          .multicombine = TRUE,
                          .export = "to_kg",
                          .packages = pkgs_to_pass,
                          .verbose = TRUE) %dopar% {
                            plc2erta.mean <- to_kg(disp$get_emish(summ_df$mw_mean[i]))
                            plc2erta.lo <- to_kg(disp$get_emish(summ_df$mw_mean[i] - summ_df$mw_sd[i]))
                            plc2erta.hi <- to_kg(disp$get_emish(summ_df$mw_mean[i] + summ_df$mw_sd[i]))
                            plc2erta.sd <- abs(plc2erta.mean - mean(c(plc2erta.lo, plc2erta.hi)))
                            
                            list("plc2erta_mean" = plc2erta.mean,
                                 "plc2erta_sd" = plc2erta.sd)
                          }
      stopCluster(cl)
      summ_df <- cbind.data.frame(summ_df, data.frame(plc2erta))
    }
  }
  
  return(summ_df)
}
get_ts_heatmap <- function(choice, copies, save = FALSE) {
  
  # d_offset aligns the time-series based on where the weekend appears to be
  # i.e. where there is a period of 2 consecutive days with reduced loads
  # assumes no emissions calculations need to be made
  emish = FALSE
  if (choice != "nyiso") {
    d_offset = 4
    unit_txt = "kw_"
    lgnd_txt1 = bquote(bar(kW))
    lgnd_txt2 = bquote(sigma[scriptscriptstyle(kW)])
    title_txt = tools::toTitleCase(choice)
  }
  if (choice == "nyiso") {
    d_offset = 0
    unit_txt = "mw_"
    lgnd_txt1 = bquote(bar(MW))
    lgnd_txt2 = bquote(sigma[scriptscriptstyle(MW)])
    title_txt = toupper(choice)
  }
  if (choice == "nyiso_plc2erta") {
    emish = TRUE
    d_offset = 0
    unit_txt = "plc2erta_"
    lgnd_txt1 = bquote(scriptstyle(bar("kg"~ CO[scriptstyle(2)]~ "eq / MWh")))
    lgnd_txt2 = bquote(sigma[scriptscriptstyle("kg"~ CO[scriptscriptstyle(2)]~ "eq / MWh")])
    title_txt = bquote("NYISO Weekly"~ CO[scriptstyle(2)]~ "eq Emissions Profile (2014)")
  }
  
  chdd_df <- fread("inputs/2014_chdd.csv", header = TRUE, stringsAsFactors = FALSE) %>%
    transmute(cl, day_ind = yday(date_time))
  # labels match order of cluster numbers
  # in chdd_df
  cl_label <- c("Jul - Sep","Jan - Mar",
                "Apr - Jun","Oct - Dec") %>% 
    factor(levels = c("Jan - Mar","Apr - Jun",
                      "Jul - Sep","Oct - Dec"))
  
  df_choice <- ifelse(choice == "nyiso_plc2erta", "nyiso", choice)            
  summ_df <- get_ts_summ(df_choice, copies, emish = emish) %>%
    mutate(day_ind = yday(date_time),
           hr = hour(date_time),
           dayhr_ind = day_ind + hr/24) %>%
    left_join(chdd_df, by = "day_ind") %>%
    mutate(cl = na.ma(cl, k = 4)) %>%
    na.omit()
  mean_txt <- paste0(unit_txt, "mean")
  sd_txt <- paste0(unit_txt, "sd")
  midpt.mean <- median(select(summ_df, contains(mean_txt))[[mean_txt]])
  midpt.sd <- median(select(summ_df, contains(sd_txt))[[sd_txt]])
  
  heatmap_df.mean <- foreach(k = 1:max(summ_df$cl),
                          .combine = "rbind.data.frame") %do% {
                            
    cl_df <- summ_df %>%
                filter(cl == k) %>%
                select(-cl)
    
    heatmap_df.0 <- cl_df %>%
      select(date_time, hr, dayhr_ind,
             contains("_mean")) %>%
      group_by(dayhr_ind) %>%
      summarise_if(is.numeric, mean) %>%
      mutate(day = (floor(dayhr_ind)+d_offset)%%7) %>%
      select(-dayhr_ind) %>%
      group_by(day, hr) %>%
      summarise_all(mean) %>%
      select(day, hr, contains(unit_txt))
    
    heatmap_df.m <- melt(heatmap_df.0,
                         id.vars = c("day","hr"),
                         measure.vars = c(paste0(unit_txt, "mean")))
    heatmap_df.s <- ddply(heatmap_df.m, .(variable),
                          transform, rescale = scale(value))
    heatmap_mean.0 <- filter(heatmap_df.s, variable == paste0(unit_txt,
                                                              "mean")) %>%
                        mutate(cl = cl_label[k])
  }
  
  heatmap_df.sd <- foreach(k = 1:max(summ_df$cl),
                             .combine = "rbind.data.frame") %do% {
                               
   cl_df <- summ_df %>%
               filter(cl == k) %>%
               select(-cl)
   
   heatmap_df.0 <- cl_df %>%
                     select(date_time, hr, dayhr_ind,
                            contains("_sd")) %>%
                     group_by(dayhr_ind) %>%
                     summarise_if(is.numeric, mean) %>%
                     mutate(day = (floor(dayhr_ind)+d_offset)%%7) %>%
                     select(-dayhr_ind) %>%
                     group_by(day, hr) %>%
                     summarise_all(mean) %>%
                     select(day, hr, contains(unit_txt))
   
   heatmap_df.m <- melt(heatmap_df.0,
                        id.vars = c("day","hr"),
                        measure.vars = c(paste0(unit_txt, "sd")))
   heatmap_df.s <- ddply(heatmap_df.m, .(variable),
                         transform, rescale = scale(value))
   heatmap_sd.0 <- filter(heatmap_df.s, variable == paste0(unit_txt,
                                                           "sd")) %>%
                      mutate(cl = cl_label[k])
  }
  
  day_labels <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
  hr_labels <- unlist(lapply(seq(3,21,3), function(x) ifelse(x>10, paste0(x, ":00"),
                                                             paste0("0", x, ":00"))))
  
  mean_plot <- ggplot(heatmap_df.mean, aes(y = day, x = hr)) + 
    geom_tile(aes(fill = value), colour = "gray80") +
    facet_grid(cl ~ .) +
    scale_x_continuous(breaks = seq(2,20,3),
                       labels = hr_labels,
                       expand=c(0,0)) +
    scale_y_continuous(breaks = seq(0,6,1),
                       labels = day_labels,
                       expand=c(0,0)) +
    scale_fill_gradient2(name = lgnd_txt1, low = "#7b3294",
                         mid = "#f7f7f7", high = "#008837",
                         midpoint = midpt.mean) +
    labs(x = "",
         y = "") +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(angle = 33, hjust = 1, size = 8),
          axis.text.x =  element_text(angle = 33, vjust = 1, hjust = 1))
  
  sd_plot <- ggplot(heatmap_df.sd, aes(y = day, x = hr)) + 
    geom_tile(aes(fill = value), colour = "gray80") +
    facet_grid(cl ~ .) +
    scale_x_continuous(breaks = seq(2,20,3),
                       labels = hr_labels,
                       expand=c(0,0)) +
    scale_y_continuous(breaks = seq(0,6,1),
                       labels = day_labels,
                       expand=c(0,0)) +
    scale_fill_gradient2(name = lgnd_txt2,
                         low = "#7b3294", mid = "#f7f7f7", high = "#008837",
                         midpoint = midpt.sd) +
    labs(x = "",
         y = "") +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(angle = 33, hjust = 1, size = 8),
          axis.text.x = element_text(angle = 33, vjust = 1, hjust = 1))
  
  heatmap_plot <- plot_grid(mean_plot, sd_plot,
                              labels = c("A","B"),
                              ncol = 1, align = "v")
  # heatmap_plot <- ggplot() +
  #   annotation_custom(
  #     grid.arrange(mean_plot, sd_plot, ncol = 1,
  #                  padding = unit(0, "line"))
  #   )
  # title <- ggdraw() + draw_label(paste(title_txt, "Weekly Load Profile (2014)"),
  #                                   fontface = "bold")
  # if (choice == "nyiso_plc2erta") {
  #   title <- ggdraw() + draw_label(title_txt, fontface = "bold")
  # }
  # heatmap_plot <- plot_grid(title, heatmap_plot,
  #                             ncol = 1, rel_heights = c(0.05, 1))
  
  if (save) {
    # ggsave(paste0("outputs/plots/", choice, "_heatmap_mean.png"),
    #        mean_plot,
    #        width = 12, height = 8, units = "in")
    # ggsave(paste0("outputs/plots/", choice, "_heatmap_sd.png"),
    #        sd_plot,
    #        width = 12, height = 8, units = "in")
    ggsave(filename = paste0("outputs/plots/", choice, "_heatmap.png"),
           heatmap_plot,
           height = 12, width = 8)
  }
  
  return(heatmap_plot)
}
get_bldg_enduse_bars <- function(save = FALSE) {
  cooling_cats <- c("Space Cooling", "Fans", "Pumps",
                    "Heat Rejection", "Humidification")
  bldg_df <- read.csv("inputs/bldg_summ.csv",
                      stringsAsFactors = FALSE) %>% 
    mutate(slim_cat = if_else(use_cat %in% cooling_cats,
                              "Space Cooling", use_cat),
           bldg_type = ifelse(bldg_type == "Apt", "Apartments", bldg_type),
           bldg_type = factor(bldg_type,
                              levels = c("Apartments", "Office",
                                                    "Supermarket", "Hospital")),
           area = ifelse(bldg_type == "Apartments", 3135,
                         ifelse(bldg_type == "Office", 4982,
                                ifelse(bldg_type == "Supermarket", 4181,
                                       22422)))) %>%
    mutate_if(is.character, as.factor) %>% 
    group_by(bldg_type, area, src, slim_cat) %>% 
    summarise_if(is.numeric, sum) %>%
    ungroup() %>% 
    spread(src, kwh) %>% 
    mutate(eng_dens = model / area,
           cent_diff = (DOE - model) / DOE,
           frac = model / sum(model)) %>%
    group_by(bldg_type) %>% 
    arrange(frac) %>%
    mutate(ymax = cumsum(frac),
           ymin = c(0, head(ymax, n = -1)))
  
  dens_bars <- ggplot(bldg_df,
                      aes(x = slim_cat,
                          y = eng_dens,
                          fill = slim_cat)) +
    facet_wrap( ~ bldg_type,
                scales = "free_x",
                nrow = 1) +
    geom_bar(position = "dodge", stat = "identity") +
    expand_limits(c(0,0)) +
    scale_fill_manual(name = NULL, values = cbb_qual.enduse,
                      drop = FALSE,
                      guide = guide_legend(nrow = 3)) +
    theme(axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          panel.background = element_rect(colour = "gray75", fill = "gray80"),
          panel.grid.major.y = element_line(colour = "gray85"),
          panel.grid.minor.y = element_line(colour = "gray85")) +
    labs(y = bquote(kWh/m^2))
  
  kwh_bars <- ggplot(bldg_df, aes(y = area,
                                  x = bldg_type)) +
    facet_wrap( ~ bldg_type,
                scales = "free_x",
                nrow = 1) +
    geom_bar(position = "dodge", stat = "identity") +
    expand_limits(c(0,0)) +
    scale_fill_manual(name = NULL, values = cbb_qual.enduse,
                      drop = FALSE,
                      guide = guide_legend(nrow = 3)) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.background = element_rect(colour = "gray75", fill = "gray80"),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    labs(y = bquote(m^2))
  
  both_bars <- plot_grid(dens_bars, kwh_bars,
                         align = "v", nrow = 2,
                         rel_heights = c(3,1))
  # both_bars <- ggplot() +
  #   annotation_custom(
  #     grid.arrange(dens_bars, kwh_bars,
  #                  nrow = 2, heights = c(3,1))
  #   )
  
  if (save) {
    ggsave(filename = paste0("outputs/plots/bldg_enduse_bars.png"),
           both_bars,
           height = 8, width = 6.25)
  }
  
  return(both_bars)
}
get_bldg_ldc <- function(copies, save = FALSE) {
  
  choices <- c("apt", "office", "hospital", "supermarket")
  ldc_df <- data.frame()
  
  for (choice in choices) {
    summ_df <- get_ts_summ(choice, copies, emish = FALSE)
    
    ldc <- summ_df %>%
      select(dayhr_ind, kw_mean, kw_sd) %>%
      group_by(dayhr_ind) %>%
      summarise_if(is.numeric, mean)
    ldc_hist = hist(ldc$kw_mean, breaks = floor(max(ldc$kw_mean)), plot = FALSE)
    ldc = data.frame("hrs" = c(0, ldc_hist$counts),
                     "kw" = ldc_hist$breaks)  %>%
      mutate(cumul_hrs = rev(cumsum(hrs)),
             rel_kw = kw / max(kw),
             hrs_diff = hrs - lag(hrs),
             bldg = choice) %>%
      mutate(bldg = ifelse(bldg == "apt", "Apts",
                           ifelse(bldg == "office", "Office",
                                  ifelse(bldg == "supermarket", "Market",
                                         "Hospital"))))
    
    ldc_df <- rbind.data.frame(ldc_df, ldc)
  }
  
  ldc_plot <- ggplot(data = ldc_df) +
    geom_line(aes(x = cumul_hrs,
                  y = kw,
                  colour = bldg), size = 1.1) +
    labs(x = "hr",
         y = bquote(P[bldg]~"(kW)")) +
    scale_x_log10(breaks = c(10,1000,3000,6000)) +
    scale_colour_manual(name = NULL,
                        values = cbb_qual[c(3,7,4,1)]) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85"))
  
  if (save) {
    ggsave(filename = paste0("outputs/plots/compare_ldc.png"),
           ldc_plot,
           width = 10, height = 6.25, units = "in")
  }
  
  return(ldc_plot)
}
get_isoterr_plots <- function(terr = "nyiso", save = FALSE) {
  source("scripts/dispatch_curve.R")
  
  disp <- get_test_curve("nyiso")
  full_df <- disp$get_dispatch() %>%
                mutate(plc2erta = to_kg(plc2erta))
  disp_plot.cap <- ggplot(full_df,
                          aes(x = factor(fuel_type),
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
  disp_plot.plc2erta <- ggplot(emitting_plants,
                               aes(x = factor(fuel_type),
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
         y = bquote("kg"~ CO[scriptstyle(2)]~ "eq / MWh")) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
    theme(panel.grid.major = element_line(colour = "gray85")) +
    theme(panel.grid.minor = element_line(colour = "gray85")) +
    theme(legend.position = "none")
  
  disp_plot.plc2erta <- plot_grid(ggdraw(),disp_plot.plc2erta,ggdraw(),
                                  nrow = 1, align = "h",
                                  rel_widths = c(0.12,1,0.28))
  # isoterr_boxplot <- ggplot() +
  #   annotation_custom(
  #     grid.arrange(disp_plot.cap, disp_plot.plc2erta,
  #                  nrow = 2)
  #   )
  
  isoterr_boxplot <- plot_grid(disp_plot.cap, disp_plot.plc2erta,
                            labels = c("A","B"),
                            nrow = 2, align = "v",
                            rel_heights = c(1, 0.75))
  # title <- ggdraw() + draw_label("NYISO Plant-level Capacity and Emissions Factors",
  #                                fontface = "bold")
  # isoterr_boxplot <- plot_grid(title, isoterr_boxplot,
  #                               ncol = 1, rel_heights = c(0.075, 1))
  
  if (save) {
      # ggsave(paste0("outputs/plots/", terr, "_namepcap.png"),
      #        disp_plot.cap,
      #        width = 10, height = 6.25, units = "in")
      # ggsave(paste0("outputs/plots/", terr, "_plc2erta.png"),
      #        disp_plot.plc2erta,
      #        width = 10, height = 6.25, units = "in")
      ggsave(filename = paste0("outputs/plots/", terr, "_cap_co2eq.png"),
                isoterr_boxplot, ncol = 1, nrow = 2,
                height = 12.5, width = 10)
    }
  
  return(isoterr_boxplot)
}
get_kt_dist <- function(which_df, save = FALSE) {
  if(which_df == "nyc_nsrdb") {
    solar_df = read.csv("inputs/solar_nsrdb_2014.csv") %>%
      mutate(df = "NYC") %>%
      select(-X)
    label_scale = 2
    y_max = 0.75
  }
  else {
    cove <- read.csv("inputs/solar_bsrn_cove.csv") %>%
      mutate(date_time = strftime(as.POSIXct(date_time), format = "%Y-%m-%d"),
             df = "COVE")
    larc <- read.csv("inputs/solar_bsrn_larc.csv") %>%
      mutate(date_time = strftime(as.POSIXct(date_time), format = "%Y-%m-%d"),
             df = "LARC")
    solar_df <- bind_rows(cove, larc) %>%
      mutate(date_time = as.factor(date_time)) %>%
      group_by(dayhr_ind, date_time, weather, df) %>%
      summarise(kt.bar = mean(kt.bar),
                kt.til = mean(kt.til))
    label_scale = 5
    y_max = 0.3
  }
  sample_days <- sample_n(group_by(solar_df, weather), 2) %>% 
    mutate(date_time = strftime(date_time, format = "%Y-%m-%d"))
  kt_plot <- ggplot(solar_df, aes(x = kt.bar, y = kt.til)) +
    expand_limits(x = c(0, 1.3),
                  y = c(0,y_max)) + 
    geom_point(aes(fill = weather, alpha = 1/3, shape = df),
               size = 3) +
    geom_label_repel(data = sample_days,
                     mapping = aes(label = date_time, alpha = 1/3),
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
    scale_fill_manual(name = NULL, values = cbb_qual[2:3]) +
    scale_shape_manual(name = NULL, values = c(21,23,19)) +
    guides(alpha = "none", size = "none",
           fill = guide_legend(override.aes = list(shape = 19,
                                                   colour = cbb_qual[2:3],
                                                   size = 4))) +
    labs(x = bquote("Daily mean of hourly avg" ~k[t]),
         y = bquote("Daily variation of hourly avg" ~k[t])) +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray85"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = c(0.2, 0.8), legend.box = "vertical",
          legend.background = element_rect(fill = "white", colour = "gray75"))
  
  if(save) {
    ggsave(filename = paste0("outputs/plots/kt_dist_", which_df, ".png"),
           kt_plot, 
           width = 10, height = 6.25, units = "in")
  }
  
  return(kt_plot)
}
get_markov_sample <- function(save = FALSE) {
  
  solar_df <- read.csv("inputs/solar_nsrdb_2014.csv") %>%
    select(date_time, kt, ghi) %>% 
    mutate(date_time = ymd_hms(date_time),
           yr = year(date_time)) %>% 
    filter(yr == 2014) %>% 
    arrange(date_time) %>% 
    select(-yr)
  samp_num <- sample(1:100, 1)
  sample <- readRDS("inputs/solar_min.rds") %>% 
    select(date_time, min_ind, contains(paste0("_",samp_num))) %>% 
    mutate(date_time = date_time - dhours(5)) %>% 
    left_join(solar_df, by = "date_time") %>% 
    fill(kt, ghi) %>% 
    slice(1:(5*24*12))
    
  names(sample) <- c("date_time", "min_ind",
                     "kt_min.cove", "kt_min.larc",
                     "ghi_min.cove", "ghi_min.larc",
                     "kt", "ghi")
  
  sample_colors = c("COVE 1min" = cbb_qual[9],
                    "LARC 1min" = cbb_qual[8],
                    "NSRDB 1hr" = cbb_qual[6])
  mC_plt.bare <- ggplot(sample, aes(x = date_time), alpha = 1/2) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d") +
    theme(panel.background = element_rect(colour = "gray75", fill = "gray80"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          legend.box = "horizontal",
          legend.background = element_rect(fill = "white", colour = "gray75"))
  
  mC_plt.kt <- mC_plt.bare +
    geom_line(aes(y = kt_min.larc, colour = "LARC 1min")) +
    geom_line(aes(y = kt_min.cove, colour = "COVE 1min")) +
    geom_line(aes(y = kt, colour = "NSRDB 1hr"),
              alpha = 1/1.2) +
    scale_colour_manual(name = NULL, values = sample_colors,
                        guide = guide_legend(override.aes = list(size = 3))) +
    labs(x = NULL,
         y = bquote(k[t])) +
    theme(legend.position = "bottom")
  
  mC_plt.ghi <- mC_plt.bare +
    geom_line(aes(y = ghi_min.larc, colour = "LARC 1min")) +
    geom_line(aes(y = ghi_min.cove, colour = "COVE 1min")) +
    geom_line(aes(y = ghi, colour = "NSRDB 1hr"),
              alpha = 1/1.2) +
    scale_colour_manual(name = NULL, values = sample_colors,
                        guide = guide_legend(override.aes = list(size = 2))) +
    labs(x = NULL,
         y = bquote("GHI (W /" ~m^2~")")) +
    theme(legend.position = "none")
  
  grobs <- ggplotGrob(mC_plt.kt)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
  mC_plt.kt <- mC_plt.kt +
    theme(legend.position = "none")
  
  mC_plt.combined <- plot_grid(mC_plt.kt,
                               NULL,
                               mC_plt.ghi,
                               labels = c("A","","B"),
                               nrow = 3,
                               rel_heights = c(1,0.3,1),
                               align = "v") +
                        draw_grob(legend, 0, 1/2.3, 1, .3/2.3)
  
  # mC_plt.combined <- ggplot() +
  #   annotation_custom(
  #     grid.arrange(mC_plt.kt, mC_plt.ghi, nrow = 2)
  #   )
  
  ### For saving combined plot
  if (save) {
    ggsave(filename = "outputs/plots/markov_sample.png",
           mC_plt.combined,
           height = 6.25, width = 10)
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
    ggsave(filename = paste0("outputs/plots/mC_freq.png"),
           mC_freq.plot, 
           width = 10, height = 6.25, units = "in")
  }
  
  return(mC_freq.plot)
}
