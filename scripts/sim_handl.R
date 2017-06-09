# Simulation Handler - GHG Changes from Demand Response w PV + Battery Storage

# These fns execute individual simulations
# - battery sizing using month containing peak dmd from 1yr time-series
# - 1yr simulation of peak shaving

# and execute multiple simulations in parallel
# with diff bldg and batt types, and degrees of peak shaving

# simulations can also be limited to a certain # of timesteps

# output saved in folders labeled by the "run_id" specified by user

library("data.table")
library("plyr")
library("dplyr")
library("foreach")
library("imputeTS")
library("iterators")
library("doSNOW")
library("futile.logger")
library("R6")
if(!exists("batt_bank", mode = "function")) source("scripts/battery_bank.R")
if(!exists("bill_calc", mode = "function")) source("scripts/costs_calc.R")
if(!exists("bldg_load", mode = "function")) source("scripts/bldg_load.R")
if(!exists("grid_load", mode = "function")) source("scripts/grid_load.R")
if(!exists("pv_load", mode = "function")) source("scripts/pv_load.R")
if(!exists("sys_ctrl.R", mode = "function")) source("scripts/sys_ctrl.R")

make_run_folder = function(run_id) {
  # script for creating a folder in which to save
  # intermediate data (i.e. logs)
  # AND simulation results
  
  # automatically creates new name if a 
  # duplicated "run_id" is chosen
  
  try_path = paste("outputs/", run_id, sep = "")
  if (dir.exists(file.path(try_path))) {
    copy_val = 1
    copy_txt = paste("_copy_",copy_val, sep = "")
    new_path = paste(try_path, copy_txt, sep = "")
    new_id = paste(run_id, copy_txt, sep = "")
    while (dir.exists(file.path(new_path))) {
      copy_val = copy_val + 1
      copy_txt = paste("_copy_",copy_val, sep = "")
      new_path = paste(try_path, copy_txt, sep = "")
      new_id = paste(run_id, copy_txt, sep = "")
    }
  }
  else {
    new_path = try_path
    new_id = run_id
  }
  dir.create(file.path(new_path))
  return(new_id)
}
size_batt <- function(run_id, bldg_nm = NULL, bldg_ts = NULL,
                      pv_ts = NULL, interval = NULL,
                      dmd_frac = NULL, batt_type = NULL,
                      terr = NULL, guess = NULL) {
  # Battery sizing begins with ID'ing month
  # containing peak dmd, and testing the specified
  # level of peak shaving with various batt sizes
  
  log_path = paste(
    "outputs/", run_id, "/batt_sizer_",
    batt_type, "_", dmd_frac, # "_",
    # strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
    ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sizer")
  flog.threshold(ERROR)
  
  max_step <- bldg_ts[which(bldg_ts$kw == max(bldg_ts$kw)),]
  flog.info(paste("Max (", max_step$kw, "kW ) happens at", max_step$date_time), name = "sizer")
  
  size_ts <- filter(bldg_ts,
                    as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  pv_ts <- filter(pv_ts,
                  as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  
  # Below, the cond'n to be satisfied
  # e.g. when the batt is properly sized
  
  # energy from drawing power beyond
  # peak shaving demand threshold (targ_kw)
  # amounts to less than a frac of
  # annual elec. consumption
  
  # unmet_kwh < unmet_thresh
  
  # "incr" determines how batt sizes get
  # adjusted between trials, in order to
  # satisfy above cond'n
  
  unmet_kwh <- sum(bldg_ts$kwh)
  unmet_thresh <- 0.00005*sum(bldg_ts$kwh)
  targ_kw <- max(max_step$kw)*(1 - dmd_frac)
  test_capacity <- ifelse(missing(guess), 1, guess)
  incr <- 0.05
  
  while ((unmet_kwh > unmet_thresh) & (incr > 0.00019)) {
    
    test_capacity <- test_capacity*(1 + incr)
    batt_meta <- list(
      "name" = "Boris the Battery",
      "run_id" = run_id,
      "ctrl_id" = "batt_sizer",
      "time_int" = interval
    )
    temp_batt <- batt_bank$new(
      meta = batt_meta,
      type = batt_type,
      nameplate = test_capacity
    )
    ctrlr_meta <- list(
      "name" = "Sam the System_Controller",
      "run_id" = run_id,
      "ctrl_id" = "batt_sizer",
      "bldg_nm" = bldg_nm,
      "time_int" = interval
    )
    temp_ctrlr <- sys_ctrlr$new(
      meta = ctrlr_meta,
      dmd_targ = targ_kw,
      batt = temp_batt,
      bldg_ts = size_ts,
      pv_ts = pv_ts
    )
    temp_ctrlr$traverse_ts(save_df = FALSE)
    unmet_kwh <- sum(temp_ctrlr$get_sim_df()$unmet_kw)*interval
    flog.error(paste("Unmet kWh -", round(unmet_kwh, 2), "kWh,",
                     "cap", round(test_capacity, 2), "kwh,"),
               name = "sizer")
    
    if ((unmet_kwh < unmet_thresh) & (incr > 0.00019)) {
      unmet_kwh <- unmet_thresh + 1
      test_capacity <- test_capacity/(1 + incr)
      incr <- incr*(2/log(unmet_thresh))
    }
    else {
      if (unmet_kwh > unmet_thresh) {
        incr <- incr*log(unmet_kwh)/2
        
        # Hard cap on capacity at 2GW
        if ((test_capacity > 2000000) & (incr > 2)) {
          test_capacity <- 2000000
          incr <- 0.0001
          unmet_kwh <- unmet_thresh - 1
        }
      }
    }
    incr_sci <- format(incr, digits = 2, scientific = T)
    log_state = paste("Unmet kWh -", round(unmet_kwh, 2),
                      "/", round(unmet_thresh, 2),
                      "- incr", incr_sci)
    flog.error(log_state, name = "sizer")
  }
  # To account for capacity degradation in the future...
  test_capacity <- test_capacity/0.8
  flog.info(paste("Final capacity is", test_capacity, "kwh"),
            name = "sizer")
  
  out_vec <- list("batt_cap" = test_capacity, "unmet_kWh" = unmet_kwh)
  return(out_vec)
}


run_one_sim <- function(run_id, ctrl_id, bldg_nm = NULL, bldg_ts = NULL, pv_ts = NULL,
                        grid_ts = NULL, terr = NULL, dmd_frac = NULL,
                        batt_type = NULL, batt_cap = NULL, interval = NULL,
                        rate = NULL, steps = NULL, save_df = NULL) {
  # All parameters / objects / time-series
  # are initialized for a single simulation
  
  # - "steps" controls the # of time intervals
  # in the simulation
  # - "save_df" determines whether the raw
  # 1yr data is saved
  
  log_path = paste(
    "outputs/", run_id, "/sim_", ctrl_id, "_",
    batt_type, # "_",
    # strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
    ".log", sep = ""
  )
  log_name = paste("sim", ctrl_id, sep = "_")
  flog.appender(appender.file(log_path), name = log_name)
  
  max_step <- filter(bldg_ts, kw == max(kw))
  targ_kw <- max(max_step$kw)*(1 - dmd_frac)    # fraction of peak demand to be shaved
  
  batt_meta <- list(
                    "name" = "Boris the Battery",
                    "run_id" = run_id,
                    "ctrl_id" = ctrl_id,
                    "time_int" = interval
  )
  batt <- batt_bank$new(
                        meta = batt_meta,
                        type = batt_type,
                        nameplate = batt_cap
  )
  disp_meta = list(
                  "name" = "Doris the Dispatch",
                  "run_id" = run_id,
                  "ctrl_id" = ctrl_id
  )
  disp <- disp_curv$new(meta = disp_meta,
                        terr = terr)
  ctrlr_meta <- list(
                    "name" = "Sam the System_Controller",
                    "run_id" = run_id,
                    "ctrl_id" = ctrl_id,
                    "bldg_nm" = bldg_nm,
                    "time_int" = interval
  )
  ctrlr <- sys_ctrlr$new(
                          meta = ctrlr_meta,
                          dmd_targ = targ_kw,
                          batt = batt,
                          bldg_ts = bldg_ts,
                          pv_ts = pv_ts,
                          grid_ts = grid_ts,
                          disp = disp
  )
  # log_rand reduces the # of logged simulations
  # each log reaches ~6 MB @ 1yr of simulation
  
  # this is a crude measure of how
  # far along a sim is. especially useful
  # when many parallel sims are in progress
  
  log_rand <- ifelse(sample(4, 1) < 2, TRUE, FALSE)
  ctrlr$traverse_ts(n = steps, log = log_rand, save_df = save_df)
  
  # Sim results are summarized
  # reduced to a single row of values
  # to be compiled in "sim_year" function
  
  sim_df <- ctrlr$get_sim_df()
  
  # operational & Utility rate summary
  batt_kw.max <- max(sim_df$batt_kw)
  batt_kw.min <- min(sim_df$batt_kw)
  batt_cyceq <- max(sim_df$cyc_eq)
  control_plc2erta <- sum(sim_df$bldg_plc2erta)
  dr_plc2erta <- sum(sim_df$grid_plc2erta)
  r <- rate
  
  annual_bill <- sim_df %>%
    mutate(mo = strftime(date_time, format = "%m"),
           bldg_kwh = bldg_kw*interval,
           grid_kwh = grid_kw*interval,
           pv_kwh = pv_kw*interval,
           unmet_kwh = unmet_kw*interval,
           curtail_kwh = curtail_kw*interval) %>%
    group_by(mo) %>%
    summarize(kw = max(bldg_kw), kw_dr = max(grid_kw),
              kwh = sum(bldg_kwh), kwh_dr = sum(grid_kwh),
              pv_kwh = sum(pv_kwh), unmet_kwh = sum(unmet_kwh),
              curtail_kwh = sum(curtail_kwh),
              kw_cost = max(control_kw.cost), kw_dr_cost = max(dr_kw.cost),
              kwh_cost = sum(control_kwh.cost), kwh_dr_cost = sum(dr_kwh.cost),
              control_cost = kw_cost + kwh_cost,
              dr_cost = kw_dr_cost + kwh_dr_cost) %>%
    mutate(mtr_cost = 34.74,
           control_cost = control_cost + mtr_cost,
           dr_cost = dr_cost + mtr_cost) %>%
    summarize_if(is.numeric, sum) %>%
    as.list()

  batt_lsc <- get_batt_lsc(batt, rate)
  
  out_vec <- list("batt_kw.max" = batt_kw.max, "batt_kw.min" = batt_kw.min,
                  "batt_cyceq" = batt_cyceq, "control_plc2erta" = control_plc2erta,
                  "dr_plc2erta" = dr_plc2erta, "interest" = r)
  out_vec <- append(out_vec, annual_bill)
  out_vec <- append(out_vec, batt_lsc)
  
  return(out_vec)
}

sim_year <- function(run_id, bldg = NULL, cop = 1, dmd_fracs = seq(0.2,0.65,0.05),
                     batt_type = NULL, terr = NULL, guess = NULL,
                     steps = NULL, is_pv = TRUE, num_clust = 3) {
  
  run_id = make_run_folder(run_id)
  
  # Tests shaving b/w 20% and 75% pk. dmd
  dmd_fracs = dmd_fracs
  
  # Tracks parallel, jittered copies of time-series
  ts_index = seq(0,cop)
  
  # Creates a list spanning entire state space
  # of simulations
  # (can be expanded to include other params
  #  that need to be sim'd separately:
  #     - degree of randomization / jittering
  #     - pv type / size)
  param_combns = as.vector((expand.grid(dmd_fracs, ts_index)))
  
  # Bldg, grid, and PV time-series are initialized
  # stochastic copies of time-series are "jittered"
  # with a factor of 0.10
  # (see respective source files
  #   - "bldg_load.R"
  #   - "grid_load.R"
  #   - "pv_load.R"
  
  #   ...for more info on this factor
  # )
  
  bldg <- get_bldg(run_id = run_id, copies = cop, type = bldg)
  log_path = paste("outputs/", run_id,"/sim_", bldg$get_metadata()[["load_nm"]], "_",
                   batt_type,
                   # rand_factors[["bldg"]], "_",
                   # rand_factors[["grid"]], "_",
                   ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sim_1yr")
  
  grid_df = get_grid(run_id, copies = bldg$get_ts_count() - 1,
                     terr = terr)
  
  if (is_pv) {
    pv = get_pv(run_id, copies = bldg$get_ts_count(), # pv copies counted differently
                type = bldg$get_metadata()[["load_nm"]])
  }
  if (!is_pv) { # else not working for some reason
    pv = get_pv(run_id, copies = bldg$get_ts_count(), # pv copies counted differently
                type = "empty")
  }
  flog.info(paste("Intialized PV with", pv$nameplate, "kw"), name = "sim_1yr")
  flog.info(paste("Intialized grid as", grid_df$get_metadata()[["load_nm"]]), name = "sim_1yr")
  
  # Setting up parallel sims for execution...
  cl <- makeCluster(num_clust)
  registerDoSNOW(cl)
  funs_to_pass = c("run_one_sim", "size_batt")
  pkgs_to_pass = c("dplyr", "futile.logger", "imputeTS")

  # The # of copies time-series to save
  # for each permutation of other params
  run_to_save <- sample(1:bldg$get_ts_count(), 1)

  output_df <- foreach(i = 1:(nrow(param_combns)),
                   .combine = "rbind.data.frame",
                   .multicombine = TRUE,
                   .errorhandling = "remove",
                   .export = funs_to_pass,
                   .packages = pkgs_to_pass,
                   .verbose = TRUE) %dopar% {
  if(!exists("batt_bank", mode = "function")) source("scripts/battery_bank.R")
  if(!exists("disp_curv", mode = "function")) source("scripts/dispatch_curve.R")
  if(!exists("bldg_load", mode = "function")) source("scripts/bldg_load.R")
  if(!exists("grid_load", mode = "function")) source("scripts/grid_load.R")
  if(!exists("pv_load", mode = "function")) source("scripts/pv_load.R")
  if(!exists("sys_ctrl.R", mode = "function")) source("scripts/sys_ctrl.R")
  tryCatch({
    
    
    log_path = paste("outputs/", run_id,"/sim_",
                     bldg$get_metadata()[["load_nm"]], "_",
                     batt_type, # "_", 
                     # rand_factors[["bldg"]], "_",
                     # rand_factor[["pv"]], "_",
                     # rand_factors[["grid"]],
                     ".log", sep = ""
    )
    flog.appender(appender.file(log_path), name = "sim_1yr")
    
    test_dmd = param_combns[i,1]
    test_ts = param_combns[i,2]
    test_rt = 0.05
    c_id = paste("ctrlr", test_dmd, test_ts, sep = "_")
    
    flog.info(paste0("Sim @ dmd_frac ", test_dmd,
                     # ", ", i, " / ", nrow(param_combns),
                     ". Interest rt is ", test_rt, 
                     # ", ", ceiling(i/(nrow(param_combns)*ncol(param_combns))),
                     # " / ", ncol(param_combns),
                     ". No rand factors yet. Total ",
                     bldg$get_ts_count()*nrow(param_combns), " sims."),
              name = "sim_1yr")
    
    # Initializing & aligning individual time-series
    bldg_ts = bldg$get_ts_df(test_ts) %>%
      mutate(date_time = strftime(date_time,
                                  format = "2014-%m-%d %H:%M:%S")) %>%
      na.omit() %>%
      arrange(date_time)
    grid_ts = grid_df$get_ts_df(test_ts) %>%
      mutate(date_time = strftime(date_time,
                                  format = "2014-%m-%d %H:%M:%S")) %>%
      filter(date_time %in% bldg_ts$date_time) %>% 
      group_by(date_time) %>%
      summarise_if(is.numeric, "mean") %>%
      full_join(select(bldg_ts, date_time), by = "date_time") %>%
      mutate(mw = na.ma(mw, k = 4)) %>%
      arrange(date_time)
    pv_ts = pv$get_ts_df(test_ts) %>%
      mutate(date_time = strftime(date_time,
                                  format = "2014-%m-%d %H:%M:%S")) %>%
      group_by(date_time) %>%
      summarise_if(is.numeric, "mean") %>%
      full_join(select(bldg_ts, date_time), by = "date_time") %>%
      mutate(kw = na.ma(kw, k = 4)) %>%
      filter(date_time %in% bldg_ts$date_time)
    interval = bldg$get_metadata()[["time_int"]]
    
    flog.info(paste("Time-series", test_ts, "length:", nrow(bldg_ts), "- bldg,",
                    nrow(grid_ts), "- grid,", nrow(pv_ts), "- pv,"), name = "sim_1yr")
    flog.info(paste("Interval is", interval), name = "sim_1yr")
    
    batt_cap = size_batt(run_id = run_id,
                         bldg_nm = bldg$get_metadata()[["load_nm"]],
                         bldg_ts = bldg_ts,
                         pv_ts = pv_ts,
                         interval = interval,
                         dmd_frac = test_dmd,
                         batt_type = batt_type,
                         terr = terr,
                         guess = guess)$batt_cap
    flog.info(paste("Running", c_id, ".", i, 
                    "out of", nrow(param_combns)),
              name = "sim_1yr")
    
    if (test_ts == run_to_save) {
      save_df = TRUE
    }
    else {
      save_df = FALSE
    }
    
    # Summarizing pre-simulation info
    one_output <- list("run_id" = run_id, bldg = bldg$get_metadata()[["load_nm"]],
                      pv_kw = pv$nameplate,
                      dmd_frac = test_dmd, ts_num = test_ts,
                      batt_type = batt_type, batt_cap = batt_cap)
    pv_cost <- get_pv_cost(pv, test_rt)
    c2g_impacts <- get_pv_batt_plc2erta(pv, batt_type, batt_cap)
    
    # Call to run simulation
    one_sim <- run_one_sim(run_id = run_id, ctrl_id = c_id,
                          bldg_nm = bldg$get_metadata()[["load_nm"]],
                          bldg_ts = bldg_ts, pv_ts = pv_ts,
                          grid_ts = grid_ts, terr = terr,
                          dmd_frac = test_dmd, batt_type = batt_type,
                          batt_cap = batt_cap, interval = interval,
                          rate = test_rt, steps = steps, save_df = save_df)
    
    # Attaching remaining simulation results
    one_output = append(one_output, pv_cost)
    one_output = append(one_output, c2g_impacts)
    one_output = append(one_output, one_sim)
    one_output
    },
    
    error = function(e) {err_msg = paste("Error:", e)
                                     flog.info(e, name = "sim_1yr")
                                     return(err_msg)
    })
  }
  stopCluster(cl)
  
  # Attaching add'l run summary info
  output_df <- output_df %>%
    mutate(tac_lo = lsc_lo + pv_levcost_lo + dr_cost,
           tac_hi = lsc_hi + pv_levcost_hi + dr_cost,
           prof_lo = control_cost - tac_lo,
           prof_hi = control_cost - tac_hi) %>%
    select(-(kw_cost:kwh_dr_cost),-mtr_cost)

  write.csv(output_df, paste0("outputs/", run_id, "/",
            run_id, "_run_results.csv"))
  return(output_df)
}

sim_results <- sim_year(run_id = "apts_test2",
                         bldg = "apt", cop = 2,
                         batt_type = "vrf",
                         terr = "nyiso", guess = 2.5,
                         num_clust = 3)

## For use in remote EC2 sessions

# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# # usage
# packages <- c("data.table", "doSNOW", "foreach",
#               "futile.logger", "imputeTS")
# ipak(packages)

# sim_results

## size_results is a sample fn call for running simulations:

# "cop" refers to the # of time-series simulated
#  per comb'n of bldg, batt, and grid type

# "steps" refers to the # of time-steps simulated
# if NULL, a full year is sim'd

# "num_clust" specifies the max # of simulations
# that can be run, depending on the computer to be used