# Simulation Handler - GHG Changes from Demand Response w PV + Battery Storage

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("data.table")
library("plyr")
library("dplyr")
library('foreach')
library("imputeTS")
library('iterators')
library('doSNOW')
library("futile.logger")
library("R6")
if(!exists("batt_bank", mode = "function")) source("battery_bank.R")
if(!exists("bill_calc", mode = "function")) source("costs_calc.R")
if(!exists("bldg_load", mode = "function")) source("bldg_load.R")
if(!exists("grid_load", mode = "function")) source("grid_load.R")
if(!exists("pv_load", mode = "function")) source("pv_load.R")
if(!exists("sys_ctrl.R", mode = "function")) source("sys_ctrl.R")

make_run_folder = function(run_id) {
  try_path = paste("outputs\\", run_id, sep = "")
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
size_batt <- function(run_id, bldg_nm = NULL, bldg_ts = NULL, pv_ts = NULL, interval = NULL,
                      dmd_frac = NULL, batt_type = NULL, terr = NULL, guess = NULL) {
  
  log_path = paste(
                  "outputs\\", run_id, "\\batt_sizer_",
                  batt_type, "_", dmd_frac, # "_",
                  # strftime(Sys.time(), format = "%d%m%y_%H%M%S"),
                  ".log", sep = ""
  )
  flog.appender(appender.file(log_path), name = "sizer")

  max_step <- bldg_ts[which(bldg_ts$kw == max(bldg_ts$kw)),]
  flog.info(paste("Max (", max_step$kw, "kW ) happens at", max_step$date_time), name = "sizer")

  size_ts <- filter(bldg_ts,
                    as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)
  pv_ts <- filter(pv_ts,
                  as.POSIXlt(date_time)$mo == as.POSIXlt(max_step$date_time)$mo)

  unmet_kwh <- sum(bldg_ts$kwh)
  unmet_thresh <- 0.00005*sum(bldg_ts$kwh)           # max unmet_kwh to trigger adequate size
  targ_kw <- max(max_step$kw)*(1 - dmd_frac)        # fraction of peak demand to be shaved
  test_capacity <- ifelse(missing(guess), 1, guess)
  incr <- 0.05
  
  # need to check if guess satisfies unmet_kwh > unmet_thresh first

  while ((unmet_kwh > unmet_thresh) & (incr > 0)) {

    test_capacity <- test_capacity + incr*max(bldg_ts$kwh)
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
    flog.info(paste("Unmet kWh -", round(unmet_kwh, 2), "kWh,",
                    "cap", round(test_capacity, 2), "kwh,"),
              name = "sizer")

    if ((unmet_kwh < unmet_thresh) & (incr > 0.0019)) {
      unmet_kwh <- unmet_thresh + 1
      test_capacity <- test_capacity - incr*max(bldg_ts$kwh)
      incr <- incr*0.3333
    }
    else {
      if ((unmet_kwh > unmet_thresh) & (incr < 3)) {
      incr <- incr*3
      }
    }
    incr_sci <- format(incr, digits = 2, scientific = T)
    log_state = paste("Unmet kWh -", round(unmet_kwh, 2),
                      "/", round(unmet_thresh, 2),
                      "- incr", incr_sci)
    flog.info(log_state, name = "sizer")
    # noquote(print(log_state))
  }
  test_capacity <- test_capacity/0.8 # to account for capacity degradation in the future
  flog.info(paste("Final capacity is", test_capacity, "kwh"),
            name = "sizer")

  out_vec <- list("batt_cap" = test_capacity, "unmet_kWh" = unmet_kwh)
  return(out_vec)
}

run_one_sim <- function(run_id, ctrl_id, bldg_nm = NULL, bldg_ts = NULL, pv_ts = NULL,
                        grid_ts = NULL, terr = NULL, dmd_frac = NULL,
                        batt_type = NULL, batt_cap = NULL,
                        rate = NULL, steps = NULL, save_df = NULL) {
  
  log_path = paste(
    "outputs\\", run_id, "\\sim_", ctrl_id, "_",
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
  ctrlr$traverse_ts(n = steps, log = TRUE, save_df = save_df)
  sim_df <- ctrlr$get_sim_df()
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

sim_year <- function(run_id, bldg = NULL, cop = 1, batt_type = NULL, terr = NULL,
                      guess = NULL, steps = NULL, is_pv = TRUE) {
  
  run_id = make_run_folder(run_id)
  
  # figure out what demand_frac range to use based on LDC
  
  dmd_fracs = seq(0.2,0.2,0.025)
  rates = seq(0.05, 0.05, 0.05)
  param_combns = as.vector((expand.grid(dmd_fracs, rates)))
  
  bldg <- get_bldg(run_id = run_id, copies = cop, type = bldg)
  log_path = paste("outputs\\", run_id,"\\sim_", bldg$get_metadata()[["bldg"]], "_",
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
                type = bldg$get_metadata()[["bldg"]])
  }
  else {
    pv = get_pv(run_id, copies = bldg$get_ts_count(), # pv copies counted differently
                type = "empty")
  }
  flog.info(paste("Intialized PV with", pv$nameplate, "kw"), name = "sim_1yr")
  flog.info(paste("Intialized grid as", grid_df$get_metadata()[["territory"]]), name = "sim_1yr")
  
  output_df = foreach(i = 1:(nrow(param_combns)),
                      .combine = "rbind.data.frame",
                      .verbose = TRUE) %do% {
    tryCatch({
      cl <- makeCluster(3)
      registerDoSNOW(cl)
  
      test_dmd = param_combns[i,1]
      test_rt = param_combns[i,2]
      funs_to_pass = c("run_one_sim", "size_batt")
      pkgs_to_pass = c("dplyr", "futile.logger", "imputeTS")
  
      flog.info(paste0("Sim @ dmd_frac ", test_dmd,
                        # ", ", i, " / ", nrow(param_combns),
                        ". Interest rt is ", test_rt, 
                        # ", ", ceiling(i/(nrow(param_combns)*ncol(param_combns))),
                        # " / ", ncol(param_combns),
                        ". No rand factors yet. Total ",
                        bldg$get_ts_count()*nrow(param_combns), " sims."),
                name = "sim_1yr")
      
      run_to_save <- sample(1:bldg$get_ts_count(), 1)
      
      sim_df = foreach(j = 1:(bldg$get_ts_count()),
                       .combine = "rbind.data.frame",
                       .multicombine = TRUE,
                       .errorhandling = "remove",
                       .export = funs_to_pass, .packages = pkgs_to_pass,
                       .verbose = TRUE) %dopar% {
                  if(!exists("batt_bank", mode = "function")) source("battery_bank.R")
                  if(!exists("disp_curv", mode = "function")) source("dispatch_curve.R")
                  if(!exists("bldg_load", mode = "function")) source("bldg_load.R")
                  if(!exists("grid_load", mode = "function")) source("grid_load.R")
                  if(!exists("pv_load", mode = "function")) source("pv_load.R")
                  if(!exists("sys_ctrl.R", mode = "function")) source("sys_ctrl.R")
                  tryCatch({
                    
                    log_path = paste("outputs\\", run_id,"\\sim_",
                                     bldg$get_metadata()[["bldg"]], "_",
                                     batt_type, # "_", 
                                     # rand_factors[["bldg"]], "_",
                                     # rand_factor[["pv"]], "_",
                                     # rand_factors[["grid"]],
                                     ".log", sep = ""
                    )
                    flog.appender(appender.file(log_path), name = "sim_1yr")
                    c_id = paste("ctrlr", i, j, sep = "_")
                  
                    
                    bldg_ts = bldg$get_ts_df(j) %>%
                      mutate(date_time = strftime(date_time,
                                                  format = "2014-%m-%d %H:%M:%S")) %>%
                      na.omit() %>%
                      arrange(date_time)
                    grid_ts = grid_df$get_ts_df(j) %>%
                      mutate(date_time = strftime(date_time,
                                                  format = "2014-%m-%d %H:%M:%S")) %>%
                      filter(date_time %in% bldg_ts$date_time) %>% 
                      group_by(date_time) %>%
                      summarise_if(is.numeric, "mean") %>%
                      full_join(select(bldg_ts, date_time), by = "date_time") %>%
                      mutate(mw = na.ma(mw, k = 4)) %>%
                      arrange(date_time)
                    pv_ts = pv$get_ts_df(j) %>%
                      mutate(date_time = strftime(date_time,
                                                  format = "2014-%m-%d %H:%M:%S")) %>%
                      group_by(date_time) %>%
                      summarise_if(is.numeric, "mean") %>%
                      full_join(select(bldg_ts, date_time), by = "date_time") %>%
                      mutate(kw = na.ma(kw, k = 4)) %>%
                      filter(date_time %in% bldg_ts$date_time)
                    interval = bldg$get_metadata()[["time_int"]]
                    
                    flog.info(paste("Time-series", j, "length:", nrow(bldg_ts), "- bldg,",
                                    nrow(grid_ts), "- grid,", nrow(pv_ts), "- pv,"), name = "sim_1yr")
                    flog.info(paste("Interval is", interval), name = "sim_1yr")
                    
                    batt_cap = size_batt(run_id = run_id,
                                         bldg_nm = bldg$get_metadata()[["bldg"]],
                                         bldg_ts = bldg_ts,
                                         pv_ts = pv_ts,
                                         interval = interval,
                                         dmd_frac = test_dmd,
                                         batt_type = batt_type,
                                         terr = terr,
                                         guess = guess)$batt_cap
                    flog.info(paste("Running", c_id,
                                    "out of", bldg$get_ts_count()*nrow(param_combns)),
                              name = "sim_1yr")
                    
                    if (j == run_to_save) {
                      save_df = TRUE
                    }
                    else {
                      save_df = FALSE
                    }
                    one_output = list("run_id" = run_id, bldg = bldg$get_metadata()[["bldg"]],
                                      pv_kw = pv$nameplate,
                                      dmd_frac = test_dmd, ts_num = j,
                                      batt_type = batt_type, batt_cap = batt_cap)
                    pv_cost = get_pv_cost(pv, test_rt)
                    c2g_impacts <- get_pv_batt_plc2erta(pv, batt_type, batt_cap)
                    one_sim = run_one_sim(run_id = run_id, ctrl_id = c_id,
                                          bldg_nm = bldg$get_metadata()[["bldg"]],
                                          bldg_ts = bldg_ts, pv_ts = pv_ts,
                                          grid_ts = grid_ts, terr = terr,
                                          dmd_frac = test_dmd, batt_type = batt_type,
                                          batt_cap = batt_cap, rate = test_rt,
                                          steps = steps, save_df = save_df)
                    
                    one_output = append(one_output, pv_cost)
                    one_output = append(one_output, c2g_impacts)
                    one_output = append(one_output, one_sim)
                    gc()
                    one_output
                    },
                    
                    error = function(e) {err_msg = paste("Error:", e)
                                         flog.info(e, name = "sim_1yr")
                                         return(err_msg)
                    })
        }
      stopCluster(cl)
      sim_df
      },
  
    error = function(e) {err_msg = paste("Error:", e)
                          flog.info(e, name = "sim_1yr")
                          return(err_msg)
                          })
  }
  
  output_df <- output_df %>%
                mutate(tac_lo = lsc_lo + pv_levcost_lo + dr_cost,
                       tac_hi = lsc_hi + pv_levcost_hi + dr_cost,
                       prof_lo = control_cost - tac_lo,
                       prof_hi = control_cost - tac_hi) %>%
                select(-(kw_cost:kwh_dr_cost),-mtr_cost)
                     
  write.csv(output_df, paste0("outputs\\", run_id, "\\",
            run_id, "_run_results.csv"))
  return(output_df)
}

size_results <- sim_year("warm_up", bldg = "office", cop = 2,
                          batt_type = "vrf", terr = "nyiso", guess = 2.5,
                          steps = 20)
one_sim <- fread("***",
                 nrows = 1000) %>%
              select(-V1)