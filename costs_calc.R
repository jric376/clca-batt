# Cost and Emissions Calculator

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("R6")

get_bill_cost = function(bldg_nm, timestep, interval, before_kw, after_kw) {
  
  # rate schedules
  {
    sc9 <- data.frame(mo = seq(1,12)) %>%
      mutate(mtr_cost = 34.74) %>%
      filter(!is.na(mo)) %>%
      mutate(kw_first = ifelse((mo < 6 | mo > 9), 112.51, 140.86),
             kw_over = ifelse((mo < 6 | mo > 9), 17.22, 21.82),
             kwh_cost = 0.0236)
    
    sc8 <- data.frame(mo = seq(1,12)) %>%
      mutate(mtr_cost = 34.74) %>%
      filter(!is.na(mo)) %>%
      mutate(kw_first = ifelse((mo < 6 | mo > 9), 233.10, 301.54),
             kw_over = ifelse((mo < 6 | mo > 9), 28.65, 22.13),
             kwh_cost = 0.0186)
  }
  if(bldg_nm == "apt") {
    rate_sched = sc8
    over_thresh = 10
  }
  else {
    rate_sched = sc9
    over_thresh = 5
  }
  
  month = as.numeric(strftime(timestep, format = "%m"))
  costs = filter(rate_sched, mo == month)
  control_kw.cost = costs$kw_first + ifelse(before_kw > over_thresh,
                                         costs$kw_over*(before_kw - over_thresh), 0)
  control_kwh.cost = before_kw*interval*costs$kwh_cost
  dr_kw.cost = costs$kw_first + ifelse(after_kw > over_thresh,
                                         costs$kw_over*(after_kw - over_thresh), 0)
  dr_kwh.cost = after_kw*interval*costs$kwh_cost
  
  output = list(
                "control_kw.cost" = control_kw.cost, "control_kwh.cost" = control_kwh.cost,
                "dr_kw.cost" = dr_kw.cost, "dr_kwh.cost" = dr_kwh.cost
  )
  return(output)
}

get_batt_lsc = function(batt, interest_rt) {
  
  calendar_life = 20
  cyc_life.lo = batt$cyc_fail.lo / batt$cyc_eq
  cyc_life.hi = batt$cyc_fail.hi / batt$cyc_eq
  life.lo = ifelse(cyc_life.lo > calendar_life, calendar_life, cyc_life.lo)
  life.hi = ifelse(cyc_life.hi > calendar_life, calendar_life, cyc_life.hi)
  
  rt_term.lo = (1 + interest_rt)^life.lo
  rt_term.hi = (1 + interest_rt)^life.hi
  crf.lo = (interest_rt*rt_term.lo)/(rt_term.lo - 1)
  crf.hi = (interest_rt*rt_term.hi)/(rt_term.hi - 1)
  
  lsc.lo = batt$nameplate*(batt$cap_cost.lo + batt$om_cost.lo)
  lsc.hi = batt$nameplate*(batt$cap_cost.hi + batt$om_cost.hi)
  if(life.lo > 15) {
    lsc.lo = lsc.lo + batt$nameplate*batt$repl_cost.lo
  }
  if(life.hi > 15) {
    lsc.hi = lsc.hi + batt$nameplate*batt$repl_cost.hi
  }
  
  lsc.lo = lsc.lo*crf.lo
  lsc.hi = lsc.hi*crf.hi
  
  out_list = list("life_lo" = life.lo,
                  "life_hi" =  life.hi,
                  # "rt_term_lo" = rt_term.lo,
                  # "rt_term_hi" = rt_term.hi,
                  # "crf_lo" = crf.lo,
                  # "crf_hi" = crf.hi,
                  "lsc_lo" = lsc.lo,
                  "lsc_hi" = lsc.hi)
  return(out_list)
}

get_pv_cost = function(pv, interest_rt) {
  
  calendar_life = 25
  cost.lo = pv$cap_cost.lo + calendar_life*pv$om_cost.lo
  cost.hi = pv$cap_cost.hi + calendar_life*pv$om_cost.hi
  
  rt_term.lo = (1 + interest_rt)^calendar_life
  rt_term.hi = (1 + interest_rt)^calendar_life
  crf.lo = (interest_rt*rt_term.lo)/(rt_term.lo - 1)
  crf.hi = (interest_rt*rt_term.hi)/(rt_term.hi - 1)
  
  pv_levcost.lo = cost.lo*crf.lo
  pv_levcost.hi = cost.hi*crf.hi
  
  out_list = list("pv_levcost_lo" = pv_levcost.lo,
                  "pv_levcost_hi" = pv_levcost.hi)
  return(out_list)
}

get_pv_batt_plc2erta = function(pv, batt_type, batt_cap) {
  if(!exists("batt_bank", mode = "function")) source("battery_bank.R")
  batt_meta <- list(
    "name" = "Boris the Battery",
    "run_id" = "c2g impacts",
    "ctrl_id" = "c2g impacts",
    "time_int" = 1/12
  )
  batt <- batt_bank$new(
    meta = batt_meta,
    type = batt_type,
    nameplate = batt_cap
  )
  batt_plc2erta <- 1000*batt$nameplate/batt$eng_dens*batt$plc2erta
  pv_plc2erta <- pv$nameplate*pv$plc2erta
  
  out_list = list("pv_plc2erta" = pv_plc2erta,
                  "batt_plc2erta" = batt_plc2erta)
  
  rm(batt, batt_meta)
  
  return(out_list)
}