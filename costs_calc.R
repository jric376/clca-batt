# Cost and Emissions Calculator

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("R6")

# interval <- 1/12
# test_df <- read.csv("outputs\\testing_1yr_copy_2\\df\\ctrlr_2_li_ion730W_2.csv", header = T)
# slim_df <- select(test_df, date_time:curtail_kw) %>%
#             mutate(mo = as.POSIXlt(date_time)$mo + 1)

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
  bldg_kw.cost = costs$kw_first + ifelse(before_kw > over_thresh,
                                         costs$kw_over*(before_kw - over_thresh), 0)
  bldg_kwh.cost = before_kw*interval*costs$kwh_cost
  grid_kw.cost = costs$kw_first + ifelse(after_kw > over_thresh,
                                         costs$kw_over*(after_kw - over_thresh), 0)
  grid_kwh.cost = after_kw*interval*costs$kwh_cost
  
  output = list(
                "bldg_kw.cost" = bldg_kw.cost, "bldg_kwh.cost" = bldg_kwh.cost,
                "grid_kw.cost" = grid_kw.cost, "grid_kwh.cost" = grid_kwh.cost
  )
  return(output)
}

get_batt_lsc = function(batt, interest_rt) {
  
  calendar_life = 20
  cyc_life.lo = batt$cyc_fail.lo / batt$cyc_eq
  cyc_life.hi = batt$cyc_fail.hi / batt$cyc_eq
  life.lo = ifelse(cyc_life.lo > calendar_life, calendar_life, cyc_life.lo)
  life.hi = ifelse(cyc_life.hi > calendar_life, calendar_life, cyc_life.hi)
  
  lsc.lo = batt$nameplate*(batt$cap_cost.lo + batt$om_cost.lo)
  lsc.lo = if(life.lo > 15) {
    lsc.lo = lsc.lo + batt$nameplate*batt$repl_cost.lo
  }
  lsc.hi = batt$nameplate*(batt$cap_cost.hi + batt$cap_cost.hi)
  lsc.hi = if(life.hi > 15) {
    lsc.hi = lsc.hi + batt$nameplate*batt$repl_cost.hi
  }
  
  # ADD CAPITAL RECOVERY FACTOR
  
  out_list = list("life.lo" = life.lo,
                  "life.hi" =  life.hi,
                  "lsc.lo" = lsc.lo,
                  "lsc.hi" = lsc.hi)
  return(out_list)
}

batt_meta <- list(
  "name" = "Boris the Battery",
  "run_id" = "run_id",
  "ctrl_id" = "ctrl_id",
  "time_int" = 1/12
)
test_batt <- batt_bank$new(
  meta = batt_meta,
  type = "vrb",
  nameplt = 30
)