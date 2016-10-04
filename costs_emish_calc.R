# Cost and Emissions Calculator

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("dplyr")
library("futile.logger")
library("R6")

interval <- 1/12
test_df <- read.csv("outputs\\testing_1yr_copy_2\\df\\ctrlr_2_li_ion730W_2.csv", header = T)
slim_df <- select(test_df, date_time:curtail_kw)
### CURRENTLY DEFAULTS RATES TO SC 9
monthly_bill <-  mutate(slim_df, mo = as.POSIXlt(date_time)$mo + 1) %>%
                    group_by(mo, mtr_cost = 34.47) %>%
                    filter(!is.na(mo)) %>%
                    summarize(max_kw = max(bldg_kw),
                              bldg_kwh = sum(bldg_kw, na.rm = TRUE)*interval,
                              max_dr_kw = max(grid_kw),
                              grid_kwh = sum(grid_kw, na.rm = TRUE)*interval) %>%
                    mutate(kw_cost = ifelse((mo < 6 | mo > 9), 112.51+(max_kw-5)*17.22,
                                            ifelse((mo >= 6 & mo <= 9), 140.86+(max_kw-5)*21.82, NA)),
                           kw_dr_cost = ifelse((mo < 6 | mo > 9), 112.51+(max_dr_kw-5)*17.22,
                                            ifelse((mo >= 6 & mo <= 9), 140.86+(max_dr_kw-5)*21.82, NA)),
                           kwh_cost = 0.0236*bldg_kwh,
                           kwh_dr_cost = 0.0235*grid_kwh,
                           before_cost = kw_cost + kwh_cost + mtr_cost,
                           after_cost = kw_dr_cost + kwh_dr_cost + mtr_cost)
annual_bill <- group_by(monthly_bill) %>%
                    summarize(
                             kw = max(max_kw), kw_dr = max(max_dr_kw),
                             kwh = sum(bldg_kwh), kwh_dr = sum(grid_kwh),
                             kw_cost = sum(kw_cost), kw_dr_cost = sum(kw_dr_cost),
                             kwh_cost = sum(kwh_cost), kwh_dr_cost = sum(kwh_dr_cost),
                             mtr_cost = sum(mtr_cost),
                             before_cost = sum(before_cost), after_cost = sum(after_cost))
