# Plot animations

# All animated plots based on custom objects
# are created here. Objects are imported as needed

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("futile.logger")
library("ggplot2")
src_list = list.files(pattern = "*load.R", full.names = TRUE)
# "bldg_load", "grid_load", "pv_load"
source(src_list[1])

test_bldg <- get_test_bldg()

get_ani_bldg <- function(bldg) {
  base_ts <- bldg$get_base_ts()
  
  for (i in seq.int(1,bldg$get_ts_count())) {
    print(i)
  }
  ts_df1 <- as.data.frame(bldg$get_ts_df()[2])[2]
  base_ts <- cbind(base_ts, ts_df1$kwh)
  colnames(base_ts) <- c("date_time", "base_kwh", "rand1_kwh")
  base_ts$month <- format(base_ts$date_time, "%m")
  jul_ts <- subset(base_ts, base_ts$month == "07")[1:2016,]
  return(plot(jul_ts$date_time, jul_ts$base_kwh, type = "l"))
  # points(jul_ts$date_time, jul_ts$rand1_kwh, col = "gray55")
}

