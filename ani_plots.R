# Plot animations

# All animated plots based on custom objects
# are created here. Objects are imported as needed

rm(list=ls())
# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
setwd("E:\\GitHub\\clca-batt")
library("futile.logger")
library("gganimate")
library("ggplot2")
src_list = list.files(pattern = "*load.R", full.names = TRUE)
# "bldg_load", "grid_load", "pv_load"
source(src_list[1])

test_bldg <- get_test_bldg()

get_ani_bldg <- function(bldg) {
  full_df <- bldg$get_base_ts()
  full_df$run<- as.character(1)
  full_df$week <- format(full_df$date_time, "%U")
  
  for (i in seq.int(2,bldg$get_ts_count())) {
    # kwh <- as.vector(bldg$get_ts_df(i))$kwh
    rand_kwh <- as.data.frame(bldg$get_ts_df(i))
    rand_kwh$run <- as.character(i)
    rand_kwh$week <- format(rand_kwh$date_time, "%U")
    full_df <- rbind(full_df, rand_kwh)
  }
  
  jul_df <- subset(full_df, full_df$week == "29") # mid-July, when elec demand peaks
  jul_plt <- ggplot(data = jul_df, aes(date_time, kwh)) +
                    geom_point() +
                    geom_line(data = subset(jul_df, jul_df$run == 1),
                                mapping = aes(frame = run),
                                colour = "white", size = 1) +
                    geom_point(data = subset(jul_df, jul_df$run == 1),
                                mapping = aes(frame = run),
                                colour = "#00CC66", size = 1.5) +
                    labs(x = "",
                         y = "kWh",
                         title = "Medium Office Elec. (July)") +
                    theme(panel.background = element_rect(colour = "gray75", fill = "gray80")) +
                    theme(panel.grid.major = element_line(colour = "gray85")) +
                    theme(panel.grid.minor = element_line(colour = "gray85"))
  ani_jul_plt <- gg_animate(jul_plt)
  
  return(ani_jul_plt)
}
get_ani_bldg(test_bldg)
