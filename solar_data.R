# Solar Resource Characterization

# Contains full set of solar data from NSRDB
# Calculates transition matrix for Markov Chains - NOT DONE

library("data.table")
library("dplyr")
library("ggplot2")
library("ggrepel")
library("tidyr")
sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]

#used for generating aggregate statistics from 2005-2014 solar data
{
# files = list.files("inputs\\solar_raw", pattern = "*.csv", full.names = TRUE)
# solar_df = rbindlist(lapply(files, fread))
# colnames(solar_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
#                        "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")
# solar_df = write.csv(solar_df, "inputs\\solar_nsrdb.csv")
  
# solar_df = read.csv("inputs\\solar_nsrdb.csv") %>%
#   mutate(date_time = as.POSIXct(strptime(paste(year,"-",mo,"-",day," ",
#                                                hr,":",0, sep = ""),
#                                          format = "%Y-%m-%d %H:%M")),
#          day_ind = as.numeric(strftime(date_time, format = "%j")) +
#                     (year-min(year))*365,
#          dayhr_ind = day_ind + as.numeric(hr/24),
#          kt = ifelse(clr_ghi == 0, 0, ghi/clr_ghi)) %>%
#   filter(!is.na(date_time)) %>%
#   arrange(dayhr_ind, date_time) %>%
#   select(-X,-(year:min)) %>%
#   group_by(dayhr_ind, date_time) %>%
#   summarise_if(is.numeric, "mean") %>%
#   mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
# 
# sun_hrs.daily = group_by(solar_df, day_ind, sun_hrs) %>%
#   tally() %>%
#   mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
#   select(-(sun_hrs:n)) %>%
#   filter(sun_hrs.daily > 0)
# kt_sum = group_by(solar_df, day_ind) %>%
#   summarise(kt.sum = sum(kt))
# kt_diff = group_by(solar_df, day_ind) %>%
#   mutate(temp_diff = abs(kt - lag(kt, default = 0))) %>%
#   summarise(kt.diff = sum(temp_diff))
# cols_to_add = Reduce(left_join, list(kt_sum, kt_diff, sun_hrs.daily)) %>%
#   mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
#          kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
#          weather = ifelse((kt.bar+kt.til)<0.6, "Overcast",
#                           ifelse((0.8*kt.bar-kt.til)>=0.72, "Cloudless",
#                                  "Some clouds")))
# solar_df = left_join(cols_to_add, solar_df)
# solar_df = write.csv(solar_df, "inputs\\solar_nsrdb_slim.csv")
}

solar_df = read.csv("inputs\\solar_nsrdb_slim.csv") %>% select(-X)
sample_days = sample_n(group_by(solar_df, weather), 1)

# need to test frequency of irradiance before doing markov stuff