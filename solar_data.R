# Solar Resource Characterization

# Contains full set of solar data from NSRDB
# Calculates transition matrix for Markov Chains - NOT DONE

library("data.table")
library("dplyr")
library("ggplot2")
library("tidyr")
sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]

# files = list.files("inputs\\solar", pattern = "*.csv", full.names = TRUE)
# solar_df = rbindlist(lapply(files, fread))
# colnames(solar_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
#                        "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")
solar_df = read.csv("inputs\\solar_nsrdb.csv")

solar_df.sample = sample.df(solar_df, 100000) %>%
  # solar_df.sample = solar_df %>%
  mutate(date_time = as.POSIXct(strptime(paste(year,"-",mo,"-",day," ",
                                               hr,":",0, sep = ""),
                                         format = "%Y-%m-%d %H:%M")),
         day_ind = as.numeric(strftime(date_time, format = "%j")) +
                    (year-min(year))*365,
         dayhr_ind = day_ind + as.numeric(hr/24),
         kt = ifelse(clr_ghi == 0, 0, ghi/clr_ghi)) %>%
  filter(!is.na(date_time)) %>%
  arrange(dayhr_ind, date_time) %>%
  select(-X,-(year:min)) %>%
  group_by(dayhr_ind, date_time) %>%
  summarise_if(is.numeric, "mean") %>%
  mutate(sun_hrs = ifelse(ghi == 0, 0, 1))

sun_hrs.daily = group_by(solar_df.sample, day_ind, sun_hrs) %>%
  tally() %>%
  mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
  select(-(sun_hrs:n)) %>%
  filter(sun_hrs.daily > 0)
kt.sum = group_by(solar_df.sample, day_ind, dayhr_ind) %>%
  summarise(kt.sum = sum(kt))
kt.diff = mutate(solar_df.sample, kt.diff = abs(kt - lag(kt, default = 0))) %>%
  group_by(day_ind, dayhr_ind) %>%
  summarise(kt.diff = sum(kt.diff))
cols_to_add = Reduce(left_join, list(kt.sum, kt.diff, sun_hrs.daily)) %>%
  mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
         kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
         weather = ifelse((kt.bar+kt.til)<0.6, "overcast",
                          ifelse((0.8*kt.bar-kt.til)>=0.72, "cloudless",
                                 "some_clouds")))

solar_df.sample = left_join(cols_to_add, solar_df.sample)
  

ggplot(solar_df.sample, aes(x = kt.bar, kt.til, colour = weather)) + geom_point()
