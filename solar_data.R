# Solar Resource Characterization

# Contains full set of solar data from NSRDB
# Calculates transition matrix for Markov Chains - NOT DONE

library("data.table")
library("dplyr")

sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
files = list.files("inputs\\solar", pattern = "*.csv", full.names = TRUE)
solar_df = rbindlist(lapply(files, fread))
colnames(solar_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
                       "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")


solar_df.sample = sample.df(solar_df, 10000) %>% 
                    mutate(date_time = as.POSIXct(strptime(paste(year,"-",mo,"-",day," ",
                                                                  hr,":",min, sep = ""),
                                                            format = "%Y-%m-%d %H:%M")),
                           slim_dt = strftime(date_time, format = "%m-%d %H:%M")) %>%
                    select(-(year:min)) %>% arrange(slim_dt) %>% group_by(slim_dt) %>%
                    summarise_if(is.numeric, c("mean", "sd"))

