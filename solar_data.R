# Solar Resource Characterization

# Contains full set of solar data from NSRDB
# Calculates transition matrix for Markov Chains - NOT DONE

library("data.table")
library('foreach')
library('iterators')
library('doSNOW')
library("futile.logger")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggrepel")
library("tidyr")
sampleDF <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]

#used for generating aggregate statistics from:

## NSRDB 2005-2014 Hourly Data
get_nyc_solar = function(overwrite = FALSE) {
  # nsrdb_files = list.files("inputs\\nsrdb_raw", pattern = "*.csv", full.names = TRUE)
  # nsrdb_df = rbindlist(lapply(nsrdb_files, fread))
  # colnames(nsrdb_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
  #                        "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")
  # nsrdb_df = write.csv(nsrdb_df, "inputs\\solar_nsrdb.csv")
    
  # nsrdb_df = read.csv("inputs\\solar_nsrdb.csv") %>%
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
  # sun_hrs.daily = group_by(nsrdb_df, day_ind, sun_hrs) %>%
  #   tally() %>%
  #   mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
  #   select(-(sun_hrs:n)) %>%
  #   filter(sun_hrs.daily > 0)
  # kt_sum = group_by(nsrdb_df, day_ind) %>%
  #   summarise(kt.sum = sum(kt))
  # kt_diff = group_by(nsrdb_df, day_ind) %>%
  #   mutate(temp_diff = abs(kt - lag(kt, default = 0))) %>%
  #   summarise(kt.diff = sum(temp_diff))
  # cols_to_add = Reduce(left_join, list(kt_sum, kt_diff, sun_hrs.daily)) %>%
  #   mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
  #          kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
  #          weather = ifelse((kt.bar+kt.til)<0.6, "Overcast",
  #                           ifelse((0.8*kt.bar-kt.til)>=0.72, "Cloudless",
  #                                  "Some clouds")))
  # nsrdb_df = left_join(cols_to_add, nsrdb_df)
  # write.csv(nsrdb_df, "inputs\\solar_nsrdb_slim.csv")
  if(!overwrite) {
    nsrdb_df.slim = read.csv("inputs\\solar_nsrdb_slim.csv")  %>% 
    select(-X) %>%
    mutate(date_time = as.POSIXct(date_time)) %>%
    mutate_if(is.factor, as.character)
  }
  else {
    nsrdb_df.slim = 0
  }
  return(nsrdb_df.slim)
}

## BSRN 2014 1-min Data
bsrn_dt = data.frame(seq.POSIXt(as.POSIXct("2014-01-01 00:01"),
                         as.POSIXct("2014-12-31 23:59"),
                         by = "1 min"))
names(bsrn_dt) <- "date_time"

bsrn_dt = bsrn_dt %>%
            mutate(index = strftime(date_time, format = "%Y-%m-%d"),
                   day = as.numeric(strftime(date_time, format = "%d")),
                   day_ind = as.numeric(strftime(date_time, format = "%j")),
                   mo = as.numeric(strftime(date_time, format = "%m"))) %>%
            filter((mo == 01 & !(day > 8 & day < 14)) |
                   (mo == 06 & !(day > 15 & day < 19) & !(day > 20 & day < 23)) |
                   (mo == 10 & !((day > 16 & day < 22) | (day == 14))) |
                   (day_ind == 111 | (day_ind > 113 & day_ind < 126))) %>%
            select(-day,-mo)
sample_dt = unique(bsrn_dt$index) # LARC data is limiting factor for days in selected months

# Combining LARC files, saved as bsrn_df.larc
get_bsrn.larc = function(overwrite = FALSE) {
#   readUrl <- function(url, skip = 3) {
#     out <- tryCatch(
#       {
#         # Just to highlight: if you want to use more than one
#         # R expression in the "try" part then you'll have to
#         # use curly brackets.
#         # 'tryCatch()' will return the last evaluated expression
#         # in case the "try" part was completed successfully
# 
#         start_row = 1 + skip
#         end_row = (24*60)+start_row
#         readLines(con=url, warn=FALSE)[start_row:end_row]
#         # The return value of `readLines()` is the actual value
#         # that will be returned in case there is no condition
#         # (e.g. warning or error).
#         # You don't need to state the return value via `return()` as code
#         # in the "try" part is not wrapped insided a function (unlike that
#         # for the condition handlers for warnings and error below)
#       },
#       error=function(cond) {
#         message(paste("URL does not seem to exist:", url))
#         message("Here's the original error message:")
#         message(cond)
#         # Choose a return value in case of error
#         return(NA)
#       },
#       warning=function(cond) {
#         message(paste("URL caused a warning:", url))
#         # message("Here's the original warning message:")
#         message(cond)
#         # Choose a return value in case of warning
#         return(-99)
#       },
#       finally={
#         # NOTE:
#         # Here goes everything that should be executed at the end,
#         # regardless of success or error.
#         # If you want more than one expression to be executed, then you
#         # need to wrap them in curly brackets ({...}); otherwise you could
#         # just have written 'finally=<expression>'
#         # message(paste("Processed URL:", url))
#         # message("Some other message at the end")
#       }
#     )
#     if(is.character(out)) { # basically, if no errors get thrown
# 
#       scrapeDF <- function(timestep) {
#         bsrn_df.row = as.data.frame.list(timestep) %>%
#           apply(2, function(x) {
#             if(x == "---") return(0)
#             else return(x)})
#         bsrn_df.row = as.data.frame.list(bsrn_df.row, stringsAsFactors = FALSE)
#         bsrn_df.row = bsrn_df.row[,!apply(bsrn_df.row, 2, function(x) any(x==""))] %>%
#           select(1:3,20)
#         colnames(bsrn_df.row) <- c("date", "time", "tempC", "ghi")
# 
#         bsrn_df.row = bsrn_df.row %>%
#           mutate_if(is.factor, as.character) %>%
#           mutate(tempC = as.numeric(tempC),
#                  ghi = as.numeric(ghi),
#                  date_time = paste(strftime(strptime(date, format = "%m/%d/%y"),
#                                                        format = "%Y-%m-%d"),
#                                               time),
#                  day_ind = as.numeric(strftime(date_time, format = "%j"))) %>%
#           select(-(date:time))
#         flog.info(paste(bsrn_df.row$date_time))
# 
#         return(bsrn_df.row)
#       }
#       out <- out[!is.na(out)]
#       out <- strsplit(out, " ")
#       out <- bind_rows(lapply(out, scrapeDF))
#     }
#     return(out)
#   }
#   url_paths.larc = lapply(
#     sample_dt,
#     function(i) paste("http://capable.larc.nasa.gov/weatherlink/data/2014/",
#     i, "data.txt", sep = "")
#     )
#   # bsrn_df.larc.days = bind_rows(lapply(url_paths.larc, function(x) readUrl(x, skip = 3)))
# 
#   cl <- makeCluster(3)
#   registerDoSNOW(cl)
# 
#   pkgs_to_pass = c("dplyr", "futile.logger")
#   bsrn_df.larc.days = foreach(i = 1:(length(url_paths.larc)),
#                               .combine = "rbind.data.frame",
#                               .multicombine = TRUE,
#                               .packages = pkgs_to_pass,
#                               .errorhandling = "remove",
#                               .verbose = TRUE) %dopar% {
#                                 readUrl(url_paths.larc[[i]])
#                               }
#   stopCluster(cl)
# 
#   bsrn_df.larc.days = bsrn_df.larc.days %>%
#                           mutate(date_time = as.POSIXct(date_time),
#                                  hr = as.numeric(strftime(date_time, format = "%H")),
#                                  dayhr_ind = day_ind + hr/24) %>%
#                           filter(!is.na(date_time)) %>%
#                           arrange(dayhr_ind, date_time) %>%
#                           select(-tempC,-hr) %>%
#                           group_by(dayhr_ind, date_time) %>%
#                           summarise_if(is.numeric, "mean") %>%
#                           mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
# 
#   tally.compare <- left_join(tally(group_by(bsrn_dt, day_ind)),
#                              tally(group_by(bsrn_df.larc.days, day_ind)),
#                              by = "day_ind")
#   write.csv(bsrn_df.larc.days, "inputs\\solar_bsrn_larc.csv")
  if(!overwrite) {
    bsrn_df.larc.days = read.csv("inputs\\solar_bsrn_larc.csv") %>% 
              select(-X) %>%
              mutate_if(is.integer, as.numeric) %>%
              mutate_if(is.factor, as.POSIXct)
  }
  else {
    bsrn_df.larc.days = 0
  }
  return(bsrn_df.larc.days)
}
# Combining COVE dat file, savd as bsrn_df.
get_bsrn.cove = function(overwrite = FALSE) {
#   bsrn_df.cove.days = readLines("inputs\\bsrn_raw\\2014001-2014365_COVEdata.DAT")
#   # cove.cols = strsplit(bsrn_df.cove.days[14], "\"")
#   # cove.cols = cove.cols[[1]][sapply(cove.cols, function(x) !(x == ","))][2:8]
#   cove.cols <- c("day", "yr_day", "time", "zenith", "azi", "tempC", "ghi")
#   bsrn_df.cove.days = strsplit(bsrn_df.cove.days[15:length(bsrn_df.cove.days)], " ")
#   bsrn_df.cove.days = rbindlist(lapply(bsrn_df.cove.days,
#                                function(x) as.data.frame.list(x, col.names = cove.cols))) %>%
#                 mutate_if(is.factor, as.character) %>%
#                 mutate_if(is.character, as.numeric) %>%
#                 mutate(min = ceiling(time*60)%%60,
#                        hr = floor(time),
#                        day = floor(day),
#                        yr = as.numeric(substr(yr_day,1,4)),
#                        date_time = as.POSIXct(strptime(paste(as.character(strptime(paste(day, yr),
#                                                                format = "%j %Y")),
#                                               paste0(hr,":",min)), format = "%Y-%m-%d %H:%M")),
#                        dayhr_ind = floor(day) + floor(time)/24) %>%
#                 select(-(day:tempC),-(min:yr))
#   bsrn_df.cove.days = mutate(bsrn_df.cove.days, ghi = case_when((bsrn_df.cove.days$ghi < 3 & bsrn_df.cove.days$ghi > -3) ~ 0,
#                                         bsrn_df.cove.days$ghi != -999 ~ bsrn_df.cove.days$ghi)) %>%
#                 fill(ghi, .direction = "up") %>%
#                 fill(ghi) %>%
#                 mutate(index = strftime(date_time, format = "%Y-%m-%d"),
#                        day_ind = as.numeric(strftime(date_time, format = "%j")))
#   
#   bsrn_df.cove.days = left_join(data.frame(index = sample_dt, stringsAsFactors = FALSE), bsrn_df.cove.days) %>%
#                   select(-index) %>%
#                   mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
#   bsrn_df.cove.days = bsrn_df.cove.days[2:nrow(bsrn_df.cove.days),]
  write.csv(bsrn_df.cove.days, "inputs\\solar_bsrn_cove.csv")
  if(!overwrite) {
    bsrn_df.cove.days = read.csv("inputs\\solar_bsrn_cove.csv") %>% 
                          select(-X) %>%
                          mutate_if(is.integer, as.numeric) %>%
                          mutate_if(is.factor, as.POSIXct)
  }
  else {
    bsrn_df.cove.days = 0
  }
  return(bsrn_df.cove.days)
}
# Adding clearsky, weather types to BSRN
add_weather = function(station, overwrite = FALSE) {
  bsrn_df.clearsky = fread("inputs\\bsrn_raw\\1155277_37.01_-76.34_2014.csv")
  colnames(bsrn_df.clearsky) = c("year","mo","day","hr","min","ghi",
                                 "clr_ghi", "tempC")
  bsrn_df.clearsky = bsrn_df.clearsky %>%
    mutate(date_time = as.POSIXct(strptime(paste(year,"-",mo,"-",day," ",
                                                 hr,":",0, sep = ""),
                                           format = "%Y-%m-%d %H:%M")),
           day_ind = as.numeric(strftime(date_time, format = "%j")),
           dayhr_ind = day_ind + as.numeric(hr/24)) %>%
    filter(!is.na(date_time)) %>%
    arrange(dayhr_ind, date_time) %>%
    select(-(year:ghi),-tempC) %>%
    group_by(dayhr_ind, date_time) %>%
    summarise_if(is.numeric, "mean") %>%
    select(-date_time)

  if(overwrite) {  
    if(station == "cove"){
      bsrn_df = get_bsrn.cove() %>%
        select(-clr_ghi,-(kt.sum:weather)) %>%
        mutate_if(is.integer, as.numeric)
      bsrn_df = left_join(bsrn_df, bsrn_df.clearsky, by = "dayhr_ind")
    }
    if(station == "larc"){
      bsrn_df = get_bsrn.larc() %>%
        select(-clr_ghi,-(kt.sum:weather)) %>%
        mutate_if(is.integer, as.numeric)
      bsrn_df = left_join(bsrn_df, bsrn_df.clearsky, by = "dayhr_ind")
    }
    if(station != "cove" & station != "larc") {
      return("Not a valid station")
    }
    bsrn_df = mutate(bsrn_df, kt = ifelse(clr_ghi == 0, 0, ghi/clr_ghi))
    sun_hrs.daily = group_by(bsrn_df, day_ind, sun_hrs) %>%
      tally() %>%
      mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
      select(-(sun_hrs:n)) %>%
      filter(sun_hrs.daily > 0)
    kt_sum = group_by(bsrn_df, day_ind) %>%
      summarise(kt.sum = sum(kt))
    kt_diff = group_by(bsrn_df, day_ind) %>%
      mutate(temp_diff = abs(kt - lag(kt, default = 0))) %>%
      summarise(kt.diff = sum(temp_diff))
    cols_to_add = Reduce(left_join, list(kt_sum, kt_diff, sun_hrs.daily)) %>%
      mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
             kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
             weather = ifelse((kt.bar+kt.til)<0.6, "Overcast",
                              ifelse((0.8*kt.bar-kt.til)>=0.72, "Cloudless",
                                     "Some clouds")))
    bsrn_df = left_join(cols_to_add, bsrn_df)
    write.csv(bsrn_df, paste0("inputs\\solar_bsrn_",station,".csv"))
  }
}

bsrn_df.cove = get_bsrn.cove()
bsrn_df.larc = get_bsrn.larc()
nsrdb_df.nyc = get_nyc_solar()

cbind(
      nsrdb_df.nyc %>% group_by(weather) %>% tally(),
      bsrn_df.cove %>% group_by(weather) %>% tally(),
      bsrn_df.larc %>% group_by(weather) %>% tally()
      )

# sample_days = sample_n(group_by(nsrdb_df, weather), 1)
# need to test frequency of irradiance before doing markov stuff