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
# library("dtplyr")
library("ggplot2")
library("ggrepel")
library("tidyr")
sampleDF <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]

#used for generating aggregate statistics from:

## NSRDB 2005-2014 Hourly Data
get_nyc_solar = function(type = "read") {
  if (type == "new" | type == "write") {
    nsrdb_files = list.files("inputs\\nsrdb_raw", pattern = "*.csv", full.names = TRUE)
    nsrdb_df = rbindlist(lapply(nsrdb_files, fread))
    colnames(nsrdb_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
                           "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")
    nsrdb_df = write.csv(nsrdb_df, "inputs\\solar_nsrdb.csv")
  
    nsrdb_df = read.csv("inputs\\solar_nsrdb.csv") %>%
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
  
    sun_hrs.daily = group_by(nsrdb_df, day_ind, sun_hrs) %>%
      tally() %>%
      mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
      select(-(sun_hrs:n)) %>%
      filter(sun_hrs.daily > 0)
    kt_sum = group_by(nsrdb_df, day_ind) %>%
      summarise(kt.sum = sum(kt))
    kt_diff = group_by(nsrdb_df, day_ind) %>%
      mutate(temp_diff = abs(kt - lag(kt, default = 0))) %>%
      summarise(kt.diff = sum(temp_diff))
    cols_to_add = Reduce(left_join, list(kt_sum, kt_diff, sun_hrs.daily)) %>%
      mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
             kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
             weather = ifelse((kt.bar+kt.til)<0.6, "Overcast",
                              ifelse((0.8*kt.bar-kt.til)>=0.72, "Cloudless",
                                     "Some clouds")))
    nsrdb_df = left_join(cols_to_add, nsrdb_df)
    if (type == "write") {
      temp_file_name = paste0("inputs\\solar_nsrdb_slim_",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                              ".csv")
      write.csv(nsrdb_df, temp_file_name)
    }
  }
  if(type == "read") {
    nsrdb_df.slim = read.csv("inputs\\solar_nsrdb_slim.csv")  %>% 
      select(-X) %>%
      mutate(date_time = as.POSIXct(date_time)) %>%
      mutate_if(is.factor, as.character)
  }
  return(nsrdb_df.slim)
}
nsrdb_df.nyc = get_nyc_solar("read") %>%
                  filter(strftime(date_time, format = "%Y") == "2014") %>% 
                  mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
                         dayhr_ind = day_ind + 
                           as.numeric(strftime(date_time, format = "%H"))/24) %>%
                  select(-(kt.sum:kt.til),-(dhi:dni),-(clr_dhi:clr_dni),
                         -sun_hrs)
                  
## BSRN 2014 1-min Data
{
bsrn_dt = data.frame(seq.POSIXt(as.POSIXct("2014-01-01 00:01"),
                                as.POSIXct("2014-12-31 23:59"),
                                by = "1 min"))
names(bsrn_dt) <- "date_time"

bsrn_dt = bsrn_dt %>%
  mutate(index = strftime(date_time, format = "%Y-%m-%d"),
         day = as.numeric(strftime(date_time, format = "%d")),
         day_ind = as.numeric(strftime(date_time, format = "%j")),
         dayhr_ind = day_ind + as.numeric(strftime(date_time, format = "%H"))/24,
         mo = as.numeric(strftime(date_time, format = "%m"))) %>%
  filter((mo == 01 & !(day > 8 & day < 14)) |
           (mo == 06 & !(day > 15 & day < 19) & !(day > 20 & day < 23)) |
            (mo == 10 & !((day > 16 & day < 22) | (day == 14))) |
           (day_ind == 111 | (day_ind > 113 & day_ind < 126))) %>%
  select(-day,-mo)
sample_dt = unique(bsrn_dt$index) # LARC data is limiting factor for days in selected months
}

# Combining LARC files, saved as bsrn_df.larc
get_bsrn.larc = function(type = "read") {
    if (type == "new" | type == "write") {
      readUrl <- function(url, skip = 3) {
        out <- tryCatch(
          {
            # Just to highlight: if you want to use more than one
            # R expression in the "try" part then you'll have to
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression
            # in case the "try" part was completed successfully
  
            start_row = 1 + skip
            end_row = (24*60)+start_row
            readLines(con=url, warn=FALSE)[start_row:end_row]
            # The return value of `readLines()` is the actual value
            # that will be returned in case there is no condition
            # (e.g. warning or error).
            # You don't need to state the return value via `return()` as code
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
          },
          error=function(cond) {
            message(paste("URL does not seem to exist:", url))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
          },
          warning=function(cond) {
            message(paste("URL caused a warning:", url))
            # message("Here's the original warning message:")
            message(cond)
            # Choose a return value in case of warning
            return(-99)
          },
          finally={
            # NOTE:
            # Here goes everything that should be executed at the end,
            # regardless of success or error.
            # If you want more than one expression to be executed, then you
            # need to wrap them in curly brackets ({...}); otherwise you could
            # just have written 'finally=<expression>'
            # message(paste("Processed URL:", url))
            # message("Some other message at the end")
          }
        )
        if(is.character(out)) { # basically, if no errors get thrown
  
          scrapeDF <- function(timestep) {
            bsrn_df.row = as.data.frame.list(timestep) %>%
              apply(2, function(x) {
                if(x == "---") return(0)
                else return(x)})
            bsrn_df.row = as.data.frame.list(bsrn_df.row, stringsAsFactors = FALSE)
            bsrn_df.row = bsrn_df.row[,!apply(bsrn_df.row, 2, function(x) any(x==""))] %>%
              select(1:3,20)
            colnames(bsrn_df.row) <- c("date", "time", "tempC", "ghi")
  
            bsrn_df.row = bsrn_df.row %>%
              mutate_if(is.factor, as.character) %>%
              mutate(tempC = as.numeric(tempC),
                     ghi = as.numeric(ghi),
                     date_time = paste(strftime(strptime(date, format = "%m/%d/%y"),
                                                           format = "%Y-%m-%d"),
                                                  time),
                     day_ind = as.numeric(strftime(date_time, format = "%j"))) %>%
              select(-(date:time))
            flog.info(paste(bsrn_df.row$date_time))
  
            return(bsrn_df.row)
          }
          out <- out[!is.na(out)]
          out <- strsplit(out, " ")
          out <- bind_rows(lapply(out, scrapeDF))
        }
        return(out)
      }
      url_paths.larc = lapply(
        sample_dt,
        function(i) paste("http://capable.larc.nasa.gov/weatherlink/data/2014/",
        i, "data.txt", sep = "")
        )
      # bsrn_df.larc.days = bind_rows(lapply(url_paths.larc, function(x) readUrl(x, skip = 3)))
  
      cl <- makeCluster(3)
      registerDoSNOW(cl)
  
      pkgs_to_pass = c("dplyr", "futile.logger")
      bsrn_df.larc.days = foreach(i = 1:(length(url_paths.larc)),
                                  .combine = "rbind.data.frame",
                                  .multicombine = TRUE,
                                  .packages = pkgs_to_pass,
                                  .errorhandling = "remove",
                                  .verbose = TRUE) %dopar% {
                                    readUrl(url_paths.larc[[i]])
                                  }
      stopCluster(cl)
  
      bsrn_df.larc.days = bsrn_df.larc.days %>%
                              mutate(date_time = as.POSIXct(date_time),
                                     hr = as.numeric(strftime(date_time, format = "%H")),
                                     dayhr_ind = day_ind + hr/24) %>%
                              filter(!is.na(date_time)) %>%
                              arrange(dayhr_ind, date_time) %>%
                              select(-tempC,-hr) %>%
                              group_by(dayhr_ind, date_time) %>%
                              summarise_if(is.numeric, "mean") %>%
                              mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
  
      tally.compare <- left_join(tally(group_by(bsrn_dt, day_ind)),
                                 tally(group_by(bsrn_df.larc.days, day_ind)),
                                 by = "day_ind")
      if (type == "write") {
        temp_file_name = paste0("inputs\\solar_bsrn_larc_",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                             ".csv")
        write.csv(bsrn_df.larc.days, temp_file_name)
      }
  }
  if (type == "read") {
    bsrn_df.larc.days = read.csv("inputs\\solar_bsrn_larc.csv") %>% 
      select(-X) %>%
      mutate_if(is.integer, as.numeric) %>%
      mutate(date_time = as.POSIXct(date_time))
  }
  return(bsrn_df.larc.days)
}
# Combining COVE dat file, savd as bsrn_df.
get_bsrn.cove = function(type = "read") {
  if (type == "new" | type == "write") {
    bsrn_df.cove.days = readLines("inputs\\bsrn_raw\\2014001-2014365_COVEdata.DAT")
    # cove.cols = strsplit(bsrn_df.cove.days[14], "\"")
    # cove.cols = cove.cols[[1]][sapply(cove.cols, function(x) !(x == ","))][2:8]
    cove.cols <- c("day", "yr_day", "time", "zenith", "azi", "tempC", "ghi")
    bsrn_df.cove.days = strsplit(bsrn_df.cove.days[15:length(bsrn_df.cove.days)], " ")
    bsrn_df.cove.days = rbindlist(lapply(bsrn_df.cove.days,
                                 function(x) as.data.frame.list(x, col.names = cove.cols))) %>%
                  mutate_if(is.factor, as.character) %>%
                  mutate_if(is.character, as.numeric) %>%
                  mutate(min = ceiling(time*60)%%60,
                         hr = floor(time),
                         day = floor(day),
                         yr = as.numeric(substr(yr_day,1,4)),
                         date_time = as.POSIXct(strptime(paste(as.character(strptime(paste(day, yr),
                                                                 format = "%j %Y")),
                                                paste0(hr,":",min)), format = "%Y-%m-%d %H:%M")),
                         dayhr_ind = floor(day) + floor(time)/24) %>%
                  select(-(day:tempC),-(min:yr))
    bsrn_df.cove.days = mutate(bsrn_df.cove.days, ghi = case_when((bsrn_df.cove.days$ghi < 3 & bsrn_df.cove.days$ghi > -3) ~ 0,
                                          bsrn_df.cove.days$ghi != -999 ~ bsrn_df.cove.days$ghi)) %>%
                  fill(ghi, .direction = "up") %>%
                  fill(ghi) %>%
                  mutate(index = strftime(date_time, format = "%Y-%m-%d"),
                         day_ind = as.numeric(strftime(date_time, format = "%j")))

    bsrn_df.cove.days = left_join(data.frame(index = sample_dt, stringsAsFactors = FALSE), bsrn_df.cove.days) %>%
                    select(-index) %>%
                    mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
    bsrn_df.cove.days = bsrn_df.cove.days[2:nrow(bsrn_df.cove.days),]
    if (type == "write") {
      temp_file_name = paste0("inputs\\solar_bsrn_cove_",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                              ".csv")
      write.csv(bsrn_df.cove.days, temp_file_name)
    }
  }
  if (type == "read") {
    bsrn_df.cove.days = read.csv("inputs\\solar_bsrn_cove.csv") %>% 
      select(-X) %>%
      mutate_if(is.integer, as.numeric) %>%
      mutate(date_time = as.POSIXct(date_time))
  }
  return(bsrn_df.cove.days)
}
# Adding clearsky, weather types to BSRN
get_bsrn_clearsky = function() {
  bsrn_df.clearsky = fread("inputs\\bsrn_raw\\1155277_37.01_-76.34_2014.csv")
  colnames(bsrn_df.clearsky) = c("year","mo","day","hr","min","ghi",
                                 "clr_ghi", "tempC")
  bsrn_df.clearsky = bsrn_df.clearsky %>%
    mutate(date_time = as.POSIXct(strptime(paste(year,"-",mo,"-",day," ",
                                                 hr,":",0, sep = ""),
                                           format = "%Y-%m-%d %H:%M")),
           day_ind = as.numeric(strftime(date_time, format = "%j")),
           dayhr_ind = day_ind + as.numeric(hr/24),
           dayhr_ind = round(dayhr_ind, 5)) %>%
    filter(!is.na(date_time)) %>%
    arrange(dayhr_ind, date_time) %>%
    select(-(year:ghi),-tempC) %>%
    group_by(dayhr_ind, date_time) %>%
    summarise_if(is.numeric, "mean") %>%
    select(-date_time)
  bsrn_df.clearsky = left_join(data.frame(day_ind = unique(bsrn_dt$day_ind),
                                          stringsAsFactors = FALSE),
                               bsrn_df.clearsky)
  
  return(bsrn_df.clearsky)
}
add_weather = function(bsrn_df.station) {
  
  bsrn_df.clearsky = get_bsrn_clearsky()
  bsrn_df.station = bsrn_df.station %>%
    mutate(dayhr_ind = round(dayhr_ind, 5)) %>%
    left_join(bsrn_df.clearsky) %>%
    mutate(kt = ifelse(clr_ghi == 0, 0, ghi/clr_ghi),
           kt = ifelse(kt > 2, 2, kt))
  sun_hrs.daily = group_by(bsrn_df.station, day_ind, sun_hrs) %>%
    tally() %>%
    mutate(sun_hrs.daily = ifelse(sun_hrs < 1, 0, as.numeric(n))) %>%
    group_by(day_ind) %>%
    slice(which.max(sun_hrs.daily)) %>%
    select(-(sun_hrs:n))
  kt_sum = group_by(bsrn_df.station, day_ind) %>%
    summarise(kt.sum = sum(kt))
  kt_diff = group_by(bsrn_df.station, day_ind) %>%
    mutate(temp_diff = abs(kt - lag(kt, default = 0))) %>%
    summarise(kt.diff = sum(temp_diff))
  cols_to_add = Reduce(left_join, list(kt_sum, kt_diff, sun_hrs.daily)) %>%
    mutate(kt.bar = ifelse(sun_hrs.daily == 0, 0, kt.sum / sun_hrs.daily),
           kt.til = ifelse(sun_hrs.daily == 0, 0, kt.diff / sun_hrs.daily),
           weather = ifelse((kt.bar+kt.til)<0.6, "Overcast",
                            ifelse((kt.bar-kt.til)>=0.72, "Cloudless",
                                   "Some clouds")))
  bsrn_df.station = left_join(cols_to_add, bsrn_df.station)
}
get_markovchains = function(df_str) {
  
  if (df_str == "cove") {
    df = get_bsrn.cove("read") 
  }
  if (df_str == "larc") {
    df = get_bsrn.larc("read")
  }
  condns <- unique(df$weather)
  chains = foreach(j = 1:(length(condns))) %do% {
                  library("markovchain")
                  condn.0 <- df %>%
                    filter(weather == condns[j]) %>%
                    mutate(kt = round(kt, 2),
                           kt2 = lead(kt)) %>%
                    fill(kt2) %>%
                    select(date_time, kt:kt2) %>%
                    filter(kt >= 0 & kt2 != 0 & kt2 <= 1)
                  mCfit.0 <- markovchainFit(condn.0[["kt"]], byrow = FALSE,
                                            possibleStates = seq(0,2,0.01))
                  return(mCfit.0)
  }
  names(chains) <- condns
  return(chains)
}
get_chain_1hr = function(t0.val, tpm) {
  if (t0.val == 0) {
    daychain <- rep(0, 60)
  }
  else {
    daychain <- as.numeric(markovchainSequence(
                                             59,
                                             tpm$estimate,
                                             t0 = round(t0.val, 2),
                                             include.t0 = TRUE))
  }
  return(daychain)
}
get_kt_1min = function(daily_df, src_df, interval) {
  # Attaches 1-min kt values to a time-series spanning 24hrs

  weather <- unique(daily_df$weather)
  # t0.vals <- (daily_df$kt)
  t0.vals <- rep(mean(filter(daily_df, kt > 0)$kt), nrow(daily_df)) 
  tpm <- get_markovchains(df_str = src_df)
  scale_factor <- 0.5
  while (abs(scale_factor) >= 0.5) {
    kt.1min <- data.frame(kt_1min = unlist(lapply(t0.vals,
                                                  function(i) {
                                                    get_chain_1hr(i,
                                                    tpm[[weather]])})
                               ))
    
    minute_df <- data.frame(date_time = seq.POSIXt(daily_df$date_time[1],
                                            length.out = 24*60,
                                            by = 60))
#     if (nrow(minute_df) < 1440) {
#       missing_dayind <- unique(daily_df$day_ind)
#       missing_day <- data.frame(date_time = seq.POSIXt(strptime(paste("2014",
#                                                                       missing_dayind),
#                                                                 format = "%Y %j"),
#                                                       length.out = 60,
#                                                       by = 60))
#       minute_df <- left_join(minute_df, missing_day, by = "date_time")
#     }
    
    minute_df <- minute_df %>%
                  mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
                         dayhr_ind = day_ind +
                         as.numeric(strftime(date_time, format = "%H"))/24) %>%
                  left_join(daily_df, by = c("day_ind", "dayhr_ind")) %>%
                  cbind.data.frame(kt.1min) %>%
                  mutate(kt_1min = ifelse(kt_1min > 2*max(daily_df$clr_ghi),
                                            2*mean(daily_df$kt),
                                            kt_1min),
                         ghi_1min = clr_ghi*kt_1min,
                         date_time = date_time.x) %>%
                  select(-date_time.y,-date_time.x)
    
    check_daily_irr <- summarize(group_by(minute_df, dayhr_ind),
                                 ghi.0 = mean(ghi),
                                 ghi.mC = mean(ghi_1min)) %>%
                        summarize_if(is.numeric, sum)
    scale_factor <- (check_daily_irr$ghi.0 / check_daily_irr$ghi.mC) - 1
    message(paste(scale_factor, "-",
                  unique(daily_df$day_ind), "-",
                  weather, "-",
                  nrow(minute_df)))
  }
  scale_factor <- ifelse(abs(scale_factor) > 0.05, scale_factor + 1, 1)
  minute_df <- mutate(minute_df, scale_factor = scale_factor,
                                 kt_1min.scl = kt_1min*scale_factor,
                                 ghi_1min.scl = ghi_1min*scale_factor,
                                 ghi.var = check_daily_irr$ghi.0,
                                 ghi_mC.var = check_daily_irr$ghi.mC)
   
  return(minute_df)
}
get_1yr_markov = function(src_df, interval, seed = NULL) {
  if (!is.null(seed)) seed = 7
  all_1min = foreach(j = 1:365,
          .combine = "bind_rows",
          .errorhandling = "remove") %do% {
              
              day_1min.0 = nsrdb_df.nyc %>%
                            filter(day_ind == j) %>%  
                            get_kt_1min(src_df, interval)
              
    return(day_1min.0)
  }
  
  return(all_1min)
}
validate_markov_df <- function(df) {
  bsrn_cove <- get_bsrn.cove("read") %>%
                  select(date_time,ghi)
  bsrn_larc <- get_bsrn.larc("read") %>%
                  select(date_time, ghi)
  
  bsrn_df <- full_join(bsrn_cove, bsrn_larc, by = "date_time",
                       suffix = c("_cove", "_larc")) %>%
                mutate(ghi_cove.diff = abs(ghi_cove - lag(ghi_cove)),
                       ghi_larc.diff = abs(ghi_larc - lag(ghi_larc)))
  bsrn_days <- unique(bsrn_df$day_ind)
  
  markov <- df %>%
              filter(day_ind %in% bsrn_days) %>%
              select(date_time, ghi_1min.scl) %>%
              mutate(ghi_1min.diff = abs(ghi_1min.scl - lag(ghi_1min.scl))) %>%
              select(date_time, ghi_1min.diff)
  
  output_df <- left_join(markov, bsrn_df, by = "date_time") %>%
                  fill(ghi_1min.diff:ghi_larc.diff, .direction = "up") %>%
                  group_by(date_time) %>%
                  summarize_if(is.numeric, mean)
  
  return(output_df)
}
test <- get_1yr_markov(src_df = "larc", interval = 1/12)
freq_test <- validate_markov_df(test)

# ggplot(data = test) + 
#   geom_line(aes(date_time, ghi)) +
#   geom_line(aes(date_time, ghi_1min), colour = "red") +
#   geom_line(aes(date_time, ghi_1min.scl), colour = "green")

ggplot(data = freq_test) + 
  geom_freqpoly(aes(ghi_1min.diff), binwidth = 50, colour = "red") + 
  geom_freqpoly(aes(ghi_1min.diff), binwidth = 50, colour = "blue") + 
  geom_freqpoly(aes(ghi_1min.diff), binwidth = 50, colour = "blue4") +
  scale_x_continuous(limits = c(0,max(freq_test$ghi_1min.diff))) +
  scale_y_log10()

check_daily_irr <- summarise(group_by(test, day_ind),
                             ghi_0 = mean(ghi),
                             ghi_mC = mean(ghi_1min))

# want similar plot to this but with variance on y axis
ggplot(data = check_daily_irr, mapping = aes(x = day_ind)) +
  geom_line(aes(y = ghi_0), colour = "red") +
  geom_line(aes(y = ghi_mC), colour = "blue")

mean_variance <- list("cove" = sum(freq_test$ghi_cove.diff) / 1440,
                      "larc" = sum(freq_test$ghi_larc.diff) / 1440,
                      "markov" = sum(freq_test$ghi_1min.diff) / 1440)

ggplot(data = freq_test, mapping = aes(x = day_ind)) +
  geom_line(aes(y = ghi_cove.diff), colour = "blue") +
  geom_line(aes(y = ghi_larc.diff), colour = "blue4") +
  geom_line(aes(y = ghi_1min.diff), colour = "blue4") +
  

mean_variance
