# Solar Resource Characterization

# Contains full set of solar data from NSRDB
# Calculates transition matrix for Markov Chains
# which get used to generate multiple copies of
# subhourly solar generation time-series

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
get_nyc_solar = function(type = "read") {
  # can be called to read a compiled & summarized time-series
  # of hourly NYC solar generation
  
  # or to compile ("write") this time-series from
  # years worth of raw csvs of solar gen from NSRDB
  
  if (type == "new" | type == "write") {
    # THIS BLOCK IS ONLY NEEDED ONCE, TO COMPILE DIFFERENT CSV FILES
    # which for now have to be manually downloaded from NSRDB
    
    # nsrdb_files = list.files("inputs/nsrdb_raw", pattern = "*.csv", full.names = TRUE)
    # nsrdb_df = rbindlist(lapply(nsrdb_files, fread))
    # colnames(nsrdb_df) = c("year","mo","day","hr","min","dhi","dni","ghi",
    #                        "clr_dhi", "clr_dni", "clr_ghi", "tempC", "pressure")
    # nsrdb_df = write.csv(nsrdb_df, "inputs/solar_nsrdb.csv")
    
    # IF YOU HAVE ALREADY COMPILED THE RAW NSRDB CSVS, then run this chunk
    nsrdb_df = read.csv("inputs/solar_nsrdb.csv") %>%
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
      mutate(sun_hrs = ifelse(ghi == 0, 0, 1)) %>%
      ungroup() %>%
      add_weather()
    
    if (type == "write") {
      temp_file_name = paste0("inputs/solar_nsrdb_2014",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                              ".csv")
      write.csv(nsrdb_df, temp_file_name)
    }
  }
  if(type == "read") {
    nsrdb_df = read.csv("inputs/solar_nsrdb_2014.csv")  %>% 
      select(-X) %>%
      mutate(date_time = as.POSIXct(date_time)) %>%
      mutate_if(is.factor, as.character) %>%
      filter(strftime(date_time, format = "%Y") == "2014") %>% 
      mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
             dayhr_ind = day_ind + 
               as.numeric(strftime(date_time, format = "%H"))/24) %>%
      select(-(kt.sum:kt.til),-(dhi:dni),-(clr_dhi:clr_dni),
             -sun_hrs)
  }
  return(nsrdb_df)
}
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

get_bsrn.larc = function(type = "read") {
  # Combining LARC 1min solar data into a single csv ("write")
  # or reading the csv, if compilation already taken care of ("read")
  
  if (type == "new" | type == "write") {
    
    # the web scraping tool below is adapted from
    # http://pastebin.com/XX5FBJqf
    
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
                                              format = "%Y-%m-%d"), time),
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
      mutate(sun_hrs = ifelse(ghi == 0, 0, 1)) %>%
      ungroup() %>%
      add_weather()
    
    # tally.compare <- left_join(tally(group_by(bsrn_dt, day_ind)),
    #                            tally(group_by(bsrn_df.larc.days, day_ind)),
    #                            by = "day_ind")
    if (type == "write") {
      temp_file_name = paste0("inputs/solar_bsrn_larc_",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                              ".csv")
      write.csv(bsrn_df.larc.days, temp_file_name)
    }
  }
  if (type == "read") {
    bsrn_df.larc.days = read.csv("inputs/solar_bsrn_larc.csv") %>% 
      select(-X) %>%
      mutate_if(is.integer, as.numeric) %>%
      mutate(date_time = as.POSIXct(date_time))
  }
  return(bsrn_df.larc.days)
}
get_bsrn.cove = function(type = "read") {
  # Combining COVE 1min solar data into a single csv ("write")
  # or reading the csv, if compilation already taken care of ("read")
  
  if (type == "new" | type == "write") {
    bsrn_df.cove.days = readLines("inputs/bsrn_raw/2014001-2014365_COVEdata.DAT")
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
      fill(ghi, .direction = "up") %>% fill(ghi) %>%
      mutate(index = strftime(date_time, format = "%Y-%m-%d"),
             day_ind = as.numeric(strftime(date_time, format = "%j")))
    
    bsrn_df.cove.days = left_join(data.frame(index = sample_dt, stringsAsFactors = FALSE), bsrn_df.cove.days) %>%
      select(-index) %>%
      mutate(sun_hrs = ifelse(ghi == 0, 0, 1))
    bsrn_df.cove.days = bsrn_df.cove.days[2:nrow(bsrn_df.cove.days),] %>%
      add_weather()
    if (type == "write") {
      temp_file_name = paste0("inputs/solar_bsrn_cove_",
                              strftime(Sys.time(), format = "%y%m%d_%H%M"),
                              ".csv")
      write.csv(bsrn_df.cove.days, temp_file_name)
    }
  }
  if (type == "read") {
    bsrn_df.cove.days = read.csv("inputs/solar_bsrn_cove.csv") %>% 
      select(-X) %>%
      mutate_if(is.integer, as.numeric) %>%
      mutate(date_time = as.POSIXct(date_time))
  }
  return(bsrn_df.cove.days)
}
get_bsrn_clearsky = function() {
  # Fetches clearsky data for Virginia
  # where LARC and COVE data comes from
  # from local csv
  
  bsrn_df.clearsky = fread("inputs/bsrn_raw/1155277_37.01_-76.34_2014.csv")
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
  # Attaches weather type, based on Hofmann (2015)
  # for 1min solar data
  
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
                            ifelse((0.8*kt.bar-kt.til)>=0.72, "Cloudless",
                                   "Some clouds")))
  bsrn_df.station = left_join(cols_to_add, bsrn_df.station)
}
get_markovchains = function(df_str) {
  # returns a transition probability matrix
  # for each weather type in
  # the selected dataframe
  # i.e., 1min solar data from either COVE or LARC
  
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
get_1day_chain = function(t0.val, cop = 2, tpm.list, weather) {
  # returns a 24-hr time-series of clearness index (kt) values
  # based on a starting condition and weather type
  
  df_names = c("cove", "larc", "too_many")
  daychains <-  foreach(i = 1:length(tpm.list),
                        .combine = "cbind.data.frame") %do% {
                          foreach(j = 1:cop,
                                  .combine = "cbind.data.frame") %do% {
                                    if (t0.val == 0) {
                                      daychain <- rep(0, 24*60)
                                    }
                                    else {
                                      tpm <- tpm.list[[i]]
                                      daychain <- as.numeric(markovchainSequence(
                                        24*60-1,
                                        tpm$estimate,
                                        t0 = round(t0.val, 2),
                                        include.t0 = TRUE))
                                    }
                                    daychain <- list(j = daychain)
                                  }}
  names(daychains) <- sort(unlist(lapply(1:length(tpm.list), function(x) {
    lapply(1:cop, function(y) {
      paste0("kt_min.",df_names[[x]],"_",y)})})))
  return(daychains)
}
attach_chains = function(df, scalars) {
  # attaches new ghi values
  # and changes name of the resulting columns
  
  ghi.col <- select(df, kt, clr_ghi)
  ghi_min <- foreach(x = iter(scalars, by = 'col'),
                     nm = colnames(scalars),
                     .combine = "cbind.data.frame") %do% {
                       new_ghi_min = ghi.col %>%
                         mutate(new_ghi_min = clr_ghi*x) %>%
                         select(new_ghi_min)
                       new_nm <- gsub("^.*?_","ghi_",nm)
                       names(new_ghi_min) <- new_nm
                       new_ghi_min
                     }
  
  df <- cbind.data.frame(df, scalars, ghi_min)
  
  return(df)
}
get_kt_1min = function(daily_df, tpm.list, cop = 2, interval) {
  # Attaches 1-min clearness index (kt) values
  # to a time-series spanning 24hrs
  
  weather <- unique(daily_df$weather)
  t0.val <- filter(daily_df, kt > 0) %>%
    filter(row_number(kt) == 1) %>%
    select(kt) %>%
    as.numeric()
  
  # sets max kt value to 2
  kt.1min <- get_1day_chain(t0.val, cop, tpm.list) %>%
    mutate_all(function(x) ifelse(x > 2, 2, x))
  
  day_df <- data.frame(date_time = seq.POSIXt(daily_df$date_time[1],
                                              length.out = nrow(kt.1min),
                                              by = 60))
  day_df <- day_df %>%
    mutate(day_ind = as.numeric(strftime(date_time, format = "%j")),
           dayhr_ind = day_ind + as.numeric(strftime(date_time, format = "%H"))/24,
           min_ind = as.numeric(strftime(date_time, format = "%M")),
           min_ind = dayhr_ind + min_ind%/%(60*interval)/(60*24)) %>%
    left_join(daily_df, by = c("day_ind", "dayhr_ind")) %>%
    mutate(date_time = date_time.x) %>%
    select(-date_time.x, -date_time.y) %>%
    attach_chains(kt.1min)
  
  # compares daily ghi values from input data
  # to markov generated ones
  # calculates scale_factor
  daily.ghi <- select(day_df, dayhr_ind, ghi) %>%
    group_by(dayhr_ind) %>%
    summarise(ghi = mean(ghi)) %>%
    ungroup() %>%
    summarise(ghi = sum(ghi)) %>%
    as.numeric()
  mC.ghi <- select(day_df, dayhr_ind, contains("ghi_")) %>%
    group_by(dayhr_ind) %>%
    summarise_at(contains("ghi_"), mean) %>%
    ungroup() %>%
    summarise_at(contains("ghi_"), sum) %>%
    rowMeans()
  scale_factor <- daily.ghi / mC.ghi
  # message(paste(scale_factor, "-",
  #               unique(daily_df$day_ind), "-",
  #               weather, "-",
  #               daily.ghi, "-",
  #               mC.ghi))
  
  # if markov-gen'd values are within 5%
  # scale factor goes unused
  # otherwise values are scaled to be within 5%
  cols_to_keep <- select(day_df, day_ind:kt, date_time)
  scale_factor <- ifelse(abs(scale_factor-1) <= 0.05, 1, scale_factor)
  scaled_kt <- select(day_df, contains("kt_"), kt)
  scaled_kt <- mutate_all(scaled_kt,
                          function(x) ifelse(scaled_kt$kt == 0, 0, x)) %>%
    select(-kt) %>%
    mutate_all(function(x) x*scale_factor)
  scaled_ghi <- select(day_df, contains("ghi_")) %>%
    mutate_all(function(x) x*scale_factor)
  output_df <- cbind.data.frame(cols_to_keep, scaled_kt, scaled_ghi)
  return(output_df)
}
get_1yr_markov = function(src_df = list("cove", "larc"),
                          cop = 2, interval,
                          save_rds, seed = NULL) {
  # compiles a specified number of copies of
  # 1yr time-series with 1min intervals 
  # of clearness index (kt) and ghi values
  # based on hourly solar data from NSRDB
  
  # can save the resulting dataframe as an rds
  # THIS STEP IS NECESSARY BEFORE RUNNING SIMULATIONS
  
  if (!is.null(seed)) seed = 7
  
  nsrdb_df <- get_nyc_solar("read")
  
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  
  funs.to.pass <- c("get_bsrn.cove", "get_bsrn.larc",
                    "get_nyc_solar", "get_markovchains",
                    "get_1day_chain", "attach_chains",
                    "get_kt_1min")
  pkgs.to.pass <- c("data.table", "foreach", "iterators",
                    "plyr", "dplyr",
                    "tidyr", "futile.logger")
  
  all_min = foreach(j = 1:365,
                    .combine = "bind_rows",
                    .multicombine = TRUE,
                    .export = funs.to.pass,
                    .packages = pkgs.to.pass,
                    # .errorhandling = "remove",
                    .verbose = TRUE) %dopar% {
                      
                      date_time <- data.frame(date_time = seq.POSIXt(strptime(paste("2014", j, "00:00"),
                                                                              format = "%Y %j %H:%M"),
                                                                     strptime(paste("2014", j, "23:55"),
                                                                              format = "%Y %j %H:%M"),
                                                                     by = "5 min"))
                      
                      day_min.0 = nsrdb_df %>%
                        filter(day_ind == j)
                      weather <- unique(day_min.0$weather)
                      tpm.1 <- get_markovchains(df_str = src_df[1])[[weather]]
                      tpm.2 <- get_markovchains(df_str = src_df[2])[[weather]]
                      tpm.list <- list(tpm.1, tpm.2)
                      
                      day_min.0 <- get_kt_1min(day_min.0, tpm.list, cop, interval)
                      kt_ghi <- select(day_min.0, min_ind,
                                       contains("kt_min."),
                                       contains("ghi_min.")) %>%
                        group_by(min_ind) %>%
                        summarise_if(is.numeric, mean) %>%
                        ungroup() %>%
                        mutate(weather = weather)
                      
                      if(nrow(date_time) < nrow(kt_ghi)) {
                        kt_ghi <- kt_ghi[1:nrow(date_time),]
                      }
                      if(nrow(date_time) > nrow(kt_ghi)) {
                        date_time <- date_time[1:nrow(kt_ghi),]
                      }
                      
                      cbind(date_time, kt_ghi)
                    }
  stopCluster(cl)
  
  all_min <- fill(all_min, weather, contains("_min"), .direction = "up")
  if(save_rds) saveRDS(all_min, "inputs/solar_min.rds")
  return(all_min)
}
validate_markov_df <- function(df, save_rds) {
  # Takes a dataframe of markov-generated 1min solar time-series
  # and compares its gradient frequency distribution
  # with that of the input 1min data (from COVE and LARC)
  
  # can save the resulting dataframe as an rds
  # THIS IS NECESSARY ONLY FOR PLOTTING
  # (see get_markov_freqpoly in ani_plots.R)
  
  bsrn_cove <- get_bsrn.cove("read")
  bsrn_larc <- get_bsrn.larc("read")
  bsrn_df <- full_join(bsrn_cove, bsrn_larc, by = "date_time",
                       suffix = c("_cove", "_larc"))
  bsrn_days <- unique(bsrn_df$day_ind_cove)
  bsrn_df <- bsrn_df %>%
    select(date_time, ghi_cove, ghi_larc)
  
  markov <- df %>%
    mutate(day_ind = as.numeric(strftime(date_time, format = "%j"))) %>%
    filter(day_ind %in% bsrn_days) %>% 
    mutate(markov_mean.cove = rowMeans(select(markov, matches("(.*ghi_)(.*cove)"))),
           markov_mean.larc = rowMeans(select(markov, matches("(.*ghi_)(.*larc)"))),
           markov_var.cove = ifelse(markov_mean.cove == 0, 0,
                                    abs(markov_mean.cove - lag(markov_mean.cove))/markov_mean.cove),
           markov_var.larc = ifelse(markov_mean.larc == 0, 0,
                                    abs(markov_mean.larc - lag(markov_mean.larc))/markov_mean.larc)) %>%
    select(date_time, contains("markov_var"))
  
  output_df <- left_join(markov, bsrn_df, by = "date_time") %>%
    mutate(cove_var = ifelse(ghi_cove == 0, 0, abs(ghi_cove - lag(ghi_cove))/ghi_cove),
           larc_var = ifelse(ghi_larc == 0, 0, abs(ghi_larc - lag(ghi_larc))/ghi_larc)) %>%
    mutate(cove_var = ifelse(cove_var < 0, 0, cove_var),
           larc_var = ifelse(larc_var < 0, 0, larc_var)) %>%
    fill(contains("var"), .direction = "up") %>%
    group_by(date_time) %>%
    summarize_if(is.numeric, mean) %>%
    select(date_time, contains("var"))
  
  if(save_rds) saveRDS(output_df, "inputs/solar_min_freq.rds")
  return(output_df)
}
