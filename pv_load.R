# PV Load Object

# This object contains a time-series of generated solar power
# plus metadata.

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library('foreach')
library('iterators')
library('doSNOW')
library('R6')

pv_load <- R6Class("PV Load",
                     public = list(
                       initialize = function(
                         meta = NA, pv_ts_path = NA,
                         rand_copies = NA, rand_factor = NA
                       ) {
                         self$add_base_ts(read.csv(pv_ts_path, head = T, stringsAsFactors = F))
                         self$add_metadata(meta)
                         self$stochastize_ts(rand_copies, rand_factor)
                       },
                       
                       add_metadata = function(metadata) {
                         if (length(metadata) < 1) {
                           private$metadata = 'Empty'
                         }
                         else {
                           for (datum_name in names(metadata)) {
                             private$metadata[[datum_name]] <- metadata[[datum_name]]
                           }
                         }
                       },
                       
                       add_base_ts = function(base_ts) {
                         if (missing(base_ts)) {
                           return("Base time-series data is missing")
                         }
                         else {
                           base_ts$date_time = strptime(base_ts$time_hr, format="%m/%d/%Y %H:%M")
                           base_ts$date_time$year <- base_ts$date_time$year + 2
                           base_ts$time_hr = NULL
                           
                           new_dt = as.POSIXlt(seq.POSIXt(base_ts$date_time[1],
                                                          base_ts$date_time[length(base_ts$date_time)],
                                                          by = "5 min"))
                           new_dt = new_dt[2:length(new_dt)]
                           new_dt = data.frame(new_dt,
                                               new_dt - as.numeric(format(new_dt, "%M"))*60)
                           colnames(new_dt) = c("dt", "date_time")
                           new_dt$dt = as.POSIXlt(new_dt$dt)
                           new_dt$date_time = as.POSIXlt(new_dt$date_time)
                           new_dt = merge(x = new_dt, y = base_ts,
                                          by = "date_time", all.y = TRUE)
                           new_dt$dt = as.POSIXct(new_dt$dt)
                           new_dt$date_time = as.POSIXct(new_dt$date_time)
                           new_dt = arrange(new_dt, dt)
                           new_dt$date_time = NULL
                           colnames(new_dt) = c("date_time", "PVinv_w")
                           
                           start_pt = sample(1:(length(new_dt$date_time)-1),1)
                           interval = list(
                             "time_int" = abs(as.numeric(
                               (difftime(new_dt$date_time[start_pt],
                                         new_dt$date_time[start_pt + 1],
                                         units = "hours"))))
                           )
                           private$metadata = append(private$metadata, interval)
                           
                           new_dt$kw = new_dt$PVinv_w*0.001*(1-0.1408) # loss factor taken from SAM
                           new_dt$kwh = new_dt$kw*private$metadata$time_int
                           new_dt$PVinv_w = NULL
                           new_dt = na.omit(new_dt)
                           
                           private$base_ts = new_dt
                         }
                       },
                       
                       stochastize_ts = function(copies = 1, rand_factor) {
                         ts_df = list()
                         ts_df[[1]] = self$get_base_ts()
                         
                         if (copies > 0) {
                           cl <- makeCluster(3)
                           
                           for (i in 1:copies) {
                             registerDoSNOW(cl)
                             j = i + 1
                             new_ts = private$base_ts
                             
                             
                             foreach(x = iter(new_ts, by = 'col'), nm = colnames(new_ts)) %do%
                               if (is.numeric(x)) {
                                 x = sapply(x, function(y) rnorm(1, mean = y, sd = y*rand_factor))
                                 new_ts[[nm]] = x
                               }
                             
                             ts_df[[j]] = new_ts
                           }
                           stopCluster(cl)
                         }
                         
                         private$ts_df = ts_df
                       },
                       
                       get_base_ts = function() {
                         return(private$base_ts)
                       },
                       
                       get_ts_count = function() {
                         return(length(private$ts_df))
                       },
                       
                       get_ts_df = function(index) {
                         if (missing(index)) return(private$ts_df)
                         if (index %in%  seq.int(1:length(private$ts_df))) {
                           return(private$ts_df[index])
                         }
                         else {
                           stop(paste(
                             "Index", index, "not in bounds.",
                             "It's between 1 and", length(private$ts_df)
                           )
                           )
                         }
                       },
                       
                       get_metadata = function() {
                         return(private$metadata)
                       }
                     ),
                     private = list(
                       base_ts = NULL,
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_pv <- function(run_id, copies = 0, factor = 0.1) {
  metadat = list(
    "bldg" = "office",
    "kw" = 86,
    "run_id" = run_id,
    "copies" = copies,
    "factor" = factor
  )
  pv_test <- pv_load$new(
    pv_ts_path = "inputs\\solar_nycDC.csv",
    meta = metadat, rand_copies = copies, rand_factor = factor
  )
  
  return(pv_test)
}