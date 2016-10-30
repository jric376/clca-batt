# PV Load Object

# This object contains a time-series of generated solar power
# plus metadata.

# wd_path = paste(Sys.getenv("USERPROFILE"), "\\OneDrive\\School\\Thesis\\program2", sep = "")
# setwd(as.character(wd_path))
# setwd("E:\\GitHub\\clca-batt")
library("data.table")
library("plyr")
library("dplyr")
library("foreach")
library("iterators")
library("doSNOW")
library("R6")
library("RSQLite")

pv_load <- R6Class("PV Load",
                     public = list(
                       
                       cell_eff = 0.17,
                       inv_eff = 0.96,
                       nameplate = 0.0,
                       plc2erta = 1834, # kg CO2eq / kWp, inclulding BOS
                       cap_cost.lo = 2000,
                       cap_cost.hi = 5300,
                       om_cost.lo = 12,
                       om_cost.hi = 22.50,
                       
                       initialize = function(
                         meta = NA,
                         rand_copies = NA
                       ) {
                         
                         if (meta[["bldg"]] == "office") {
                           nameplate = 86
                         }
                         
                         self$add_ts_df(readRDS("inputs\\solar_min.rds"),
                                        rand_copies)
                         self$add_metadata(meta)
                         self$nameplate = nameplate
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
                       
                       add_ts_df = function(rds, copies) {
                         ts_df = list()
                         
                         dt <- select(rds, date_time)
                         ghi <- select(rds, min_ind, contains("ghi_min."))
                         
                         for (i in 1:copies) {
                           
                           ghi_samp <- select(ghi, sample(1:ncol(ghi), 1))
                           names(ghi_samp) <- "kw"
                           ghi_samp <- mutate(ghi_samp,
                                              kw = kw*self$cell_eff*self$inv_eff)
                           
                           ts_df[[i]] <- cbind(dt, ghi_samp)
                         }
                         
                         private$ts_df = ts_df
                       },
                       
                       # add_base_ts = function(base_ts) {
                       #   if (missing(base_ts)) {
                       #     return("Base time-series data is missing")
                       #   }
                       #   else {
                       #     base_ts$date_time = strptime(base_ts$time_hr, format="%m/%d/%Y %H:%M")
                       #     base_ts$date_time$year <- base_ts$date_time$year + 2
                       #     base_ts$time_hr = NULL
                       #     
                       #     new_dt = as.POSIXlt(seq.POSIXt(base_ts$date_time[1],
                       #                                    base_ts$date_time[length(base_ts$date_time)],
                       #                                    by = "5 min"))
                       #     new_dt = new_dt[2:length(new_dt)]
                       #     new_dt = data.frame(new_dt,
                       #                         new_dt - as.numeric(format(new_dt, "%M"))*60)
                       #     colnames(new_dt) = c("dt", "date_time")
                       #     new_dt$dt = as.POSIXlt(new_dt$dt)
                       #     new_dt$date_time = as.POSIXlt(new_dt$date_time)
                       #     new_dt = merge(x = new_dt, y = base_ts,
                       #                    by = "date_time", all.y = TRUE)
                       #     new_dt$dt = as.POSIXct(new_dt$dt)
                       #     new_dt$date_time = as.POSIXct(new_dt$date_time)
                       #     new_dt = plyr::arrange(new_dt, dt)
                       #     new_dt$date_time = NULL
                       #     colnames(new_dt) = c("date_time", "PVinv_w")
                       #     
                       #     interval = self$set_interval(base_ts)
                       #     
                       #     new_dt$kw = new_dt$PVinv_w*0.001*(1-0.1408) # loss factor taken from SAM
                       #     new_dt$kwh = new_dt$kw*interval
                       #     new_dt$PVinv_w = NULL
                       #     new_dt = na.omit(new_dt)
                       #     
                       #     private$base_ts = new_dt
                       #   }
                       # },
                       
                       set_interval = function(ts) {
                         start_pt = 2
                         interval.num = abs(as.numeric(
                                             (difftime(ts$date_time[start_pt],
                                                       ts$date_time[start_pt + 1],
                                                       units = "hours"))))
                         interval = list("time_int" = interval.num)
                         private$metadata = append(private$metadata, interval)
                         
                         return(interval.num)
                       },
                       
                       get_ts_count = function() {
                         return(length(private$ts_df))
                       },
                       
                       get_ts_df = function(index) {
                         if (missing(index)) return(private$ts_df)
                         if (index %in%  seq.int(1:length(private$ts_df))) {
                           return(private$ts_df[[index]])
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
                       ts_df = NULL, # takes a list of dataframes
                       metadata = NULL
                     )
)

get_pv <- function(run_id, type, copies = 0) {

  metadat = list(
    "bldg" = type,
    "run_id" = run_id,
    "copies" = copies
  )
  
  pv_test <- pv_load$new(
                        meta = metadat,
                        rand_copies = copies
  )
  
  return(pv_test)
}