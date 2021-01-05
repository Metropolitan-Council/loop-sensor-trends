##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
#############
config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL
det_config <- unique(config[,.(r_node_name, r_node_n_type, 
                               r_node_transition, r_node_label, r_node_lon, r_node_lat, 
                               r_node_lanes, r_node_shift, r_node_s_limit, r_node_station_id, 
                               r_node_attach_side, corridor_route, corridor_dir)])

node_files <- list.files('data/data_daily_node')
node_names <- gsub('.csv', '', node_files)
# gam_list <- vector("list", length(node_names))
# pred_ls <- vector("list", length(node_names))

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# node_files <- node_files[1:10] # test


foreach(i = node_files) %dopar% {
  
  print(i) 
  flush.console()
  library(data.table)
  library(lubridate)
  library(mgcv)
  
  
  # i <- node_files[[3245]] # test
  # i <- "rnd_1805.csv"
  dailydat <- fread(paste0("data/data_daily_node/", i))
  dailydat <- unique(dailydat)
  if(nrow(dailydat) == 0){} else{
    
  # Dealing with date ----
  dailydat[, date := as.IDate(fast_strptime(date, "%Y-%m-%d"))]
  dailydat[, dow := wday(date)]
  dailydat[, doy := yday(date)]
  dailydat[, year := year(date)]
  dailydat[, woy := week(date)]
  dailydat[, weekday := factor(weekdays(date))]
  dailydat[, monthday := format(date, "%b %d")]
  
  
  # get rid of 2017 data: (december 15-31 included in this pull) ----
  dailydat <- dailydat[year > 2017, ]
  
  # must have 3 years of data, at least 60 days of data in each year ----
  dailydat[, "num_days_per_year" := uniqueN(date), by = .(r_node_name, year)]
  dailydat <- dailydat[num_days_per_year > 60]
  
  has_2020_data <- unique(dailydat$r_node_name[dailydat$year == 2020 & dailydat$date < "2020-03-01"])
  has_2018_data <- unique(dailydat$r_node_name[dailydat$year == 2018])
  has_2019_data <- unique(dailydat$r_node_name[dailydat$year == 2019])
  
  dailydat <- dailydat[dailydat$r_node_name %in% has_2020_data
                       & dailydat$r_node_name %in% has_2019_data
                       & dailydat$r_node_name %in% has_2018_data, ]
  if(nrow(dailydat) == 0){} else{

  # subset to relevant dates:
  modeling_dat <- dailydat[dailydat$date < "2020-03-01",]
  
  # 2020 data v. sensitive - exclude some special holidays and weather days
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-01", ] # holiday - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-17", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-18", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-02-09", ] # snow day - exclude
  
  # empty dataset of predictions
  date_range <- c(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "days"))
  predict_dat <- data.table(date = date_range)
  predict_dat[, date := as.IDate(date)]
  predict_dat[, dow := wday(date)]
  predict_dat[, doy := yday(date)]
  predict_dat[, year := year(date)]
  predict_dat[, woy := week(date)]
  predict_dat[, weekday := factor(weekdays(date))]
  predict_dat[, monthday := format(date, "%b %d")]
  predict_dat[, r_node_name := modeling_dat$r_node_name[[1]]]
  predict_dat <- merge(predict_dat, det_config, all.x = T)
  
  
  # Old Model (2020) - With s(day of week, k = 7)
  this_gam <- with(
    modeling_dat,
    mgcv::gam(
      volume.sum ~
        s(dow, k = 7, by = as.factor(year)) # one knot for each day of the week
      + s(doy) # general seasonal trend, let it vary by year, allow knots to be set by gam
      + as.factor(year) # intercept for each year
    )
  )
  
  # gam_list[[i]] <- this_gam
  
  # New Model (2020)  - day of week as a factor
  new_gam <- with(
    modeling_dat,
    mgcv::gam(
      volume.sum ~
      + interaction(as.factor(year), as.factor(dow))
      + s(doy, bs = "cc") # general seasonal trend, let it vary by year, allow knots to be set by gam
    )
  )
  
  

  
  predict_dat[, c("volume.predict", "volume.predict.se",
                  "new.volume.predict", "new.volume.predict.se") := 
                cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = T), 
                      predict.gam(object = new_gam, newdata = predict_dat, se.fit = T))]
  
  predict_dat <- merge(predict_dat, 
                       dailydat[,.(r_node_name, date, volume.sum)], all.x = T, 
                       by = c('r_node_name', 'date'))
  
  # difference from predicted, n volume:
  predict_dat[, volume.diff.raw := (volume.sum - volume.predict)]
  
  # difference from predicted, in %:
  predict_dat[, volume.diff := round(((volume.sum - volume.predict) / volume.predict) * 100, 1)]
  
  
  
  
  
  
  fwrite(predict_dat, file = paste0('output/daily_model_predictions_bynode/', i))
  
  
  
  
  # store difference from normal for 2020 for mapping
  # pred_ls[[i]] <- predict_dat
  
  } # end check for nodes with missing data
  } # end first check for nodes with no data at all
}


# saveRDS(gam_list, file = paste0('output/gam-models-', Sys.Date(), '.RData'))

stopCluster(cl)
