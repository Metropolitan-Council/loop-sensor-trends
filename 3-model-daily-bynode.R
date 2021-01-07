##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
# Database packages:
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle)


#Connecting to the database -------------------------------
connect.string = '(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))'
tbidb = ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect.string,
  username = 'mts_planning_data',
  # mts_planning_view for viewing data only, no write priviliges. 
  # mts_planning_data is the username for write privlieges.
  password = rstudioapi::askForPassword("database password")
)
#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")
#############



config <- fread('data/Configuration of Metro Detectors 2020-03-24.csv')
config$date<-NULL
det_config <- unique(config[,.(node_name, r_node_n_type, 
                               r_node_transition, r_node_label, r_node_lon, r_node_lat, 
                               r_node_lanes, r_node_shift, r_node_s_limit, r_node_station_id, 
                               r_node_attach_side, corridor_route, corridor_dir)])

# node_files <- list.files('data/data_daily_node')
# node_names <- gsub('.csv', '', node_files)
node_year <- ROracle::dbGetQuery(
  tbidb,
 "select * from rtmc_yearly_node where data_year < to_date('2020', 'YYYY')"
) %>%
  mutate(DATA_YEAR = year(as.Date(DATA_YEAR)))%>%
  mutate(DATA_YEAR = as.factor(DATA_YEAR))%>%
  mutate(has_data = 1)%>%
  pivot_wider(names_from = DATA_YEAR, values_from = has_data) %>%
  filter(`2018` == 1 & `2019` == 1)

node_names <- node_year$NODE_NAME

# gam_list <- vector("list", length(node_names))
# pred_ls <- vector("list", length(node_names))




# node_files <- node_files[1:10] # test


for(i in node_names) {
  print(i)
  flush.console()
  

  library(data.table)
  library(lubridate)
  library(mgcv)
  library(dplyr)
  
  
  # i <- node_names[1000] # test

  
  dailydat <- ROracle::dbGetQuery(
    tbidb,
    paste0("select * from rtmc_daily_node where data_date < to_date('2020-03-01', 'YYYY-MM-DD')",
    " and node_name = ", "'",
    i, "'")
  )%>%
    rename_all(tolower)
  
  dailydat <- data.table(dailydat)
  
  if(nrow(dailydat) == 0){} else{
    
  # Dealing with date ----
  dailydat[, date := as.IDate(fast_strptime(as.character(data_date), "%Y-%m-%d"))]
  dailydat[, dow := wday(date)]
  dailydat[, doy := yday(date)]
  dailydat[, year := year(date)]
  dailydat[, woy := week(date)]
  dailydat[, weekday := factor(weekdays(date))]
  dailydat[, monthday := format(date, "%b %d")]
  
  
  # Must be non-zero total volume for the day -- these models do a bad job of modeling really low volumes.
  dailydat <- dailydat[total_volume>100]
  # get rid of 2017 data: (december 15-31 included in this pull) ----
  # dailydat <- dailydat[year > 2017, ]
  
  # must have at least 75% complete data in 2018 and 2019 ----
  pct_data = nrow(dailydat[dailydat$year %in% 2018:2019])/(365*2)
  

  if(pct_data>=0.75){} else{

  # subset to relevant dates:
  modeling_dat <- dailydat[dailydat$date < "2020-03-01",]
  
  # 2020 data v. sensitive - exclude some special holidays and weather days
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-01", ] # holiday - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-17", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-18", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-02-09", ] # snow day - exclude
  
   
  
  # Old Model (2020) - With s(day of week, k = 7)
  this_gam <- with(
    modeling_dat,
    mgcv::gam(
      total_volume ~
        s(dow, k = 7) # one knot for each day of the week
      + s(doy) # general seasonal trend, let it vary by year, allow knots to be set by gam
      # + as.factor(year) # intercept for each year
    )
  )
  
  # New Model (2020)  - day of week as a factor
  new_gam <- with(
    modeling_dat,
    mgcv::gam(
      total_volume ~
      + as.factor(dow)
      + s(doy, bs = "cc") # general seasonal trend, let it vary by year, allow knots to be set by gam
    )
  )
  
  # empty dataset of predictions
  date_range <- c(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "days"))
  
  predict_dat <- data.table(date = date_range)
  predict_dat[, date := as.IDate(date)]
  predict_dat[, dow := wday(date)]
  predict_dat[, doy := yday(date)]
  predict_dat[, node_name := modeling_dat$node_name[[1]]]
  
  # Predictions - total volume
  predicts_v2020_2 <- copy(predict_dat) 
  predicts_v2021_1 <- copy(predict_dat)
  
  predicts_v2020_2[, c("volume_predict", "volume_predict_se") := 
                                    cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = T, type = "response"))]
  predicts_v2021_1[, c("volume_predict", "volume_predict_se") := 
                                    cbind(predict.gam(object = new_gam, newdata = predict_dat, se.fit = T, type = "response"))]
  # Predictions - just seasonal effect, average weekday
  predicts_v2020_2[, c("s_doy_predict", "s_doy_predict_se") := 
                                    cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = T, 
                                                      type= "response", terms = "s(doy)"))]
  predicts_v2021_1[, c("s_doy_predict", "s_doy_predict_se") := 
                                    cbind(predict.gam(object = new_gam, newdata = predict_dat, se.fit = T, 
                                                      type= "response", terms = "s(doy)"))]
  # Predictions - just day-of-week effect, average day of the year
  predicts_v2020_2[, c("dow_predict", "dow_predict_se") := 
                                    cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = F, 
                                                      type= "response", terms = "s(dow)"))]
  predicts_v2021_1[, c("dow_predict", "dow_predict_se") := 
                                    cbind(predict.gam(object = new_gam, newdata = predict_dat, se.fit = F, 
                                                      type= "response", terms = "s(dow)"))]
  
 
  predicts_v2020_2[,model_name:="gam_2020_2"]
  predicts_v2021_1[,model_name:="gam_2021_1"]
  predicts <- rbind(predicts_v2020_2, predicts_v2021_1)
  
  predicts %>%
    rename(predict_date = date)%>%
    rename_all(toupper) %>%
    mutate(across(where(is.numeric), ~round(.x)))%>%
    select(NODE_NAME, PREDICT_DATE, VOLUME_PREDICT, VOLUME_PREDICT_SE,
           MODEL_NAME, S_DOY_PREDICT, S_DOY_PREDICT_SE, DOW_PREDICT, DOW_PREDICT_SE)%>%
    ROracle::dbWriteTable(
    conn = tbidb,
    name = "RTMC_PREDICT_TEMP",
    row.names = FALSE,
    append = TRUE)
  
  ROracle::dbSendQuery(tbidb, "commit")
  
  ROracle::dbSendQuery(tbidb,
                       paste0(
                         "insert into rtmc_predictions",
                         " select * from rtmc_predict_temp",
                         " where",
                         " not exists (",
                         " select * from rtmc_predictions",
                         " where  rtmc_predict_temp.predict_date = rtmc_predictions.predict_date",
                         " and rtmc_predict_temp.node_name = rtmc_predictions.node_name",
                         " and rtmc_predict_temp.model_name = rtmc_predictions.model_name",
                         ")" 
                       ))
  
  ROracle::dbSendQuery(tbidb, "commit")     
  
  ROracle::dbSendQuery(tbidb, "truncate table rtmc_predict_temp")
  ROracle::dbSendQuery(tbidb, "commit")    
  # # Storing Model Summaries -- an idea for another time. 
  # # create list
  # model_list <- list(this_gam, new_gam)
  # # give the elements useful names
  # names(model_list) <- c('gam_2020_2','gam_2021_1')
  # 
  # # get the summaries using `lapply
  # summary_list <- lapply(model_list, summary)
  # 
  # # extract the coefficients from these summaries
  # 
  # p.table_list <- lapply(summary_list, `[[`, 'p.table')
  # 
  # s.table_list <- lapply(summary_list, `[[`, 's.table')
  # 
  # rbind(summary(this_gam)$p.table, summary(this_gam)$s.table)
  # 
  
  
  
  
  
  # store difference from normal for 2020 for mapping
  # pred_ls[[i]] <- predict_dat
  
  } # end check for nodes with missing data
  } # end first check for nodes with no data at all
}


# saveRDS(gam_list, file = paste0('output/gam-models-', Sys.Date(), '.RData'))

stopCluster(cl)
