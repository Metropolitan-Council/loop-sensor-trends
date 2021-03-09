##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
library(dplyr)
library(ggplot2)
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
# rnd_86377


config <- dbGetQuery(tbidb, "select * from rtmc_configuration")


node_names <- unique(config$NODE_NAME)

# gam_list <- vector("list", length(node_names))
# pred_ls <- vector("list", length(node_names))


which(node_names == "rnd_88859") # left off at 506

# node_files <- node_files[1:10] # test

tictoc::tic()


# pdf(file = 'hourly_diff_plots.pdf', onefile = T)

for (i in node_names[2001:3983]) {
  
  # i <- node_names[1000] # test
  # i <- 'rnd_88221' # test
  
  
  hourlydat <- ROracle::dbGetQuery(
    tbidb,
    paste0(
      "select * from rtmc_hourly_node where data_date < to_date('2020-03-01', 'YYYY-MM-DD')",
      " and node_name = ",
      "'",
      i,
      "'"
    )
  ) %>%
    rename_all(tolower)  %>%
    select(node_name, data_date, data_hour, total_volume)
  
  
  
  hourlydat <- data.table(hourlydat)
  if (nrow(hourlydat) <= 2 | median(hourlydat$total_volume)< 10) {
            print(paste0("no data for node ", i))
          } else{
            # Dealing with date ----
            hourlydat[, date := as.Date(data_date)]
            hourlydat[, doy := yday(date)]
            hourlydat[, year := year(date)]
            hourlydat[, weekday := factor(weekdays(date))]
            
            # must have at least 75% complete data in 2018 and 2019 ----
            pct_data = nrow(hourlydat[hourlydat$year %in% 2018:2019]) / (365 * 2 * 24)
            
            if (pct_data < 0.75) {
              print(paste0("insufficient data for node ", i, ' -- ', round(100*pct_data), "% complete"))
            } else {
              # subset to relevant dates:
              modeling_dat<- hourlydat[hourlydat$date < "2020-03-01",]    
              modeling_dat$weekday <- factor(modeling_dat$weekday, 
                                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
              this_gam <- with(modeling_dat,
                               gam(
                                 total_volume ~
                                   s(data_hour, by = weekday, bs = "cs")
                                 + s(doy, bs = "cs", k = 12),
                                 family = tw() # tweedie family for these low-count data?
                               ))
              
              # empty dataset of predictions
              date_range <-
                c(seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2021-12-31 23:00:00"), by = "hours"))
              
              predict_dat <-
                data.table(date = date_range)
              predict_dat[, weekday := factor(weekdays(date))]
              predict_dat[, doy := yday(date)]
              predict_dat[, data_hour:=hour(date)]
              predict_dat[, node_name := modeling_dat$node_name[[1]]]
              
              predict_dat[, c('predicted.volume') :=
                            round(
                              mgcv::predict.gam(
                                object = this_gam,
                                newdata = predict_dat,
                                se.fit = F,
                                type = 'response'
                              )
                            )]
              
              # also calculate a strict average
              avg_dat <- modeling_dat[,
                                      round(mean(total_volume, na.rm = T)),
                                      by = .(data_hour, weekday, month(date))]
              
              setnames(avg_dat, old = "V1", new = 'avg.volume')
              
              
              
              # Plotting data:
              predict_dat[, month := month(date)]
              
              plot_dat <- merge(predict_dat, avg_dat, all.x = T)
              
              setorder(plot_dat, 'date', 'data_hour')
              
              # for plotting, just grab the middle week of every month
              plot_dat <- plot_dat[mday(plot_dat$date)%in%c(15:21) 
                                   & year(plot_dat$date) == 2021,]
              plot_dat$weekday <- factor(plot_dat$weekday, 
                                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
              title_string <- paste0("Node ", i, " | Rsq= ", round(summary(this_gam)$r.sq, 2), " | Median Volume = ", 
                                     median(modeling_dat$total_volume, na.rm = T))
              myplot <-
              ggplot(plot_dat, aes(x = data_hour, y = predicted.volume))+
                geom_line()+
                facet_grid(month~weekday) + 
                geom_line(aes(y = avg.volume), color = 'red') + 
                cowplot::theme_cowplot() + theme(plot.title = element_text(hjust = 0.5))+
                ggtitle(title_string)
              
              ggsave(paste0('output/hrtestplots/hourlytest_', i, '.jpeg'),
                     myplot, height = 8, width = 8, units = "in")
              
              
              # Rbind two ways of calculating baseline
              avg_dat_withdates <- merge(avg_dat, predict_dat[,.(node_name, date, weekday, month, doy, data_hour)],  
                                         by = c("weekday", "month", "data_hour"))
              avg_dat_withdates[,model_name := 'avg_2018_2019']
              setnames(avg_dat_withdates, 'avg.volume', 'predicted.volume')
              predict_dat[,model_name := 'gam_hourly_2021_1']
              
              predicts_to_db <- 
              predict_dat %>%
                rbind(avg_dat_withdates) %>%
                arrange(node_name, model_name, date) %>%
                select(node_name, date, predicted.volume, model_name, data_hour)
              
              temp_insert_result <- 
                ROracle::dbWriteTable(
                  predicts_to_db,
                  conn = tbidb,
                  name = "RTMC_PREDICT_HOURLY_TEMP",
                  row.names = FALSE,
                  append = TRUE
                )
              temp_insert_result
              # ROracle::dbClearResult(temp_insert_result)
              
              temp_commit_result <- ROracle::dbSendQuery(tbidb, "commit")
              temp_commit_result
              ROracle::dbClearResult(temp_commit_result)
              
              perm_insert_result <-
                ROracle::dbSendQuery(
                  tbidb,
                  paste0(
                    "insert into rtmc_predictions_hourly",
                    " select * from rtmc_predict_hourly_temp",
                    " where",
                    " not exists (",
                    " select * from rtmc_predictions_hourly",
                    " where  rtmc_predict_hourly_temp.predict_date = rtmc_predictions_hourly.predict_date",
                    " and rtmc_predict_hourly_temp.node_name = rtmc_predictions_hourly.node_name",
                    " and rtmc_predict_hourly_temp.model_name = rtmc_predictions_hourly.model_name",
                    ")"
                  )
                )
              perm_insert_result
              ROracle::dbClearResult(perm_insert_result)
              
              insert_commit_result <- ROracle::dbSendQuery(tbidb, "commit")
              insert_commit_result
              ROracle::dbClearResult(insert_commit_result)
              
              truncate_table_result <-
                ROracle::dbSendQuery(tbidb, "truncate table rtmc_predict_hourly_temp")
              truncate_table_result
              ROracle::dbClearResult(truncate_table_result)
              
              truncate_commit_result <-
                ROracle::dbSendQuery(tbidb, "commit")
              truncate_commit_result
              ROracle::dbClearResult(truncate_commit_result)
                
              print(paste0("..............................................................", round(which(node_names == i)/length(node_names)*100), "% complete"))
              flush.console()
              
            } # end check for complete data
          } # end  check for enough data
        } # end for loop loop

tictoc::toc()

