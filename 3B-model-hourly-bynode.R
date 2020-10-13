library(doParallel)
library(foreach)
library(data.table)
library(sf)
library(leaflet)
library(tidyverse)
library(mgcv)

# Read in hourly data by node and combine.
# READ DATA (takes about 1.5 minutes)----
setwd('N:\\MTS\\Working\\Modeling\\MetroLoopDetectors\\loop-sensor-trends')
sensor_config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')

node_files <- list.files('data/data_hourly_node/')
node_files <- node_files[order(node_files)]
node_names <- gsub('.csv', '', node_files)
# corr_nodes <- sensor_config[corridor_route %in% c("I-94", "I-694", "I-494", "I-35E")
#                            # If you want to exclude entry/exit ramps:
#                            & r_node_n_type == 'Station', ]
# corr_nodes <- unique(corr_nodes[, c('r_node_name', 'r_node_n_type')])
#
# corr_files <- node_files[node_names %in% corr_nodes$r_node_name]


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

tictoc::tic()


# pdf(file = 'hourly_diff_plots.pdf', onefile = T)

foreach(i = node_files,
        .packages = c('data.table', 'mgcv', 'ggplot2')) %dopar% {
          # for (i in node_files) {
          # print(i)
          # dat <- fread(paste0('data/data_hourly_node/', i), header = T)
          dat <-
            fread(paste0('data/data_hourly_node/', i)
                  , header = T)
          dat <- dat[,-1]
          
          dat[, volume.sum := ifelse(volume.sum > 10000, NA, volume.sum)]
          
          if (nrow(dat) <= 2) {
            # print(paste0("no data for node ", i))
          } else{
            complete_df <- dat[date >= '2018-01-01'
                               & date < '2020-03-01', ]
            # make sure it has 2018, 2019 and 2020 data
            n_expected <- data.table(year = c(2018, 2019, 2020),
                                     n_expected = c(365 * 24,
                                                    365 * 24,
                                                    61 * 25))
            
            complete_df <-
              complete_df[, lapply(.SD, function(x)
                sum(!is.na(x[x > 0]))),  # make sure it's not all zeros, too -- should have 1 car per hour
                by = c("year"),
                .SDcols = c("volume.sum")]
            complete_df <- merge(n_expected, complete_df, all = T)
            complete_df[, pct_complete := volume.sum / n_expected]
            # at least x% of data across all important data periods:
            dat_complete <-
              ifelse(sum(is.na(complete_df$pct_complete) > 0) # if it's missing a year
                     | min(complete_df$pct_complete) < 0.25, # or if the non-zero, non-na total of rows is less than 25%
                     0, 1) # then the data are incomplete. otherwie, good enough for now!
            
            if (dat_complete == 0) {
              # print(paste0(
              #   "incomplete data, 1 or more years missing sufficient data for node ",
              #   i
              # ))
            } else {
              dat <- dat[, doy := yday(date)]
              dat <- dat[, dow := wday(date)]
              dat[, dow_fac := fcase(dow == 1,
                                     "Sun",
                                     dow %in% c(2:5),
                                     "Mon-Thr",
                                     dow == 6,
                                     "Fri",
                                     dow == 7,
                                     "Sat")]
              dat[, dow_fac := factor(dow_fac, levels = c("Sun", "Mon-Thr", "Fri", "Sat"))]
              
              
              modeling_dat <-
                dat[dat$date >= '2018-01-01' &
                      dat$date < '2020-03-01',]
              modeling_dat <-
                modeling_dat[!modeling_dat$date == "2020-01-01",] # holiday in 2020
              modeling_dat <-
                modeling_dat[!modeling_dat$date == '2020-01-17',] # cold snap - exclude
              modeling_dat <-
                modeling_dat[!modeling_dat$date == '2020-01-18',] # cold snap - exclude
              modeling_dat <-
                modeling_dat[!modeling_dat$date == '2020-02-09',] # snow day - exclude
              
              
              this_gam <- with(modeling_dat,
                               gam(
                                 volume.sum ~
                                   s(hour, by = dow_fac)
                                 + s(doy, bs = "cs", k = 12)
                                 + as.factor(year)
                                 + dow_fac,
                                 family = tw()
                               ))
              
              predict_dat <- dat[year==2020,
                                 .(r_node_name,
                                   year,
                                   date,
                                   doy,
                                   dow,
                                   dow_fac,
                                   hour,
                                   volume.sum)]
              
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
                                      round(mean(volume.sum, na.rm = T)),
                                      by = .(hour, dow_fac, month(date))]
              setnames(avg_dat, old = "V1", new = 'avg.volume')
              
              predict_dat[, month := month(date)]
              predict_dat <- merge(predict_dat, avg_dat, all.x = T)
              setorder(predict_dat, 'date', 'hour')
              
              
              # # difference from predicted, in %:
              # predict_dat[, diff.hourly.volume := round(((volume.sum - predicted.volume) / predicted.volume) * 100, 1)]
              
              fwrite(predict_dat[,.(r_node_name, date, hour, volume.sum, predicted.volume, avg.volume)],
                     paste0('output/hourly_model_predictions_bynode/',
                            i),
                     append = F)
              
              # myplot <-
              #   ggplot(predict_dat[hour %in% c(8, 12, 17) &
              #                        doy < 100, ], aes(x = date, y = predicted.volume)) +
              #   theme_minimal() +
              #   geom_point() +
              #   geom_line() +
              #   geom_line(aes(y = volume.sum), color = 'red') +
              #   facet_wrap(hour ~ ., scales = "free", nrow = 3) +
              #   scale_x_date(date_breaks = "week", date_labels = '%b %d') +
              #   geom_hline(yintercept = 0) +
              #   ggtitle(predict_dat$r_node_name) +
              #   theme(legend.position = 'none') +
              #   labs(x = "Date", y = "Difference from Predicted Volume")
              # print(myplot)
              # ggsave(paste0('output/hrtestplots/hourlytest_', i, '.jpeg'),
              #        myplot)
              
            } # end check for complete data
          } # end  check for enough data
        } # end foreach loop

tictoc::toc()

stopCluster(cl)
