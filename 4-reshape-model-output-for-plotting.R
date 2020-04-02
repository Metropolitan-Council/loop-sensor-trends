

##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
#############

node_files <- list.files('output/daily_model_predictions_bynode')
node_names <- gsub('.csv', '', node_files)

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# node_files <- node_files[1:10] # test


diffs_dt <- rbindlist(foreach(i = node_files) %dopar% {
  library(data.table)
  dat <- fread(paste0('output/daily_model_predictions_bynode/', i))
  dat[, date := as.IDate(date)]
  # Trim to after March 1 2020:
  dat <- dat[date >= "2020-03-01", ]
  dat <- dat[date <= Sys.Date()-1]
  dat
})

stopCluster(cl)

# some very high numbers?
# hist(diffs_dt[,volume.diff])
# summary(diffs_dt[,volume.diff])
unique(diffs_dt[volume.diff < (-100), .(r_node_name, r_node_n_type)])
# r_node_name r_node_n_type
# 1:    rnd_5932          Exit
# 2:    rnd_5948          Exit
# 3:    rnd_5950      Entrance
# 4:   rnd_85131          Exit

unique(diffs_dt[volume.predict < 0, .(r_node_name, r_node_n_type)])

# negative predicted values -- one spurious node
# diffs_dt <- diffs_dt[!r_node_name == 'rnd_86223'] -- now many more
diffs_dt <- diffs_dt[!r_node_name %in% unique(diffs_dt[volume.predict < 0, r_node_name])]

# other spurious observations (8 of them)
# diffs_dt<-diffs_dt[volume.diff < (100)]
# another problematic node:
# diffs_dt <- diffs_dt[!r_node_name %in% c('rnd_86469', 'rnd_95784')]

# very high values
summary(diffs_dt[volume.diff > 100 & year == 2020, .(volume.predict, volume.diff)])
# how many of these?
# unique(diffs_dt[volume.diff > 500 & year == 2020, .(r_node_name, r_node_n_type, date)])
# 1:     rnd_849          Exit 2020-03-15
# 2:     rnd_849          Exit 2020-03-17
# 3:     rnd_849          Exit 2020-03-18
# 4:     rnd_849          Exit 2020-03-19
# 5:     rnd_849          Exit 2020-03-20
# 6:   rnd_88037      Entrance 2020-03-14
# 7:   rnd_88037      Entrance 2020-03-15
# 8:   rnd_88037      Entrance 2020-03-17
# 9:   rnd_88037      Entrance 2020-03-18

# get rid of thse as well
diffs_dt <- diffs_dt[!r_node_name %in% unique(diffs_dt[volume.diff > 500 & year == 2020, r_node_name])]



fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node.csv"))
fwrite(diffs_dt, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-by-node.csv"))


# More data reshaping of model output ----


# Total difference from expected for whole metro area ----
diffs_4plot <- diffs_dt[r_node_n_type == "Station" & year == 2020 # this year's data, stations only. ####
                        , lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                        .SDcols = c("volume.sum", "volume.predict"),
                        by = .(date, dow, doy, year, woy, weekday, monthday)
                        ]

# vmt is 1/2 of volume ----
diffs_4plot[, c("vmt.sum", "vmt.predict") := list(volume.sum * 0.5, volume.predict * 0.5)]
diffs_4plot[, "Difference from Typical VMT (%)" := round(100 * (vmt.sum - vmt.predict) / vmt.predict, 2)]
diffs_4plot[, difference_text := ifelse(`Difference from Typical VMT (%)` < 0, paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than typical"),
                                        paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")
)]


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region.csv"))
fwrite(diffs_4plot, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-region.csv"))
