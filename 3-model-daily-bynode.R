##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
library(lubridate)
#############

dailydat <- fread("data/data_daily_bynode_clean.csv")

# Dealing with date ----
dailydat[, date := as.IDate(date)]
dailydat[, dow := wday(date)]
dailydat[, doy := yday(date)]
dailydat[, year := year(date)]
dailydat[, woy := week(date)]
dailydat[, weekday := factor(weekdays(date))]
dailydat[, monthday := format(date, "%b %d")]


# get rid of 2017 data: (december 15-31 included in this pull) ----
dailydat <- dailydat[year > 2017, ]

# get rid of december data: ----
dailydat <- dailydat[doy < 100, ]

dim(dailydat) # 348203     14


# ggplot(dailydat, aes(x = volume.sum, fill = factor(year)))+
#   geom_density(alpha = 0.5)
# some very high numbers
dailydat[volume.sum > num_sensors_this_year * 23 * 2000] # reasonable though

# no such thing as 0 daily volume
dailydat <- dailydat[volume.sum > 0]

# must have 3 years of data, at least 60 days of data in each year ----
dailydat[, "num_days_per_year" := uniqueN(date), by = .(r_node_name, year)]
dailydat <- dailydat[num_days_per_year > 60]

has_2020_data <- unique(dailydat$r_node_name[dailydat$year == 2020])
has_2018_data <- unique(dailydat$r_node_name[dailydat$year == 2018])
has_2019_data <- unique(dailydat$r_node_name[dailydat$year == 2019])

dailydat <- dailydat[dailydat$r_node_name %in% has_2020_data
& dailydat$r_node_name %in% has_2019_data
& dailydat$r_node_name %in% has_2018_data, ]
dim(dailydat) # 322241     15

dailydat_s <- split(dailydat, dailydat$r_node_name)

# length(dailydat_s) # 1274
diffs_ls <- vector("list", length(dailydat_s))
gam_list <- vector("list", length(dailydat_s))


# MODEL TIME ----
for (s in seq_along(dailydat_s)) {
  # print(s)
  # flush.console()
  this_dat <- dailydat_s[[s]]
  # subset to relevant dates:
  modeling_dat <- this_dat[this_dat$date < "2020-03-01"
  & this_dat$doy <= 90 # feed it relevant dates - before april 1
  & this_dat$doy > 1, ] # exclude the one major holiday in here - jan 1

  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-17", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-18", ] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == "2020-02-09", ] # snow day - exclude

  this_gam <- with(
    modeling_dat,
    mgcv::gam(
      volume.sum ~
      s(dow, k = 7, by = as.factor(year)) # one knot for each day of the week
      + s(doy) # general seasonal trend, let it vary by year, allow knots to be set by gam
        + as.factor(year) # intercept for each year
    )
  )

  gam_list[[s]] <- this_gam

  this_dat[, c("volume.predict", "volume.predict.se") := cbind(predict.gam(object = this_gam, newdata = this_dat, se.fit = T))]

  # difference from predicted, n volume:
  this_dat[, volume.diff.raw := (volume.sum - volume.predict)]

  # difference from predicted, in %:
  this_dat[, volume.diff := round(((volume.sum - volume.predict) / volume.predict) * 100, 1)]

  # store difference from normal for 2020 for mapping
  this_diff <- this_dat
  diffs_ls[[s]] <- this_diff
}

diffs_dt <- rbindlist(diffs_ls)
# saveRDS(gam_list, file = paste0('data/gam-models-', Sys.Date(), '.RData'))

# some very high numbers?
# hist(diffs_dt[,volume.diff])
# summary(diffs_dt[,volume.diff])
unique(diffs_dt[volume.diff < (-100), .(r_node_name, r_node_n_type)])
# r_node_name r_node_n_type
# 1:    rnd_1576          Exit
# 2:    rnd_5932          Exit
# 3:    rnd_5948          Exit
# 4:    rnd_5950      Entrance
# 5:     rnd_841      Entrance
# 6:   rnd_85131          Exit
# 7:   rnd_85435      Entrance
# 8:   rnd_85649       Station
# 9:   rnd_85657       Station
# 10:   rnd_86223       Station
# 11:   rnd_86797      Entrance
# 12:   rnd_87263       Station
# 13:   rnd_87273       Station
# 14:   rnd_87565      Entrance
# 15:   rnd_87915      Entrance
# 16:   rnd_89115      Entrance
# 17:   rnd_89281          Exit
# 18:   rnd_89289      Entrance
# 19:   rnd_91110          Exit
# 20:   rnd_95341          Exit
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
unique(diffs_dt[volume.diff > 500 & year == 2020, .(r_node_name, r_node_n_type, date)])
# r_node_name r_node_n_type       date
# 1:     rnd_849          Exit 2020-03-15
# 2:     rnd_849          Exit 2020-03-16
# 3:     rnd_849          Exit 2020-03-17
# 4:     rnd_849          Exit 2020-03-18
# 5:     rnd_849          Exit 2020-03-19
# 6:     rnd_849          Exit 2020-03-20
# 7:   rnd_86283      Entrance 2020-01-01
# 8:   rnd_86283      Entrance 2020-01-02
# 9:   rnd_88037      Entrance 2020-03-14
# 10:   rnd_88037      Entrance 2020-03-15
# 11:   rnd_88037      Entrance 2020-03-17
# 12:   rnd_88037      Entrance 2020-03-18
# 13:   rnd_89451      Entrance 2020-03-22

# get rid of thse as well
diffs_dt <- diffs_dt[!r_node_name %in% unique(diffs_dt[volume.diff > 500 & year == 2020, r_node_name])]
diffs_dt[, date := as.IDate(date)]
# Trim to after March 1 2020:
diffs_dt <- diffs_dt[date >= "2020-03-01", ]

fwrite(diffs_dt, paste0("output/pred-and-act-vol-by-node-", Sys.Date(), ".csv"))
fwrite(diffs_dt, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-by-node-", Sys.Date(), ".csv"))


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


fwrite(diffs_4plot, paste0("output/pred-and-act-vol-region-", Sys.Date(), ".csv"))
fwrite(diffs_4plot, paste0("covid.traffic.trends/data-raw/pred-and-act-vol-region-", Sys.Date(), ".csv"))
