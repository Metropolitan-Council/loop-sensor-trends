library(readr)
library(lubridate)
library(tidyverse)
library(data.table)
library(sf)
library(leaflet)
library(foreach)
library(doParallel)

translate_vol <- function(file){
  print(file)
  flush.console()
vol_temp <- read_fwf(file, fwf_widths(c(1,2,2,6,1,1,2,2,2,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,1)))
sheet <- vol_temp %>% gather(hour, total, -X1, -X2, -X3, -X4, -X5, -X6, -X7, -X8, -X9) %>% 
  filter(hour != "X35") %>%
  filter(hour != "X10") %>%
  mutate(hour = as.integer(substr(hour,2,3))-10) %>% 
  mutate(total = as.integer(total), X4 = as.integer(X4)) %>% 
  mutate(date = ymd(paste(X7, X8, X9, sep = "/"))) %>% 
  group_by(X4, date, hour) %>% 
  rename(sensor = X4)%>%
  summarise(total = sum(total))
return(sheet)
}


files_list <- list.files(pattern = ".ATR$", recursive = TRUE)
atrdat <- rbindlist(lapply(files_list, translate_vol)) 
# Some issues parsing data: 
# "data/mndot_atr/2020_Jan_atr_wim.ATR"
atrdat[,date:=as.IDate(date)]

# March data in a CSV: 
march_atrdat <- fread('data/mndot_atr/March_2020_Volume.csv')
march_atrdat[,c('sensor'):=tstrsplit(site, "CNT", keep = 2)]
march_atrdat[,sensor:=as.numeric(sensor)]
march_atrdat[,date:=as.IDate(fast_strptime(date, '%Y-%m-%d'))]

# March data needs to be aggregated across lanes, to the sensor level (sum):
summary(march_atrdat) # only 1 NA value.
march_atrdat <- march_atrdat[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)), 
             .SDcols = 'total', 
             by = .(sensor, date)]


# Other ATR data needs to be aggregated to the daily scale, to match march:
summary(atrdat$total) # 144 NAs to worry about here
  unique(atrdat[is.na(atrdat$total), .(sensor, date)])
  # sensor date
  # 1:     47 <NA>
  #   2:    353 <NA>
  #   3:    381 <NA>
  #   4:    382 <NA>
  #   5:    400 <NA>
  #   6:    407 <NA>
atrdat <- atrdat[!is.na(date),]
atrdat <- atrdat[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)), 
                              .SDcols = 'total', 
                              by = .(sensor, date)]


# does this match?
unique(march_atrdat$sensor[!march_atrdat$sensor %in% atrdat$sensor]) # 33 not in ATR data
uniqueN(march_atrdat$sensor[march_atrdat$sensor %in% atrdat$sensor]) # 58 in ATR data
uniqueN(march_atrdat$sensor) # 91 total


# read in shapefile for etra attributes
shp <- st_read('data/mndot_atr/mndot_atr_shapefile/MN_CONTINUOUS_COLLECTION_SITES_20200325.shp')
shp <- st_transform(shp, crs = 4326)
uniqueN(shp$CONT_ID) # 130 of these
uniqueN(atrdat$sensor) # 101 of these
uniqueN(march_atrdat$sensor) # 91 of these

unique(march_atrdat$sensor[!march_atrdat$sensor %in% shp$CONT_ID]) # 5 sensors present in data not present in shapefile
# 1027 1046 7047 9221 9353
shp$CONT_ID[shp$CONT_ID > 1000 & shp$CONT_ID < 1500]
uniqueN(shp$CONT_ID[shp$CONT_ID %in% atrdat$sensor]) # all present


# join up
allatr <- merge(march_atrdat, atrdat, all = T)

# merge in a blank table with dates to find NAs
# create master matrix of dates and sensors, empty of data.
master_mat <- expand.grid(date = unique(allatr$date), 
                          sensor = unique(allatr$sensor))
master_mat <- data.table(master_mat)
# master_mat <- master_mat[,lapply(.SD, as.character), .SDcols = c('date', 'sensor')]

# merge master to hourly data to fill in NA values for missing observations
allatr <- merge(allatr, master_mat, by = c('date', 'sensor'), all = T)

# Dealing with date ----
allatr[, date := as.IDate(date)]
allatr[, dow := wday(date)]
allatr[, doy := yday(date)]
allatr[, year := year(date)]
allatr[, woy := week(date)]
allatr[, weekday := factor(weekdays(date))]
allatr[, monthday := format(date, "%b %d")]


# Now find sensors with enough data to model
# must have 3 years of data, at least 60 days of data in each year ----
allatr[, "num_days_per_year" :=sum(!is.na(total)), by = .(sensor, year)]

allatr$sensor <- as.character(allatr$sensor)

hist(allatr$num_days_per_year)

allatr <- allatr[num_days_per_year > 60]

has_march2020_data <- unique(allatr$sensor[allatr$year == 2020 & allatr$doy > 60])
has_2018_data <- unique(allatr$sensor[allatr$year == 2018])
has_2019_data <- unique(allatr$sensor[allatr$year == 2019])

dailydat <- allatr[allatr$sensor %in% has_march2020_data & 
                   allatr$sensor %in% has_2019_data & 
                   allatr$sensor %in% has_2018_data, ]

uniqueN(dailydat$sensor) # 40 sensors now.

unique(march_atrdat$sensor[!march_atrdat$sensor %in% dailydat$sensor])

# Where are these sensors?
shp_sub <- shp%>%
  filter(shp$CONT_ID %in% unique(dailydat$sensor))


leaflet(shp_sub) %>%
  addProviderTiles("CartoDB.Positron")%>%
  addCircleMarkers()



# now split by sensor
dailydats <- split(dailydat, dailydat$sensor)

# model
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

predictions <- 
rbindlist(foreach(i = dailydats) %dopar% {
  
  library(data.table)
  library(lubridate)
  library(mgcv)
    
  dailydat <- i
  
  # subset to relevant dates:
    modeling_dat <- dailydat[dailydat$date < "2020-03-01",]
    # & this_dat$doy <= 90 # feed it relevant dates - before april 1
    # & this_dat$doy > 1, ] # exclude the one major holiday in here - jan 1
    
    # 2020 data v. sensitive - exclude some special holidays and weather days
    modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-01", ] # cold snap - exclude
    modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-17", ] # cold snap - exclude
    modeling_dat <- modeling_dat[!modeling_dat$date == "2020-01-18", ] # cold snap - exclude
    modeling_dat <- modeling_dat[!modeling_dat$date == "2020-02-09", ] # snow day - exclude
    
    this_gam <- with(
      modeling_dat,
      mgcv::gam(
        total ~
          s(dow, k = 7) # one knot for each day of the week
        + s(doy, bs = 'cs') # general seasonal trend, let it vary by year, allow knots to be set by gam
        + as.factor(year) # intercept for each year
      )
    )
    
    
    # generate predictions
    date_range <- c(seq(as.Date("2018-01-01"), as.Date("2020-05-01"), by = "days"))
    
    predict_dat <- data.table(date = date_range)
    predict_dat[, date := as.IDate(date)]
    predict_dat[, dow := wday(date)]
    predict_dat[, doy := yday(date)]
    predict_dat[, year := year(date)]
    predict_dat[, woy := week(date)]
    predict_dat[, weekday := factor(weekdays(date))]
    predict_dat[, monthday := format(date, "%b %d")]
    predict_dat[, sensor := modeling_dat$sensor[[1]]]
    
   
    predict_dat[, c("volume.predict", "volume.predict.se") := cbind(predict.gam(object = this_gam, newdata = predict_dat, se.fit = T))]
    
    predict_dat <- merge(predict_dat, 
                         dailydat[,.(sensor, date, total)], all.x = T, 
                         by = c('sensor', 'date'))
    
    # difference from predicted, n volume:
    predict_dat[, volume.diff.raw := (total - volume.predict)]
    
    # difference from predicted, in %:
    predict_dat[, volume.diff := round(((total - volume.predict) / volume.predict) * 100, 1)]
    
    # store difference from normal for 2020 for mapping
    predict_dat
    
})

stopCluster(cl)

# some outliers: 
predictions[predictions$volume.diff > 200] # Sensor 209, 222, 231, 386
# get rid of these
predictions <- predictions[volume.diff < 200]


ggplot(predictions[date < '2020-01-01'], aes(x = date, y = volume.predict, color = sensor, group = sensor))+
  geom_smooth(se = F)+
  theme(legend.position = 'none')

# mndot's baseline -- typical weekday, same month, 2019.
predictions[,month:=month(date)]
predictions[year == 2019,volume.predict.2:=mean(total, na.rm = T), 
            by = .(sensor, dow, month, year)]
predictions[,volume.predict.2:=max(volume.predict.2, na.rm = T),
            by = .(sensor, dow, month)]


pretot <- predictions[,lapply(.SD, sum), 
                      .SDcols = c('volume.predict','volume.predict.2', 'total'),
                      by = .(date, dow, doy, year, woy, weekday, monthday, month, sensor)]

pretot <- melt(pretot, id.vars = c('sensor','date', 'dow','doy', 'year', 'woy', 'weekday', 'monthday', 'month', 'total'),
               variable.name = 'Baseline', value.name = 'volume.predict')

pretot$Baseline <- factor(pretot$Baseline, 
                                levels = c('volume.predict', 'volume.predict.2'),
                                labels = c('Modeled, Jan 2018-Feb 2020\n(40 sensors)\n', 'Traffic on same month &\nweekday, 2019 (40 sensors)\n'))
pretot[,volume.diff := ((total - volume.predict)/volume.predict) * 100]

# now average
pretot <- pretot[,lapply(.SD, mean),
       .SDcols = 'volume.diff',
       by = .(date, Baseline)]

statenums <- fread('output/diff-vol-state.csv')
setnames(statenums, old = 'Difference from Typical VMT (%)', new = 'volume.diff')
statenums$District<-NULL
statenums$datasource <- 'MnDOT analysis\n(Difference from same month/weekday 2019)'
statenums[,date:=as.IDate(date)]
# pretot$datasource <- 'MetCouncil analysis\n(Difference from typical traffic 2018-2020)'
# 

# 
# 
# 
# compdat <- merge(pretot, statenums, all = T)

diffs_4plot <- fread(paste0('output/pred-and-act-vol-region.csv'))
diffs_4plot[,date:=as.IDate(date)]
setnames(diffs_4plot, old = 'Difference from Typical VMT (%)', new = 'volume.diff')

ggplot(pretot[date >= '2020-03-01'], aes(x = date, y = volume.diff, color = Baseline))+
  
  # horizontal line at zero:
  geom_hline(yintercept = 0)+
  
  # series for our regional predictsion: 
  geom_point(size = 2)+
  geom_line(size = 0.75) + 

  # # lines and points for Metro:
  # geom_point(data = statenums,aes(color = 'Data from MnDOT (100 sensors)\n'), size = 2)+
  # geom_line(data = statenums, aes(color = 'Data from MnDOT (100 sensors)\n'), size = 0.75, show.legend = F)+
  
  
  # # lines and points for Metro:
  # geom_point(data = diffs_4plot,aes(color = ' Metro Freeways\n'), size = 2)+
  # geom_line(data = diffs_4plot, aes(color = ' Metro Freeways\n'), size = 0.75, show.legend = F)+
  # 
  
  # global options: 
  theme_minimal()+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
  # axes: 
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_x_date(limits = c(as.Date('2020-02-29'), as.Date('2020-04-01')),
    date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  scale_y_continuous(limits = c(-70, 15), breaks = seq(from = -70, to = 10, by = 10))+
  #  colors:
  scale_color_manual(values = c('black', mtsRed, 'gray60'), name = "Baseline")


# curious about this number: 
pretot[date == '2020-03-07']

# march saturdays 2019? 
marchsats <- pretot[month==3 & year %in% c(2018, 2019) & weekday %in% c('Saturday', 'Sunday'), .(total, year, woy, weekday)]

ggplot(marchsats, aes(x = woy, y = total, group = factor(year), fill = factor(year)))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_grid(weekday~.)

marchsats_avg <- marchsats[,lapply(.SD, mean),
                           .SDcols = 'total', 
                           by = .(year, weekday)]
marchsats_avg
(396093.6 - 408225.4)/408225.4
# 3% less traffic on Saturdays in March 2019 relative to 2018

ggplot(marchsats_avg, aes(x = year, y = total, group = factor(weekday), fill = factor(weekday)))+
  geom_bar(stat = 'identity', position = 'dodge')
