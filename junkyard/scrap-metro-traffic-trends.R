##############
library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(jsonlite)
library(stringr)
library(timeDate)
library(lubridate)
library(rlang)
library(parallel)
library(data.table)
library(foreach)
library(doParallel)
library(data.table)
library(grid)
library(gridExtra)
library(mgcv)
library(sf)
library(leaflet)
library(geojsonio)
load("councilcolors.Rdata")
#############

# volume: number of cars that pass over the detector
# check with tony on volume
# occupancy is the amount of time that the detector is occupied
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


sensors <- list.files('data/data_hourly_20200318')
sensors <- rev(sort(sensors)) # puts newest data on top
rawdat <- do.call("rbind", foreach(i = sensors) %dopar% {
  library(data.table)
  fread(paste0("data/data_hourly_20200318", "/", i))
})
stopCluster(cl)

hourlydat <- copy(rawdat)

# don't need occupancy data, only volume data: 
hourlydat[,c('occupancy.nulls', 'occupancy.sum', 'occupancy.mean', 'occupancy.median'):=NULL]


# Find duplicate observations -- multiple pulls are written into the same data file
# example  -- hourlydat[date == '2020-03-19' & sensor == 100]
# get only the second observation:
hourlydat <- hourlydat[!duplicated(hourlydat, by = c('date', 'hour', 'sensor'), fromLast=TRUE)]

# for whole days with no observations, "hour" is NA. fill these in properly
hourlydat <- hourlydat[!is.na(hour)]

# create master matrix of dates and sensors
master_mat <- expand.grid(date = unique(hourlydat$date), 
                          sensor = unique(hourlydat$sensor),
                          hour = 0:23)
master_mat <- data.table(master_mat)
master_mat <- master_mat[,lapply(.SD, as.character), .SDcols = c('date', 'sensor'), by = 'hour']

# fills in NA values for missing observations
hourlydat <- merge(hourlydat, master_mat, by = c('date', 'sensor', 'hour'), all = T)

# dealing with date
hourlydat[,date:=as.IDate(fast_strptime(date, "%Y-%m-%d"))] 
hourlydat[,year:=year(date)]

# get rid of data for today, it's incomplete
# hourlydat <- hourlydat[date < Sys.Date()]

# gapfilling: sometimes a few observations were missing within an hour (volume.nulls). 
# each hour had 120 30-second observation windows.
# calculate a new volume with the average volume for that hour, as long as there are at least 15 minutes of data (less than 90 nulls)
hourlydat[,volume.sum.raw:=volume.sum]
hourlydat[,volume.sum := ifelse(volume.nulls <=90, round(volume.sum.raw + volume.nulls * volume.mean), volume.sum.raw)]

# if more than 90 nulls (>45 minutes missing), call it NA for that hour:
hourlydat[,volume.sum := ifelse(volume.nulls>90, NA, volume.sum)]

# hist(hourlydat$volume.sum)
# hourlydat[volume.sum>3000] # greater than 3000 cars per hour? 
# 3000/60 # that's 50 cars per minute!
hourlydat[,exceeds_max:=ifelse(volume.sum>max_volume, 1, 0)]

# exclude these (0.06% of observations)
hourlydat <- hourlydat[,volume.sum:=ifelse(volume.sum>=3000, NA, volume.sum)]

# 0 cars per hour? 
# hist(hourlydat[volume.sum == 0, date], breaks = 'weeks')

hourlydat[,c('volume.nulls', 'volume.mean', 'volume.median', 'volume.sum.raw'):=NULL]

# gapfilling2: if a whole hour is missing, fill in with average of the two hours on either side of it
setorder(hourlydat, sensor, year, date, hour)
hourlydat[,`:=`(volume.sum.rollmean = shift(frollapply(volume.sum, 3, mean, align = 'center', na.rm = T, hasNA = T))), 
          by = .(sensor, year)]
hourlydat[,volume.sum:=ifelse(is.na(volume.sum), volume.sum.rollmean, volume.sum)]
# hist(hourlydat[is.na(volume.sum), hour]) # for some reason a lot of NAs at 0:00 (12 AM). wonder why...?
# A full ten percent of 12 AM observations missing??
# nrow(hourlydat[is.na(volume.sum) & hour == 0])/nrow(hourlydat[hour ==0])
# Best to delete
hourlydat <- hourlydat[hour > 0]
hourlydat[,c('volume.sum.rollmean'):=NULL]


# Aggregate to nodes (instead of sensors)
config <- fread('Configuration of Metro Detectors 2020-03-19.csv')
config$date<-NULL
hourlydat <- merge(hourlydat, config, all.x = T, all.y = F, 
                   by.x = 'sensor', by.y = 'detector_name')

# count number of lanes with data for each hour, at node
hourlydat[,num_lanes_with_data:=sum(!is.na(volume.sum)), 
          by = .(r_node_name, r_node_lanes, r_node_lon, r_node_lat, 
                 corridor_route, corridor_dir,
                 year, date, hour)]

hist(hourlydat[,num_lanes_with_data-r_node_lanes], 
     breaks = 2*max(abs(hourlydat[,num_lanes_with_data-r_node_lanes])))

# sometimes more lanes than the node generally has. sometimes less. Confusing!
hourlydat[,num_sensors_this_year:=uniqueN(sensor), 
          by = .(r_node_name, year)]
hourlydat[num_sensors_this_year>6,]

hist(hourlydat$num_sensors_this_year - hourlydat$num_lanes_with_data)
# Yes, I believe this number of sensors instead

# exclude to only those hourly observations at each node where the number of lanes
# equals the number of observations
hourlydat <- hourlydat[num_lanes_with_data == num_sensors_this_year]

hourlydat <- hourlydat[,lapply(.SD, sum), 
                       .SDcols = 'volume.sum',
                       by = .(r_node_name, r_node_lon, r_node_lat, 
                              corridor_route, corridor_dir,
                              year, date, hour,
                              num_lanes_with_data, num_sensors_this_year)]



# sum for all hours of the day (daily-scale data)
dailydat <- hourlydat[,lapply(.SD, sum), .SDcols = 'volume.sum', 
                      by = .(r_node_name, r_node_lon, r_node_lat, 
                             num_sensors_this_year,
                             corridor_route, corridor_dir,
                             year, date)]


# Dealing with date
dailydat[,date:=as.IDate(date)]
dailydat[,dow:=wday(date)]
dailydat[,doy:=yday(date)]
dailydat[,year:=year(date)]
dailydat[,woy:=week(date)]
dailydat[,weekday:=factor(weekdays(date))]
dailydat[,monthday:=format(date, "%b %d")]


# get rid of 2017 data:
dailydat <- dailydat[year>2017,]

# get rid of december data:
dailydat <- dailydat[doy<100,]

dim(dailydat) # 348203     14


ggplot(dailydat, aes(x = volume.sum, fill = factor(year)))+
  geom_density(alpha = 0.5)
# some very high numbers
dailydat[volume.sum>num_sensors_this_year * 23 * 2000] # reasonable though

# no such thing as 0 daily volume
dailydat <- dailydat[volume.sum>0]

# must have 3 years of data, at least 60 days of data
dailydat[,'num_days_per_year':=uniqueN(date), by = .(r_node_name, year)]
dailydat <- dailydat[num_days_per_year>60]
has_2020_data <- unique(dailydat$r_node_name[dailydat$year == 2020])
has_2018_data <- unique(dailydat$r_node_name[dailydat$year == 2018])
has_2019_data <- unique(dailydat$r_node_name[dailydat$year == 2019])
dailydat <- dailydat[dailydat$r_node_name %in% has_2020_data
                     & dailydat$r_node_name %in% has_2019_data
                     & dailydat$r_node_name %in% has_2018_data,]
dim(dailydat) # 322241     15



dailydat_s <- split(dailydat, dailydat$r_node_name)

# length(dailydat_s) # 1274
diffs_ls <- vector("list", length(dailydat_s))

# test <- dailydat_s[10:12]
# pdf("plots_by_node_all_20200321.pdf", onefile = T, width = 10, height = 7)

for(s in seq_along(dailydat_s)){
  # print(s)
  # flush.console()
  this_dat <- dailydat_s[[s]]
  # subset to relevant dates: 
  modeling_dat <- this_dat[this_dat$date < '2020-03-01' 
                           & this_dat$doy <= 90 # feed it relevant dates - before april 1
                           & this_dat$doy >1,] # exclude the one major holiday in here - jan 1
  
  modeling_dat <- modeling_dat[!modeling_dat$date == '2020-01-17',] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == '2020-01-18',] # cold snap - exclude
  modeling_dat <- modeling_dat[!modeling_dat$date == '2020-02-09',] # cold snap - exclude
  unique(modeling_dat$date)
  this_gam <- with(modeling_dat,
                   mgcv::gam(volume.sum ~ 
                               s(dow, k = 7, by = as.factor(year)) # one knot for each day of the week
                             + s(doy) #general seasonal trend, let it vary by year, allow knots to be set by gam
                             + as.factor(year) # intercept for each year
                   ))
  this_dat[,c('volume.predict', 'volume.predict.se'):=cbind(predict.gam(object = this_gam, newdata = this_dat, se.fit = T))]
  # difference from predicted, in %: 
  this_dat[,volume.diff := round( ( (volume.sum - volume.predict) / volume.predict) * 100, 1)]
  
  # predicted_and_observed_plot<-
  # ggplot(this_dat, aes(x = date, y = volume.sum, color = factor(year)))+
  #   theme_minimal()+
  #   geom_ribbon(aes(ymin = volume.predict-volume.predict.se, ymax = volume.predict + volume.predict.se, fill = factor(year)),
  #               alpha = 0.5, color = NA)+
  #   geom_point()+
  #   geom_line()+
  #   facet_wrap(year~., scales = "free_x", nrow = 3)+
  #   scale_x_date(date_breaks = "2 weeks", date_labels = '%b %d')+
  #   geom_hline(yintercept = 0)+
  #   ggtitle(this_dat$r_node_name)+
  #   theme(legend.position = 'none')+
  #   labs(x = "Date", y = "Total Daily Volume, Predicted and Observed")
  # 
  # diff_from_normal_plot<-
  #   ggplot(this_dat, aes(x = date, y = volume.diff, color = factor(year)))+
  #   theme_minimal()+
  #   geom_point()+
  #   geom_line()+
  # facet_wrap(year~., scales = "free", nrow = 3)+
  # scale_x_date(date_breaks = "week", date_labels = '%b %d')+
  # geom_hline(yintercept = 0)+
  # ggtitle(this_dat$r_node_name)+
  # theme(legend.position = 'none')+
  # labs(x = "Date", y = "Difference from Predicted Volume")
  
  # grid.arrange(predicted_and_observed_plot, diff_from_normal_plot, nrow = 1)
  
  # store difference from normal for 2020 for mapping
  this_diff <- this_dat[year == 2020,]
  diffs_ls[[s]]<-this_diff
}

# dev.off()

diffsdt <- rbindlist(diffs_ls)

# some very high numbers?
hist(diffsdt[,volume.diff])
summary(diffsdt[,volume.diff])

# diffsdt<-diffsdt[volume.diff > (-100)]
# negative predicted values -- one spurious node
diffsdt <- diffsdt[!r_node_name == 'rnd_86223']
# other spurious observations (8 of them)
diffsdt<-diffsdt[volume.diff < (100)] # 2020, 1 obs


diffsdt$scl_volume <- scale(diffsdt$volume.predict, center= F)

diffs_sf <- st_as_sf(diffsdt, coords = c('r_node_lon', 'r_node_lat'), crs = 4326)


binpal <- colorBin("RdBu", diffsdt$volume.diff, 
                   bins = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100), 
                   pretty = FALSE, reverse = T)

# hist(diffsdt$volume.predict)
# hist(diffsdt$scl_volume)

node_labels <- sprintf(
  "<strong>%s %s</strong>
  <br>Node ID: %s
      <br>Expected Volume: %s vehicles
      <br>Actual Volume: %s vehicles
      <br>Difference from Expected:<strong> %s %%</strong>",
  diffsdt[,corridor_route ], 
  diffsdt[,corridor_dir], 
  diffsdt[,r_node_name],
  diffsdt[,round(volume.predict)], 
  diffsdt[,volume.sum], 
  diffsdt[,round(volume.diff, 1)])%>% 
  lapply(htmltools::HTML)



length(unique(diffsdt$r_node_name)) # 1179
# diffsdt[,c('start', 'end'):=list(date, date+1)]
# power_geo <- geojsonio::geojson_json(diffsdt,lat="r_node_lat",lon="r_node_lon")
# 
# # add leaflet-timeline as a dependency
# #  to get the js and css
# leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
#   name = "leaflet-timeline",
#   version = "1.0.0",
#   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
#   script = "javascripts/leaflet.timeline.js",
#   stylesheet = "stylesheets/leaflet.timeline.css"
# )
# 
# leaflet(power_geo)%>%
#   addTiles()%>%
#   addTimeline()

# diffs_sf %>%
#   filter(date == '2020-03-18')%>%
#   filter(volume.diff>=-60)%>%
m<-
  leaflet(diffs_sf)%>%
  addProviderTiles('CartoDB.DarkMatter')%>%
  addPolygons(data = t2040shp)%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-21' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-21' ],
                   group = "Sat. March 21")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-20' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-20' ],
                   group = "Fri. March 20")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-19' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-19' ],
                   group = "Thurs. March 19")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-18' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-18' ],
                   group = "Weds. March 18")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-17' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-17' ],
                   group = "Tues. March 17")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-16' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-16' ],
                   group = "Mon. March 16")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-15' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-15' ],
                   group = "Sun. March 15")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-14' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-14' ],
                   group = "Sat. March 14")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-13' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-13' ],
                   group = "Fri. March 13")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-12' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-12' ],
                   group = "Thurs. March 12")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-11' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-11' ],
                   group = "Weds. March 11")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-10' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-10' ],
                   group = "Tues. March 10")%>%
  addCircleMarkers(data = diffs_sf[diffs_sf$date == '2020-03-09' ,],
                   color = ~binpal(volume.diff), stroke = T, fillOpacity = 0.75, 
                   radius = ~5*(scl_volume),
                   label = node_labels[diffs_sf$date == '2020-03-09' ],
                   group = "Mon. March 9")%>%
  addLayersControl(
    baseGroups = c("Mon. March 9", "Tues. March 10", "Weds. March 11", "Thurs. March 12","Fri. March 13","Sat. March 14",
                   "Sun. March 15","Mon. March 16","Tues. March 17", "Weds. March 18", "Thurs. March 19", "Fri. March 20", "Sat. March 21"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~volume.diff, title = "% Change in Traffic")

library(htmlwidgets)
saveWidget(m, file="metro-volumes-2020-03-22.html")


# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx
# Total difference from expected for whole metro area ####
diffs_corr <- diffsdt[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                      .SDcols = c('volume.sum', 'volume.predict'), 
                      by = .(date, dow, doy, year, woy, weekday, monthday)]
diffs_corr[,c("vmt.sum", "vmt.predict"):=list(volume.sum*0.5, volume.predict * 0.5)]
diffs_corr[,'Difference from Typical VMT (%)':=round(100*(vmt.sum-vmt.predict)/vmt.predict, 2)]

diff_from_normal_plot_corr <-
  ggplot(diffs_corr[doy>0], aes(x = date, y = `Difference from Typical VMT (%)`, 
                                group = year,
                                text = paste0('Date: ', date," (", weekday, ")",
                                              '<br>Typical VMT: ', formatC(signif(vmt.predict, 3), format = "d", big.mark = ","), 
                                              '<br>Actual VMT: ', formatC(signif(vmt.sum, 3), format = "d", big.mark = ","), 
                                              '<br>Difference from Typical VMT: ', `Difference from Typical VMT (%)`, " %")))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "week", date_labels = '%b %d\n(%A)')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area Traffic")+
  theme(legend.position = 'none')+
  labs(x = "Date", y = "% Difference from Typical VMT\n(Vehicle Miles Traveled)")+
  geom_text(aes(x = as.Date('2020-01-11'), y = -26, label = "Jan. 17 Cold Snap"))
diff_from_normal_plot_corr

plotly_corr <- plotly::ggplotly(diff_from_normal_plot_corr, tooltip = 'text')

plotly_corr

library(htmlwidgets)
saveWidget(plotly_corr, file="metro-area-vmt-total-2020-03-22.html")

# trends and actual
diffs_corr_long <- melt(diffs_corr[,.(vmt.sum, vmt.predict, date, dow, doy, year, woy, weekday, monthday, `Difference from Typical VMT (%)`)], 
                        id.vars = c('date', 'dow', 'doy', 'year', 'woy', 'weekday', 'monthday', "Difference from Typical VMT (%)"),
                        variable.name = "estimate_type", value.name = "VMT")
diffs_corr_long$estimate_type <- ifelse(diffs_corr_long$estimate_type == "vmt.sum", "Actual", "Typical")

diffs_corr_long[,difference_text:=ifelse(estimate_type == "Actual", 
                                         ifelse(`Difference from Typical VMT (%)` <0, paste0(abs(`Difference from Typical VMT (%)`), " % less than typical"),
                                                paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")),
                                         ifelse(`Difference from Typical VMT (%)` <0, paste0(abs(`Difference from Typical VMT (%)`), " % more than actual"),
                                                paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than actual")))]
                                         

trends_and_actual_plot_corr <-
  ggplot(diffs_corr_long[doy>0], aes(x = date, y = VMT, 
                                     group = estimate_type, color = estimate_type,
                                     text = paste0(estimate_type, ' VMT', 
                                                   "<br>", weekday, " ", monthday,
                                                   '<br>', formatC(signif(VMT, 3), format = "d", big.mark = ","), ' VMT total',
                                                   '<br>', round(VMT/hh_total, 1), ' VMT per household', 
                                                   '<br>', difference_text)))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(councilBlue, 'black'), name = "Estimate\nType")+
  scale_x_date(date_breaks = "week", date_labels = '%b %d')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "Metro Area VMT\n(Vehicle Miles Traveled)")+
  geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e07, label = "Jan. 17\nCold Snap"))+
  geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

trends_and_actual_plot_corr

plotly_corr_actual <- plotly::ggplotly(trends_and_actual_plot_corr, tooltip = 'text')

plotly_corr_actual

saveWidget(plotly_corr_actual, file="metro-area-vmt-total-2020-03-22.html")


# Aggregate to Thrive 2040 Designations

t2040shp <- st_read('data/thrive2040shapefile/ThriveMSP2040CommunityDesignation.shp')
t2040shp <- st_transform(t2040shp, crs = 4326)
mctu <- st_join(diffs_sf, t2040shp, join = st_within)
mctu <- mctu%>%
  group_by(CTU_NAME, date, year, dow, doy, woy, weekday, monthday)%>%
  summarise(avg.diff = mean(volume.diff), total.vol = sum(volume.sum), pred.vol = sum(volume.predict))

mctu2 <- st_join(t2040shp, mctu, on = 'CTU_NAME')
mctu2 <- mctu2%>%filter(!is.na(date))
mctu2df <- data.table(mctu2)
mctu2df$geometry<-NULL
str(mctu2df)
# str(mctu2df)
ctu_labels <- sprintf(
  "<strong>%s</strong>
      <br>Expected Volume: %s vehicles
      <br>Actual Volume: %s vehicles
      <br>Difference from Expected:<strong> %s %%</strong>",
  mctu2df[,CTU_NAME.y],
  mctu2df[,round(pred.vol)],
  mctu2df[,round(total.vol)],
  mctu2df[,round(avg.diff, 1)])%>% 
  lapply(htmltools::HTML)

m2<-
  leaflet(mctu2)%>%
  addProviderTiles('CartoDB.DarkMatter')%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-20' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-20'],
              group = "Fri. March 20")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-19' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-19'],
              group = "Thurs. March 19")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-18' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-18'],
              group = "Weds. March 18")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-17' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-17'],
              group = "Tues. March 17")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-16' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-16'],
              group = "Mon. March 16")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-15' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-15'],
              group = "Sun. March 15")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-14' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-14'],
              group = "Sat. March 14")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-13' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-13'],
              group = "Fri. March 13")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-12' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-12'],
              group = "Thurs. March 12")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-11' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-11'],
              group = "Weds. March 11")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-10' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-10'],
              group = "Tues. March 10")%>%
  addPolygons(data = mctu2[mctu2$date == '2020-03-09' ,],
              color = ~binpal(avg.diff), stroke = T, fillOpacity = 0.75,
              label = ctu_labels[mctu2df$date == '2020-03-09'],
              group = "Mon. March 9")%>%
  addLayersControl(
    baseGroups = c("Mon. March 9", "Tues. March 10", "Weds. March 11", "Thurs. March 12","Fri. March 13","Sat. March 14",
                   "Sun. March 15","Mon. March 16","Tues. March 17", "Weds. March 18", "Thurs. March 19", "Fri. March 20"),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(pal = binpal, values = ~avg.diff, title = "% Change in Traffic")


m2
saveWidget(m2, file="metro-volumes-by-ctu-2020-03-21.html")


# graphs by CTU
mctu2df



diffs_ctu <- mctu2df[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                     .SDcols = c('avg.diff'), 
                     by = .(date, dow, doy, year, woy, weekday, monthday, CTU_NAME.x)]
# diffs_ctu[,c("vmt.sum", "vmt.predict"):=list(volume.sum*0.5, volume.predict * 0.5)]
diffs_ctu[,'Difference from Typical VMT (%)':=round(avg.diff, 1)]
diffs_ctu[,"CTU Name":=CTU_NAME.x]


diff_from_normal_plot_corr <-
  ggplot(diffs_ctu[doy>0], aes(x = date, y = `Difference from Typical VMT (%)`, 
                               color = `CTU Name`,
                               group = `CTU Name`,
                               text = paste0(`CTU Name`,'<br>Date: ', date," (", weekday, ")",
                                             '<br>Difference from Typical VMT: ', `Difference from Typical VMT (%)`, " %")))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "week", date_labels = '%b %d\n(%A)')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area Traffic")+
  labs(x = "Date", y = "Difference from Typical VMT\n(Vehicle Miles Traveled)")+
  geom_text(aes(x = as.Date('2020-01-11'), y = -26, label = "Jan. 17 Cold Snap"), color = 'black')
diff_from_normal_plot_corr

plotly_corr <- plotly::ggplotly(diff_from_normal_plot_corr, tooltip = 'text')

plotly_corr

library(htmlwidgets)
saveWidget(plotly_corr, file="metro-area-vmt-total-by-CTU-2020-03-21.html")




