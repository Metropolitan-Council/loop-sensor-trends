##############
library(data.table)
library(foreach)
library(doParallel)
library(mgcv)
#############


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
  
  # store difference from normal for 2020 for mapping
  this_diff <- this_dat
  diffs_ls[[s]]<-this_diff
}

diffsdt <- rbindlist(diffs_ls)


# some very high numbers?
hist(diffsdt[,volume.diff])
summary(diffsdt[,volume.diff])

# diffsdt<-diffsdt[volume.diff > (-100)]
# negative predicted values -- one spurious node
diffsdt <- diffsdt[!r_node_name == 'rnd_86223']
# other spurious observations (8 of them)
diffsdt<-diffsdt[volume.diff < (100)] # 2020, 1 obs

fwrite(diffsdt, paste0('data/predicted-and-actual-volumes-', Sys.Date(), '.csv'))
