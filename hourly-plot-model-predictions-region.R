###########################
library(ggplot2)
library(plotly)
library(data.table)
library(mgcv)

load('councilcolors.Rdata')
##########################

############# MODELS #############
gam_list <- readRDS('output/gam-models-hourly-2020-03-27.RData')


############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

hourlydat <- fread('data/data_hourly_bynode_clean.csv')
# only want 2020 data for March: (december 15-31 included in this pull) ----
# hourlydat <- hourlydat[date>='2020-03-01',]

# Dealing with date ----
hourlydat[,date:=as.IDate(date)]
hourlydat[,dow:=wday(date)]
hourlydat[,doy:=yday(date)]
hourlydat[,year:=year(date)]
hourlydat[,woy:=week(date)]
hourlydat[,weekday:=factor(weekdays(date))]
hourlydat[,monthday:=format(date, "%b %d")]

# get rid of 2017 data: (december 15-31 included in this pull) ----
hourlydat <- hourlydat[year>2017,]

# get rid of december data: ----
hourlydat <- hourlydat[doy<100,]

dim(hourlydat) # 17254564       21


# ggplot(hourlydat, aes(x = volume.sum, fill = factor(year)))+
#   geom_density(alpha = 0.5)
# some very high numbers
hourlydat[volume.sum>num_sensors_this_year * 2000] # reasonable though


# must have 3 years of data, at least 60 days of data in each year ----
hourlydat[,'num_days_per_year':=uniqueN(date), by = .(r_node_name, year)]
hourlydat <- hourlydat[num_days_per_year>60]

has_2020_data <- unique(hourlydat$r_node_name[hourlydat$year == 2020])
has_2018_data <- unique(hourlydat$r_node_name[hourlydat$year == 2018])
has_2019_data <- unique(hourlydat$r_node_name[hourlydat$year == 2019])

hourlydat <- hourlydat[hourlydat$r_node_name %in% has_2020_data
                       & hourlydat$r_node_name %in% has_2019_data
                       & hourlydat$r_node_name %in% has_2018_data,]
dim(hourlydat) # 16231608       22

hourlydat_s <- split(hourlydat, hourlydat$r_node_name)

diffs_ls <- vector("list", length(hourlydat_s))
# 2853

############# PREDICTIONS #############

for(s in seq_along(hourlydat_s)){
  print(s)
  flush.console()
  this_dat <- hourlydat_s[[s]]
  this_gam <- gam_list[[s]]
  
  gam_list[[s]] <- this_gam
  
  this_dat[,c('volume.predict', 'volume.predict.se'):=cbind(predict.gam(object = this_gam, 
                                                                        newdata = this_dat, se.fit = T))]
  
  # difference from predicted, n volume:
  this_dat[,volume.diff.raw:=(volume.sum - volume.predict)]
  
  # difference from predicted, in %: 
  this_dat[,volume.diff := round( ( (volume.sum - volume.predict) / volume.predict) * 100, 1)]
  
  # store difference from normal for 2020 for mapping
  this_diff <- this_dat
  diffs_ls[[s]]<-this_diff
}

diffs_dt <- rbindlist(diffs_ls)

# get rid of negative values -- this needs to be better
diffs_dt <- diffs_dt[volume.predict>=0,]
###################################
library(data.table)
diffs_4plot <- diffs_dt[,lapply(.SD, sum), 
                        .SDcols = c('volume.predict', 'volume.sum'),
                        by = .(year, date, hour, dow, doy, woy, weekday, monthday)]
diffs_4plot <- diffs_4plot[date > '2020-03-07']
diffs_4plot[,volume.diff:=volume.predict - volume.sum]

diffs_4plot[,pct_diff:=((volume.sum - volume.predict)/volume.predict) * 100]

# Static plot
diffs_4plot$weekday <- factor(diffs_4plot$weekday, 
                              levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

diffs_4plot$weekofyear <- 
  ifelse(diffs_4plot$date > '2020-03-28', 'This Week',
         ifelse(diffs_4plot$date > '2020-03-21', 'Last Week',
                ifelse(diffs_4plot$date > '2020-03-14', 'Two Weeks Ago', 'Three Weeks Ago')))

diffs_4plot$weekofyear <- factor(diffs_4plot$weekofyear, levels = c('This Week', 'Last Week', 'Two Weeks Ago', 'Three Weeks Ago'))


ggplot(diffs_4plot[diffs_4plot$weekofyear == 'This Week',])+
  geom_line(aes(x = hour, y = volume.sum), color = metroLiPurp)+
  geom_line(aes(x = hour, y = volume.predict), color = metroDaPurp)+
  facet_grid(~weekday)+
  theme_minimal()

static_plot <- 
ggplot(diffs_4plot, 
       aes(x = hour, y = (volume.sum), color = weekofyear))+
  facet_grid(~weekday)+
  theme_minimal()+
  # geom_point(size = 3)+
  geom_line(size = 2)+
  scale_x_continuous(n.breaks = 12)+
  geom_hline(yintercept = 0)+
  ggtitle("Hourly Traffic Trends on MnDOT Roads\nUpdated 3/27/2020")+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  labs(x = "Hour", y = "% difference from typical traffic")+
  scale_color_manual(values = c("black", metroDaPurp, metroMePurp, metroLiPurp),
                     name = "Week of")


# Waterfall Plot
diffs_4plot[,daypart:=ifelse(hour %in% 0:6, '12AM-6AM', 
                             ifelse(hour %in% 7:9, '7AM-9AM', 
                                    ifelse(hour %in% 10:16, '10AM-4PM', 
                                           ifelse(hour %in% 17:19, '4PM-7PM',
                                                  ifelse(hour %in% 20:23, '8PM-11PM', NA)))))]
diffs_4plot_daypart <- diffs_4plot[,lapply(.SD, sum), 
                                   .SDcols = c('volume.sum', 'volume.predict'), 
                                   by = .(daypart, weekday, weekofyear)]

diffs_4plot_daypart[,pct_diff:=((volume.sum - volume.predict)/volume.predict) * 100]

diffs_4plot_daypart[,daypart:=factor(daypart,
                                     levels = c('12AM-6AM', '7AM-9AM', '10AM-4PM', '4PM-7PM', '8PM-11PM'))]

ggplot(diffs_4plot_daypart[weekday %in% c('Sunday', 'Monday') 
                           
                          & weekofyear %in% c('This Week', 'Last Week', 'Two Weeks Ago')], 
         aes(x = daypart, y = (volume.sum), color = weekofyear))+
  facet_grid(~weekday)+
  theme_minimal()+
  # geom_point(size = 3)+
  geom_bar(aes(x = daypart, y = (volume.predict), color = weekofyear), 
           fill = 'white', stat = 'identity', position = 'dodge', linetype = 'dashed')+
  geom_bar(stat = 'identity', position = 'dodge', aes(fill = weekofyear))+

  scale_color_manual(values = c("black", metroDaPurp, metroMePurp, metroLiPurp),
                     name = "Week of")+
  scale_fill_manual(values = c("black", metroDaPurp, metroMePurp, metroLiPurp),
                     name = "Week of")+
  geom_hline(yintercept = 0)
  ggtitle("Hourly Traffic Trends on MnDOT Roads\nUpdated 3/27/2020")+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  labs(x = "Hour", y = "% difference from typical traffic")+

  # scale_color_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")+
  # scale_y_continuous(limits = c(-65, 15), breaks = seq(from = -70, to = 10, by = 10))+
  geom_segment(data = actions, 
               aes(x = date, xend = date, y = arrow_start, yend = arrow_end), 
               arrow = arrow(angle = 20, length = unit(0.75, 'lines'), type = "closed"), color = councilBlue)+
  geom_point(data = diffs_4plot[date %in% actions$date], pch = 21, fill = 'white', size = 11)+
  geom_text(data = actions,
            aes(x = hour, y = arrow_start, 
                label = paste0(format(date, '%b %d'), ": ", action)), 
            hjust = 'right',color = councilBlue, size = 4)+
  geom_text(data = actions,
              aes(x = hour, y = arrow_end, 
                  label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
            # hjust = 'center', vjust = -0.7, 
            color = councilBlue, size = 4, fontface = 'italic')


ggsave( 'output/traffic-trends-actions-hourly.jpeg',static_plot, height = 5, width = 20, units = 'in', dpi = 300)
