

###########################
library(ggplot2)
library(plotly)
library(data.table)

load('councilcolors.Rdata')
##########################



############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

diffs_4plot <- fread(paste0('output/pred-and-act-vol-region.csv'))
diffs_4plot[,date:=as.IDate(date)]



ggplot(diffs_4plot, aes(x = date))+
  geom_point(aes(y = vmt.predict, color = "Predicted VMT"))+
  geom_line(aes(y = vmt.predict, color = "Predicted VMT"))+
  geom_point(aes(y = vmt.sum, color = "Actual VMT"))+
  geom_line(aes(y= vmt.sum, color = "Actual VMT"))+
  scale_x_date(date_breaks = "3 days")+
  scale_color_manual(values = c(mtsRed, 'black'))

#########################
# MNDOT Traffic Trends
yesterday <- Sys.Date() - 1 # change back to -1 when new data available
# yesterday <- as.IDate(yesterday)
# yesterday <- paste0(month(yesterday), "-", mday(yesterday), "-", year(yesterday))
# yesterday <- format(yesterday, format = '%m-%d-%Y')
# mndotdat <- fread(paste0("http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_", yesterday, "_update.csv"))

mndotdat <- fread(paste0('data/Daily_Volume_Change_', yesterday, '_update.csv'))
mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%Y-%m-%d")]
fwrite(mndotdat, paste0("output/diff-vol-state.csv"), row.names = F)
mndotdat[,date:=as.IDate(date)]


old_mndotdat <- fread('data/Daily_Volume_Change_05-05-2020_update.csv')
old_mndotdat <- old_mndotdat[District %in% c("MnDOT Statewide")]
old_mndotdat <- melt(old_mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
old_mndotdat[, date := as.IDate(date, format = "%m/%d/%Y")]
old_mndotdat[,date:=as.IDate(date)]


###################################
# MN state actions ####
actions <- cbind(
  date = c(
    # '2020-03-06', #1st Confirmed\nCOVID-19 case in MN
           # '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-18', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           # '2020-03-18', #Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
           '2020-04-12',
           '2020-03-28', #Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
           '2020-04-27', # Gov. Walz announces 
           as.character(Sys.Date()-1)),
  action = c(
    # '1st Confirmed\nCOVID-19 case in MN', 
             # 'UMN Suspends\nIn-Person Classes', 
             'Peacetime\nemergency declared', 
             'Public schools closed;\nIn-person dining suspended',
             # 'Gov. Walz & MDH ask all gyms, bars, public spaces\n to close,restaurants limit to take-out',
             'Easter Sunday\nSnowstorm',
             '"Stay-at-home"\norder takes effect',
             'Some\nworkplaces\nre-open',
             '')
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[,arrow_end:=`Difference from Typical VMT (%)`-0.1]
actions[,arrow_start:=c(
  # -5, -16, 
  -27, 
  # -38, 
   -49, 
  -80, 
  -85, -60, NA)]

# add logo
library(png)
mypng <- readPNG('MetcLogo4C-360x265.png')

# Static plot
static_plot <-
  ggplot(diffs_4plot[doy>66 & year == 2020], 
         aes(x = date, y = (`Difference from Typical VMT (%)`), color = 'MnDOT Metro Freeways\n(1000+ Stations)\n'))+

  # shaded rectangle for stay-at-home order:
  annotate("rect", xmin = (as.Date('2020-03-28')), xmax = Sys.Date()+1, ymin = -Inf, ymax = Inf, 
           alpha = .15)+
  
  
  # horizontal line at zero:
  geom_hline(yintercept = 0)+
  
  # lines and points for MnDOT: 
  geom_point(data = old_mndotdat[old_mndotdat$date > '2020-03-06',],aes(color = 'MnDOT Statewide:\nPrevious Baseline\n(105 Stations)\n'), size = 3)+
  geom_line(data = old_mndotdat[old_mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide:\nPrevious Baseline\n(105 Stations)\n'), size = 1, show.legend = F)+
  
  # lines and points for MnDOT: 
  geom_point(data = mndotdat[mndotdat$date > '2020-03-06',],aes(color = ), size = 3)+
  geom_line(data = mndotdat[mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide:\nMay 6 2020 Update\n(105 Stations)\n'), size = 1, show.legend = F)+
  
  
  geom_point(size = 3)+
  geom_line(size = 1, show.legend = F)+
  
  # global options: 
  theme_minimal()+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  theme(panel.grid.major.x = element_line(color = 'gray90'),
        panel.grid.major.y = element_line(color = 'gray90'))+
  # ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
  # axes: 
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_x_date(breaks = seq(as.Date('2020-03-08'), as.Date('2020-05-04'),by="week"),
               date_labels = '%m/%d\n(%a)',
               limits = c(as.Date('2020-03-06'), Sys.Date()+1))+
  scale_y_continuous(limits = c(-90, 15), breaks = seq(from = -90, to = 10, by = 10))+
  #  colors:
  scale_color_manual(values = c(councilBlue, 'black', mtsRed), name = "Traffic Sensor Group")
  
   # logo
  # annotation_raster(mypng, ymin = -95, ymax= -70,xmin = as.numeric(as.Date('2020-03-06')),xmax = as.numeric(as.Date('2020-03-15')))


static_plot
ggsave(paste0('covid.traffic.trends/inst/app/www/mndot-comparison.png'),static_plot, height = 7, width = 14, units = 'in', dpi = 300)

### Plot Weekly Trends

weekly_diffs <- diffs_4plot[date > '2020-03-01'
                              & weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')]

weekly_diffs[,woy:=week(date-5)] # adjust to Monday
weekly_diffs$weekday <- factor(weekly_diffs$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

mndotdat[,date:=as.IDate(date)]
mndotdat[,woy:=week(date-5)] # adjust to Monday
mndotdat[,weekday:=weekdays(date)]
mndotdat$weekday <- factor(mndotdat$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

weekly_mndot <- mndotdat[date > '2020-03-01'
                            & weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')]


old_mndotdat[,date:=as.IDate(date)]
old_mndotdat[,woy:=week(date-5)] # adjust to Monday
old_mndotdat[,weekday:=weekdays(date)]
old_mndotdat$weekday <- factor(old_mndotdat$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

old_weekly_mndot <- old_mndotdat[date > '2020-03-01'
                         & weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')]

# aggregate to weekly scale
old_weekly_mndot <- old_weekly_mndot[,lapply(.SD, mean),
                             .SDcols = 'Difference from Typical VMT (%)',
                             by = .(woy)]

weekly_mndot <- weekly_mndot[,lapply(.SD, mean),
                             .SDcols = 'Difference from Typical VMT (%)',
                             by = .(woy)]
weekly_diffs <- weekly_diffs[,lapply(.SD, mean),
                             .SDcols = 'Difference from Typical VMT (%)',
                           by = .(woy)]
old_weekly_mndot[,`Traffic Sensor Group`:='MnDOT Statewide:\nPrevious Baseline\n(105 Stations)\n']
weekly_mndot[,`Traffic Sensor Group`:='MnDOT Statewide:\nMay 6 2020 Update\n(105 Stations)\n']
weekly_diffs[,`Traffic Sensor Group`:='MnDOT Metro Freeways\n(1000+ Stations)\n']

weekly_dat <- rbind(weekly_mndot, old_weekly_mndot, weekly_diffs)

weekly_dat <- merge(weekly_dat, unique(mndotdat[mndotdat$weekday == 'Monday',c('woy', 'date')]))
weekly_dat[,date:=as.IDate(date)]
weekly_dat[,week_of:=format(date, '%b %d')]
weekly_dat[,week_of:=paste0('Week of\n', week_of)]
weekly_dat[,week_of:=factor(week_of, levels = unique(weekly_dat$week_of))]
weekly_dat$woy2 <- format(weekly_dat$woy, "%m-%d")

plot2<-
  ggplot(weekly_dat,  
         aes(x = week_of,
             y = (`Difference from Typical VMT (%)`), fill = `Traffic Sensor Group`))+
  theme_minimal()+
  
  # # shaded rectangle for stay-at-home order:
  # annotate("rect", xmin = 12.5, xmax = 17.5, ymin = -Inf, ymax = Inf,
  #          alpha = .15)+
  
  geom_bar(stat = 'identity', position = 'dodge', width =0.9)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(limits = c(-55,10), breaks = seq(from = -50, to = 0, by = 10))+
  # scale_x_continuous(limits = c(8.5, 17.5), breaks = seq(from = 9, to = 17, by = 1))+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  labs(x = "", y = "% difference from typical traffic")+
  geom_text(aes(y = (`Difference from Typical VMT (%)`),
                label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
            vjust = 1, size = 3.7, hjust = 0.5, position = position_dodge(width = 1), color = 'gray40')+
  scale_fill_manual(values = c(councilBlue,'black', mtsRed), name = "Traffic Sensor Group")
    # ggtitle(paste0("Weekly Average Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
   # scale_x_date(date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  # annotation_raster(mypng, ymin = 2, ymax= 20,xmin = 7, xmax = 9)
plot2

ggsave(paste0('output/weekly-traffic-trends.png'),plot2, height = 7, width = 10, units = 'in', dpi = 300)


