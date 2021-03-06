
library(ggplot2)
library(plotly)
library(data.table)

load('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/councilcolors.Rdata')
##########################

#Connecting to the database: the other 25% of the battle -------------------------------
connect.string = '(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))'
tbidb = ROracle::dbConnect(dbDriver("Oracle"), 
                           dbname = connect.string, 
                           username = 'mts_planning_data', # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
                           password = rstudioapi::askForPassword("database password"))

#Checking system-wide trends-------------------------------
diffs_4plot <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_DAILY_SYSTEM_DIFF")
head(diffs_4plot)
diffs_4plot <- data.table(diffs_4plot)[,date:=substr(DAY, start = 1, stop = 10)]
diffs_4plot[,date:=as.IDate(date, format = "%Y-%m-%d")]
setkey(diffs_4plot, date)
diffs_4plot <- diffs_4plot[date<'2020-10-20']
diffs_4plot[,`Difference from Typical VMT (%)`:=(100*((TOTAL_VOLUME-TOTAL_PREDICTED_VOLUME)/(TOTAL_PREDICTED_VOLUME)))]
diffs_4plot[,date:=as.IDate(date)]
diffs_4plot[,diffvol_use:=ifelse(date %in% c(as.Date('2020-07-03'), as.Date('2020-07-04'), as.Date('2020-09-07')), NA, `Difference from Typical VMT (%)`)]
diffs_4plot[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'left', na.rm = T))]



ggplot(diffs_4plot, aes(x = date))+
  geom_point(aes(y = TOTAL_PREDICTED_VOLUME, color = "Predicted VMT"))+
  geom_line(aes(y = TOTAL_PREDICTED_VOLUME, color = "Predicted VMT"))+
  geom_point(aes(y = TOTAL_VOLUME, color = "Actual VMT"))+
  geom_line(aes(y= TOTAL_VOLUME, color = "Actual VMT"))+
  scale_x_date(date_breaks = "months")+
  scale_color_manual(values = c(mtsRed, 'black')) + 
  theme_minimal()

#########################
# MNDOT Traffic Trends
yesterday <- as.Date('2020-10-25')
# yesterday <- Sys.Date() - 1# change back to -1 when new data available
# yesterday <- as.IDate(yesterday)
# yesterday <- paste0(month(yesterday), "-", mday(yesterday), "-", year(yesterday))
# yesterday <- format(yesterday, format = '%m-%d-%Y')
# mndotdat <- fread(paste0("http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_", yesterday, "_update.csv"))

mndotdat <- fread(paste0('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/data/mndot-data/Daily_Volume_Change_', yesterday, '_update.csv'), 
                  header = T)
mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%Y-%m-%d")]
fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/diff-vol-state.csv"), row.names = F)
fwrite(mndotdat, paste0("N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/diff-vol-state.csv"), row.names = F)
mndotdat[,date:=as.IDate(date)]

mndotdat[,diffvol_use:=ifelse(date %in% c(as.Date('2020-07-03'), as.Date('2020-07-04'), as.Date('2020-09-07')), NA, `Difference from Typical VMT (%)`)]
mndotdat[,rollingavg:=shift(frollapply(diffvol_use, 7, mean, align = 'left', na.rm = T))]

###################################
# MN state actions ####
actions <- cbind(
  date = c(
    # '2020-03-06', #1st Confirmed\nCOVID-19 case in MN
           # '2020-03-11', #UMN Suspends\nIn-Person Classes
           # '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           # '2020-03-18', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           # '2020-03-18', #Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
           # '2020-04-12',
           '2020-03-28', #Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
           # '2020-04-27', # Gov. Walz announces 
           '2020-05-18'
           ),
           # ,  as.character(Sys.Date()-1)),
  action = c(
    # '1st Confirmed\nCOVID-19 case in MN', 
             # 'UMN Suspends\nIn-Person Classes', 
             # 'Peacetime\nemergency declared', 
             # 'Public schools closed;\nIn-person dining suspended',
             # 'Gov. Walz & MDH ask all gyms, bars, public spaces\n to close,restaurants limit to take-out',
             # 'Easter Sunday\nSnowstorm',
             '"Stay-at-home"\norder takes effect',
             # 'Some\nworkplaces\nre-open',
             '"Stay-at-home"\norder expires;"Stay-safe"\norder takes effect'
  )
             # , '')
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[,arrow_end:=`Difference from Typical VMT (%)`-0.1]
actions[,arrow_start:=c(
  # -5, -16, 
  # -27, 
  # -38, 
   # -49, 
  # -80, 
  -85, 
  # -75,
  -70
)]
  # , NA)]

# add logo
# library(png)
# mypng <- readPNG('MetcLogo4C-360x265.png')

# Static plot
static_plot <-
  ggplot(diffs_4plot, 
         aes(x = date, y = (`Difference from Typical VMT (%)`), color = 'MnDOT Metro Freeways\n(1000+ Stations)\n'))+

  # shaded rectangle for stay-at-home order:
  annotate("rect", xmin = as.Date('2020-03-28'), xmax = as.Date('2020-05-18'), ymin = -Inf, ymax = Inf, 
           alpha = .15)+
  
  # shaded rectangle for stay-safe order:
  # annotate("rect", xmin = as.Date('2020-05-18'), xmax = Sys.Date()+3, ymin = -Inf, ymax = Inf, 
  #          fill = councilBlue, alpha = .15)+
  
  # horizontal line at zero:
  geom_hline(yintercept = 0)+
   
  # annotations - actions
  # geom_segment(data = actions, 
  #              aes(x = date, xend = date, y = arrow_start+1, yend = arrow_end), 
  #              arrow = arrow(angle = 20, length = unit(0.75, 'lines'), type = "closed"), color = councilBlue)+
  # geom_text(data = actions,
  #           aes(x = date, y = arrow_start, 
  #               label = paste0(format(date, '%b %d'), ": ", action)), 
  #           hjust = 'right', vjust = 'top', color = councilBlue, size = 4)+
  
  # lines and points for MnDOT: 
  geom_point(data = mndotdat[mndotdat$date > '2020-03-06',], size = 1)+
  geom_point(data = mndotdat[mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 1)+
  # geom_line(data = mndotdat[mndotdat$date > '2020-03-06',], aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 0.5, linetype = 'dotted', show.legend = F)+
  geom_line(data = mndotdat[mndotdat$date > '2020-03-06',], aes(y = rollingavg, color = 'MnDOT Statewide\n(105 Stations)\n'), size = 1, show.legend = F)+
  
  # lines and points for Metro:
  geom_point(size = 1)+
  # geom_line(size = 0.5, linetype = 'dotted', show.legend = F)+
  geom_line(aes(y = rollingavg), size = 1, show.legend = F)+
  # # large points for State: 
  # geom_point(data = mndotdat[date %in% actions$date], pch = 21, fill = 'white', color = 'black',size = 11, show.legend = F)+
  # geom_text(data = mndotdat[date %in% actions$date & !date == '2020-03-06'],
  #           aes(x = date, y = `Difference from Typical VMT (%)`, 
  #               label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
  #           # hjust = 'center',vjust = -1.35, 
  #           color = 'black', size = 3.7, fontface = 'italic')+
  # 
  # # large points for Metro: 
  # geom_point(data = diffs_4plot[date %in% actions$date], pch = 21, fill = 'white', size = 11, show.legend = F)+
  # geom_text(data = actions,
  #           aes(x = date, y = arrow_end, 
  #               label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
  #           # hjust = 'center', vjust = -0.7, 
  #           color = councilBlue, size = 4, fontface = 'italic')+
  
  # global options: 
  theme(legend.position = 'right')+
  theme(panel.grid.major.x = element_line(color = 'gray90'),
        panel.grid.major.y = element_line(color = 'gray90'))+
  # ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
  # axes: 
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_x_date(breaks = seq(as.Date('2020-03-08'), Sys.Date()+3,by="2 weeks"),
               date_labels = '%m/%d',
               limits = c(as.Date('2020-03-06'), Sys.Date()+3))+
  scale_y_continuous(limits = c(-70, 15), breaks = seq(from = -70, to = 10, by = 10))+
  #  colors:
  scale_color_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")+
  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.justification = 'center') + 
  theme_minimal() +
  theme(legend.position = 'right') +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
  
   # logo
  # annotation_raster(mypng, ymin = -95, ymax= -70,xmin = as.numeric(as.Date('2020-03-06')),xmax = as.numeric(as.Date('2020-03-15')))


static_plot
ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/traffic-trends-actions1.png',static_plot, width = 10, height = 4, bg = "transparent")
ggsave('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/inst/app/www/traffic-trends-actions.png',static_plot, height = 7, width = 10, units = 'in', dpi = 300)
pdf('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/traffic.pdf', width =10, height = 4, family = "ArialMT")
static_plot
dev.off()

pdf('C:/Users/AsmusAL/OneDrive - Metropolitan Council/TBIHouseholdSurvey/CovidPanel/PresentationFigures/traffic.pdf', width = 10, height = 4, bg = "transparent", family = "ArialMT")
static_plot
dev.off()

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



# aggregate to weekly scale
weekly_mndot <- weekly_mndot[,lapply(.SD, mean),
                             .SDcols = 'Difference from Typical VMT (%)',
                             by = .(woy)]
weekly_diffs <- weekly_diffs[,lapply(.SD, mean),
                             .SDcols = 'Difference from Typical VMT (%)',
                           by = .(woy)]
weekly_mndot[,`Traffic Sensor Group`:='MnDOT Statewide\n(105 Stations)\n']
weekly_diffs[,`Traffic Sensor Group`:='MnDOT Metro Freeways\n(1000+ Stations)\n']

weekly_dat <- rbind(weekly_mndot, weekly_diffs)

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
  scale_fill_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")
    # ggtitle(paste0("Weekly Average Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
   # scale_x_date(date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  # annotation_raster(mypng, ymin = 2, ymax= 20,xmin = 7, xmax = 9)
plot2

ggsave(paste0('N:/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/weekly-traffic-trends.png'),plot2, height = 7, width = 10, units = 'in', dpi = 300)


