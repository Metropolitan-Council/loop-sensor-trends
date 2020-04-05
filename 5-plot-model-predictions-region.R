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

#########################
# MNDOT Traffic Trends
yesterday <- Sys.Date() - 1 # change back to -1 when new data available
yesterday <- as.IDate(yesterday)
yesterday <- paste0(month(yesterday), "-", mday(yesterday), "-", year(yesterday))

# mndotdat <- fread(paste0("http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_", yesterday, "_update.csv"))

mndotdat <- fread('data/Daily_Volume_Change_4-4-2020_update.csv')

mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"), variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%m/%d/%Y")]

fwrite(mndotdat, paste0("output/diff-vol-state.csv"), row.names = F)
mndotdat[,date:=as.IDate(date)]
###################################

# MN state actions ####
actions <- cbind(
  date = c('2020-03-06', #1st Confirmed\nCOVID-19 case in MN
           '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-15', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           '2020-03-18', #Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
           '2020-03-22',
           '2020-03-28', #Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
           as.character(Sys.Date()-1)),
  action = c('1st Confirmed\nCOVID-19 case in MN', 
             'UMN Suspends\nIn-Person Classes', 
             'Gov. Walz declares peacetime emergency;\ncalls for cancellation of events >250 ppl', 
             'Gov. Walz & MDH announce public schools\nwill close by Mar. 18',
             'Gov. Walz & MDH ask all gyms, bars, public spaces\n to close,restaurants limit to take-out',
             '',
             'MN "Stay-at-home" order\ntakes effect',
             '')
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[,arrow_end:=`Difference from Typical VMT (%)`-0.1]
actions[,arrow_start:=c(-5, -16, -27, -38, -49, NA, -80, NA)]

# add logo
library(png)
mypng <- readPNG('MetcLogo4C-360x265.png')

# Static plot
static_plot <-
  ggplot(diffs_4plot[doy>59 & year == 2020], 
         aes(x = date, y = (`Difference from Typical VMT (%)`), color = 'MnDOT Metro Freeways\n(1000+ Stations)\n'))+

  # shaded rectangle for stay-at-home order:
  annotate("rect", xmin = (as.Date('2020-03-28')), xmax = Sys.Date(), ymin = -Inf, ymax = Inf, 
           alpha = .15)+
  
  # horizontal line at zero:
  geom_hline(yintercept = 0)+
   
  # annotations - actions
  geom_segment(data = actions, 
               aes(x = date, xend = date, y = arrow_start+1, yend = arrow_end), 
               arrow = arrow(angle = 20, length = unit(0.75, 'lines'), type = "closed"), color = councilBlue)+
  geom_text(data = actions,
            aes(x = date, y = arrow_start, 
                label = paste0(format(date, '%b %d'), ": ", action)), 
            hjust = 'right', vjust = 'top', color = councilBlue, size = 4)+
  
  # series for MnDOT: 
  geom_point(data = mndotdat,aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 3)+
  geom_line(data = mndotdat, aes(color = 'MnDOT Statewide\n(105 Stations)\n'), size = 1, show.legend = F)+
  geom_point(data = mndotdat[date %in% actions$date], pch = 21, fill = 'white', color = 'black',size = 11, show.legend = F)+
  geom_text(data = mndotdat[date %in% actions$date & !date == '2020-03-06'],
            aes(x = date, y = `Difference from Typical VMT (%)`, 
                label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
            # hjust = 'center',vjust = -1.35, 
            color = 'black', size = 3.7, fontface = 'italic')+
  
  # series for metro: 
  geom_point(size = 3)+
  geom_line(size = 1, show.legend = F)+
  geom_point(data = diffs_4plot[date %in% actions$date], pch = 21, fill = 'white', size = 11, show.legend = F)+
  geom_text(data = actions,
            aes(x = date, y = arrow_end, 
                label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
            # hjust = 'center', vjust = -0.7, 
            color = councilBlue, size = 4, fontface = 'italic')+
  
  # global options: 
  theme_minimal()+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
  # axes: 
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_x_date(date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  scale_y_continuous(limits = c(-85, 15), breaks = seq(from = -80, to = 10, by = 10))+
  #  colors:
  scale_color_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")+
  
   # logo
  annotation_raster(mypng, ymin = 2, ymax= 20,xmin = as.numeric(as.Date('2020-03-23')),xmax = as.numeric(as.Date('2020-03-27')))



ggsave(paste0('output/traffic-trends-actions.png'),static_plot, height = 7, width = 13, units = 'in', dpi = 300)
ggsave(paste0('covid.traffic.trends/inst/app/www/traffic-trends-actions.png'),static_plot, height = 7, width = 13, units = 'in', dpi = 300)


diffs_4plot$woy <- ifelse(diffs_4plot$date >= '2020-03-29', 'This week', 'Last week')
diffs_4plot$weekday <- factor(diffs_4plot$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

mndotdat[,date:=as.IDate(date)]
mndotdat$woy <- ifelse(mndotdat$date >= '2020-03-29', 'This week', 'Last week')
mndotdat[,weekday:=weekdays(date)]
mndotdat$weekday <- factor(diffs_4plot$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))


plot2<-
  ggplot(diffs_4plot[date >= '2020-03-22'
                     & weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),],
         aes(x = woy,
             y = (`Difference from Typical VMT (%)`), fill = 'MnDOT Metro Freeways\n(1000+ Stations)\n'))+
  theme_minimal()+
  facet_grid(~weekday)+
  geom_bar(stat = 'identity', position = 'dodge', width =0.8)+
  geom_hline(yintercept = 0)+
    scale_y_continuous(limits = c(-55,0), breaks = seq(from = -50, to = 0, by = 10))+
    scale_x_discrete(position = 'top')+
  cowplot::theme_cowplot()+
  theme(legend.position = 'right')+
  labs(x = "Day", y = "% difference from typical traffic")+
  geom_text(aes(label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"),'%')),
            vjust = 'outward', size = 3.7)+
  scale_fill_manual(values = c(councilBlue, 'black'), name = "Traffic Sensor Group")+
    ggtitle(paste0("Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
   # scale_x_date(date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  annotation_raster(mypng, ymin = 2, ymax= 20,xmin = 5, xmax = 6)

ggsave(paste0('output/this-week-and-last.png'),plot2, height = 7, width = 10, units = 'in', dpi = 300)


