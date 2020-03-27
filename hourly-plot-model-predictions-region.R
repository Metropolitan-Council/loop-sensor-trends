###########################
library(ggplot2)
library(plotly)
library(data.table)

load('councilcolors.Rdata')
##########################

############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

diffs_4plot <- fread(paste0('output/hourly-pred-and-act-vol-region-', Sys.Date(), '.csv'))
diffs_4plot[,date:=as.IDate(date)]


# mndotdat <- fread(paste0('output/diff-vol-state-', Sys.Date(), '.csv'))
# mndotdat[,date:=as.IDate(date)]
###################################

# MN state actions ####
actions <- cbind(
  date = c('2020-03-06', #1st Confirmed\nCOVID-19 case in MN
           '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-15', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           '2020-03-18', #Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
           '2020-03-22',
           '2020-03-25'),#Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
  action = c('1st Confirmed\nCOVID-19 case in MN', 
             'UMN Suspends In-Person Classes', 
             'Gov. Walz declares peacetime emergency;\ncalls for cancellation of events >250 ppl', 
             'Gov. Walz & MDH announce public schools\nwill close by Mar. 18',
             'Gov. Walz & MDH ask all gyms, bars, public spaces\n to close,restaurants limit to take-out',
             '',
             'Gov. Walz announces a "stay-at-home" order will take effect Mar. 27')
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[,arrow_start:=5]
actions[,arrow_end:=`Difference from Typical VMT (%)`-0.1]
# diffs_4plot_long2 <- merge(diffs_4plot_long, actions, all = T)

actions[,arrow_start:=c(-7, -14, -23, -42, -52, NA, -60)]

# Static plot
diffs_4plot$weekday <- factor(diffs_4plot$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

diffs_4plot$weekofyear <- ifelse(diffs_4plot$date > '2020-03-21', 'This Week',
                                 ifelse(diffs_4plot$date > '2020-03-14', 'Last Week',
                                        ifelse(diffs_4plot$date > '2020-03-07', 'Two Weeks Ago', 'Three Weeks Ago')))
diffs_4plot$weekofyear <- factor(diffs_4plot$weekofyear, levels = c('This Week', 'Last Week', 'Two Weeks Ago', 'Three Weeks Ago'))
ggplot(diffs_4plot[diffs_4plot$date == '2020-03-23',], 
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
  scale_color_manual(values = c( metroDaPurp, metroMePurp, metroLiPurp),
                     name = "Week of")

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


ggsave( 'output/traffic-trends-actions.jpeg',static_plot, height = 7, width = 11, units = 'in', dpi = 300)