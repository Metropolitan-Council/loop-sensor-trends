###########################
library(ggplot2)
library(plotly)
library(data.table)

load('councilcolors.Rdata')
##########################

############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

diffs_4plot <- fread(paste0('output/pred-and-act-vol-region-', Sys.Date(), '.csv'))
diffs_4plot[,date:=as.IDate(date)]

###################################

# MN state actions ####
actions <- cbind(
  date = c('2020-03-06', #MN Dept. of Health\n confirms 1st COVID-19 case in MN
           '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-15', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           '2020-03-18', #Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
           '2020-03-25'),#Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
  action = c('MN Dept. of Health\n confirms 1st COVID-19 case in MN', 
             'UMN Suspends\nIn-Person Classes', 
             'Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl', 
             'Gov. Walz & MDH announce\npublic schools\nwill close by Mar. 18',
             'Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out',
             'Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27')
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[,arrow_start:=5]
actions[,arrow_end:=`Difference from Typical VMT (%)`-0.1]
# diffs_4plot_long2 <- merge(diffs_4plot_long, actions, all = T)

actions[,arrow_start:=c(10, 8, 0, -40, -60, -30)]

# Static plot
ggplot(diffs_4plot[doy>61 & year == 2020], 
       aes(x = date, y = (`Difference from Typical VMT (%)`), group = year,
           text = paste0("<br>", weekday, " ", monthday,
                         '<br>', round(vmt.predict/hh_total, 1), ' Typical VMT per household', 
                         '<br>', round(vmt.sum/hh_total, 1), ' Actual VMT per household', 
                         '<br>', difference_text)))+
  # geom_vline(data = actions, aes(xintercept = as.numeric(date)), color = 'gray50', linetype = 'dashed')+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_y_continuous(limits = c(-55, 10), breaks = seq(from = -50, to = 10, by = 10))+
  geom_segment(data = actions, 
               aes(x = date, xend = date, y = arrow_start, yend = arrow_end), 
               arrow = arrow(angle = 15, length = unit(0.2, 'inches'), type = 'closed'), color = councilBlue)+
  geom_text(data = actions,
            aes(x = date, y = arrow_start, 
                label = action), 
            hjust = 'inward',color = councilBlue, size = 4)+
  geom_text(data = actions,
              aes(x = date, y = arrow_start*-0.5, 
                  label = gsub(pattern =" than typical", replacement = "\ntraffic", x = difference_text)), 
            hjust = 'inward',
            vjust = 'inward', color = councilBlue, size = 6)+
  geom_point(data = diffs_4plot[diffs_4plot$date %in% actions$date,], color = councilBlue, size = 3)

actions_plot <-
  ggplot(diffs_4plot[doy>61 & year == 2020], 
         aes(x = date, y = (`Difference from Typical VMT (%)`), group = year,
             text = paste0("<br>", weekday, " ", monthday,
                           '<br>', round(vmt.predict/hh_total, 1), ' Typical VMT per household', 
                           '<br>', round(vmt.sum/hh_total, 1), ' Actual VMT per household', 
                           '<br>', difference_text)))+
  geom_vline(data = actions, aes(xintercept = as.numeric(date)), color = 'gray50', linetype = 'dashed')+
  # geom_segment(data = actions, aes(xend = date, yend = (arrow_end/1000)))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "% difference from typical traffic")+
  scale_y_continuous(breaks = seq(from = -50, to = 10, by = 10))+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-06'), group = year,
                                 y = 7, label = "MN Dept. of Health\nconfirms 1st COVID-19\ncase in MN"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-11'), group = year,
                                 y = 5, label = "UMN Suspends\nIn-Person Classes"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-13'), group = year,
                                 y = -10, label = "Gov. Walz declares peacetime emergency;\ncalls for cancellation of events >250 ppl"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-15'), group = year,
                                 y = -30, label = "Gov. Walz announces\npublic schools\nwill close by Mar. 18"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-18'), group = year,
                                     y = -40, label = "Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')+
  geom_text(color = councilBlue, aes(size = 3, x = as.Date('2020-03-25'), group = year,
                                     y = -40, label = "Gov. Walz announces a stay-at-home order\nwill take effect Mar. 27"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')
  # geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

  
actions_plot <- plotly::ggplotly(actions_plot, 
                                 tooltip = 'text')%>%
  style(textposition = "left")
actions_plot


library(htmlwidgets)
setwd("output")
saveWidget(plotly_diff, file=paste0("metro-area-vmt-percent-", Sys.Date(), ".html"))
saveWidget(plotly_diff_actual, file=paste0("metro-area-vmt-actuals-", Sys.Date(), ".html"))
saveWidget(actions_plot, file=paste0("metro-area-vmt-percent-actions-", Sys.Date(), ".html"))

