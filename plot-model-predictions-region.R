###########################
library(ggplot2)
library(plotly)
load('councilcolors.Rdata')
##########################

############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

diffs_4plot <- fread(paste0('data/pred-and-act-vol-for-plotting-wide', Sys.Date(), '.csv'))
diffs_4plot[,date:=as.IDate(date)]
diffs_4plot_long <- fread(paste0('data/pred-and-act-vol-for-plotting-long', Sys.Date(), '.csv'))
diffs_4plot_long[,date:=as.IDate(date)]

###################################

# % difference from normal #####
diff_from_normal_plot <-
  ggplot(diffs_4plot[doy>0 & year == 2020], 
         aes(x = date, y = `Difference from Typical VMT (%)`, 
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
  geom_text(color = 'black', aes(x = as.Date('2020-01-18'), y = -45, label = "Jan. 17\nSnow Storm"))+
  geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = -28, label = "Feb. 9\nSnow Storm"))

plotly_diff <- plotly::ggplotly(diff_from_normal_plot, tooltip = 'text')
# plotly_diff


# trends and actual ####
trends_and_actual_plot <-
  ggplot(diffs_4plot_long[doy>0 & year == 2020], aes(x = date, y = (VMT/1000), 
                                     group = estimate_type, color = estimate_type,
                                     text = paste0(estimate_type, ' VMT', 
                                                   "<br>", weekday, " ", monthday,
                                                   '<br>', formatC(signif(VMT, 3), format = "d", big.mark = ","), ' VMT total',
                                                   '<br>', round(VMT/hh_total, 1), ' VMT per household', 
                                                   '<br>', difference_text)))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(councilBlue, 'black'), name = "")+
  scale_x_date(date_breaks = "week", date_labels = '%b %d')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "Metro Area VMT\n(1000s of Vehicle Miles Traveled)")+
  geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e04, label = "Jan. 17\nSnow Storm"))+
  geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e04, label = "Feb. 9\nSnow Storm"))

plotly_diff_actual <- plotly::ggplotly(trends_and_actual_plot, tooltip = 'text')

# MN state actions ####
actions <- cbind(
  date = c('2020-03-06', #MN Dept. of Health\n confirms 1st COVID-19 case in MN
           '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-15', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           '2020-03-18'),#Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
  action = c('MN Dept. of Health\n confirms 1st COVID-19 case in MN', 
             'UMN Suspends\nIn-Person Classes', 
             'Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl', 
             'Gov. Walz & MDH announce\npublic schools\nwill close by Mar. 18',
             'Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out'
             )
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
actions<-merge(actions, diffs_4plot_long[estimate_type == "Actual Traffic",c("VMT", "date", "difference_text", "estimate_type", "weekday", "monthday")], all.x = T, all.y = F)
actions[,arrow_start:=max(diffs_4plot_long$VMT * 1.2)]
actions[,arrow_end:=VMT-0.1 * arrow_start]
# diffs_4plot_long2 <- merge(diffs_4plot_long, actions, all = T)

actions_plot <-
  ggplot(diffs_4plot_long[doy>61 & year == 2020], 
         aes(x = date, y = (VMT/1000),
             color = estimate_type, group = estimate_type,
             text = paste0(estimate_type, ' VMT', 
                           "<br>", weekday, " ", monthday,
                           '<br>', formatC(signif(VMT, 3), format = "d", big.mark = ","), ' VMT total',
                           '<br>', round(VMT/hh_total, 1), ' VMT per household', 
                           '<br>', difference_text)))+
  geom_vline(data = actions,
             aes(xintercept = as.numeric(date)), color = 'gray50', linetype = 'dashed')+
  # geom_segment(data = actions, aes(xend = date, yend = (arrow_end/1000)))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(councilBlue, 'black'), name = "")+
  scale_x_date(date_breaks = "days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "Metro Area VMT\n(1000s of Vehicle Miles Traveled)")+
  scale_y_continuous(limits = c(0, 30000))+
  geom_text(color = 'black', aes(size = 3, group = "Actual Traffic", x = as.Date('2020-03-06'), 
                                 y = 5000, label = "MN Dept. of Health\nconfirms 1st COVID-19\ncase in MN"),
            size = 4, fontface = "italic", vjust = 'inward', hjust = 'outward')
  # geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

  
actions_plot <- plotly::ggplotly(actions_plot, 
                                 tooltip = 'text')%>%
  style(textposition = "left")
actions_plot

# MN state actions plot -- difference from normal ----
actions <- cbind(
  date = c('2020-03-06', #MN Dept. of Health\n confirms 1st COVID-19 case in MN
           '2020-03-11', #UMN Suspends\nIn-Person Classes
           '2020-03-13', #Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl
           '2020-03-15', #Gov. Walz announces\npublic schools\nwill close by Mar. 18
           '2020-03-18'),#Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out
  action = c('MN Dept. of Health\n confirms 1st COVID-19 case in MN', 
             'UMN Suspends\nIn-Person Classes', 
             'Gov. Walz declares\npeacetime emergency;\ncalls for cancellation\nof events >250 ppl', 
             'Gov. Walz & MDH announce\npublic schools\nwill close by Mar. 18',
             'Gov. Walz & MDH ask\nall gyms, bars, public spaces to close,\nrestaurants limit to take-out'
  )
)

actions <-data.table(actions)
actions[,date:=as.IDate(date)]
test<-merge(actions, diffs_4plot, all = T)
test$is_action <- ifelse(is.na(test$action), "", "MN State Actions")
# diffs_4plot_long2 <- merge(diffs_4plot_long, actions, all = T)

actions_plot <-
  ggplot(test[doy>61 & year == 2020], 
         aes(x = date, y = `Difference from Typical VMT (%)`, 
             group = year,
             text = paste0('Date: ', date," (", weekday, ")",
                           '<br>Typical VMT: ', round(vmt.predict/1000000, 1),
                           ' M (', round(vmt.predict/hh_total), ' miles per household)', 
                           '<br>Actual VMT: ', round(vmt.sum/1000000, 1),
                           ' M (', round(vmt.sum/hh_total), ' miles per household)', 
                           '<br>Difference from Typical VMT: ', round(`Difference from Typical VMT (%)`), " %"),))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "week", date_labels = '%b %d\n(%A)')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area Traffic on MnDOT Roads")+
  theme(legend.position = 'none')+
  labs(x = "Date", y = "% Difference from Typical VMT\n(Vehicle Miles Traveled)")


library(htmlwidgets)
setwd("output")
saveWidget(plotly_diff, file=paste0("metro-area-vmt-percent-", Sys.Date(), ".html"))
saveWidget(plotly_diff_actual, file=paste0("metro-area-vmt-actuals-", Sys.Date(), ".html"))
saveWidget(plotly_diff2, file=paste0("metro-area-vmt-percent-actions-", Sys.Date(), ".html"))

