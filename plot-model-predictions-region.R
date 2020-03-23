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
  ggplot(diffs_4plot[doy>0 & year == 2020], aes(x = date, y = `Difference from Typical VMT (%)`, 
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
  ggplot(diffs_4plot_long[doy>0 & year == 2020], aes(x = date, y = VMT, 
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
  labs(x = "Date", y = "Metro Area VMT\n(Vehicle Miles Traveled)")+
  geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e07, label = "Jan. 17\nSnow Storm"))+
  geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

plotly_diff_actual <- plotly::ggplotly(trends_and_actual_plot, tooltip = 'text')

# MN state actions ####
actions <- cbind(
  date = c('2020-03-06', '2020-03-11', '2020-03-13', '2020-03-16', '2020-03-18'),
  action = c('1st COVID-19 Case in MN', 'UMN Suspends Classes', 'Restaurants Closed',
             'Gatherings >250 ppl Banned', 'Public Schools Close')
)
actions <-data.table(actions)
actions[,date:=as.IDate(date)]

diffs_4plot_long2 <- merge(diffs_4plot_long, actions, all = T)

actions_plot <-
  ggplot(diffs_4plot_long2[doy>60 & year == 2020], aes(x = date, y = VMT, 
                                                    group = estimate_type, color = estimate_type,
                                                    text = paste0(estimate_type, ' VMT', 
                                                                  "<br>", weekday, " ", monthday,
                                                                  '<br>', formatC(signif(VMT, 3), format = "d", big.mark = ","), ' VMT total',
                                                                  '<br>', round(VMT/hh_total, 1), ' VMT per household', 
                                                                  '<br>', difference_text, 
                                                                  '<br>', ifelse(is.na(action), "", action))))+
  geom_vline(data = actions, aes(xintercept = as.numeric(date)), color = 'gray50', linetype = 'dashed')+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(councilBlue, 'black'), name = "")+
  scale_x_date(date_breaks = "week", date_labels = '%b %d')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area VMT (Vehicle Miles Traveled) on MnDOT Roads")+
  theme(legend.position = 'right')+
  labs(x = "Date", y = "Metro Area VMT\n(Vehicle Miles Traveled)")
  # geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e07, label = "Jan. 17\nSnow Storm"))+
  # geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

actions_plot <- plotly::ggplotly(actions_plot, tooltip = 'text')
# actions_plot_corr


library(htmlwidgets)
setwd("output")
saveWidget(plotly_diff, file=paste0("metro-area-vmt-percent-", Sys.Date(), ".html"))
saveWidget(plotly_diff_actual, file=paste0("metro-area-vmt-actuals-", Sys.Date(), ".html"))
saveWidget(actions_plot, file="metro-area-vmt-actions-", Sys.Date(),".html")

