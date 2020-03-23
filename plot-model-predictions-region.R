##############
library(ggplot2)
library(plotly)
load('councilcolors.Rdata')
#############

diffs_dt <- fread('data/predicted-and-actual-volumes-2020-03-22.csv')
diffs_dt[,date:=as.IDate(date)]

# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

# Total difference from expected for whole metro area ####
diffs_corr <- diffs_dt[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                      .SDcols = c('volume.sum', 'volume.predict'), 
                      by = .(date, dow, doy, year, woy, weekday, monthday)]
diffs_corr[,c("vmt.sum", "vmt.predict"):=list(volume.sum*0.5, volume.predict * 0.5)]
diffs_corr[,'Difference from Typical VMT (%)':=round(100*(vmt.sum-vmt.predict)/vmt.predict, 2)]

diff_from_normal_plot_corr <-
  ggplot(diffs_corr[doy>0 & year == 2020], aes(x = date, y = `Difference from Typical VMT (%)`, 
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

plotly_corr <- plotly::ggplotly(diff_from_normal_plot_corr, tooltip = 'text')

plotly_corr

library(htmlwidgets)
setwd("output")
saveWidget(plotly_corr, file="metro-area-vmt-percent-2020-03-22.html")

# trends and actual####
diffs_corr_long <- melt(diffs_corr[,.(vmt.sum, vmt.predict, date, dow, doy, year, woy, weekday, monthday, `Difference from Typical VMT (%)`)], 
                        id.vars = c('date', 'dow', 'doy', 'year', 'woy', 'weekday', 'monthday', "Difference from Typical VMT (%)"),
                        variable.name = "estimate_type", value.name = "VMT")
diffs_corr_long$estimate_type <- ifelse(diffs_corr_long$estimate_type == "vmt.sum", "Actual Traffic", "Typical Traffic")

diffs_corr_long[,difference_text:=ifelse(estimate_type == "Actual Traffic", 
                                         ifelse(`Difference from Typical VMT (%)` <0, paste0(abs(`Difference from Typical VMT (%)`), " % less than typical"),
                                                paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % more than typical")),
                                         ifelse(`Difference from Typical VMT (%)` <0, paste0(abs(`Difference from Typical VMT (%)`), " % more than actual"),
                                                paste0(abs(round(`Difference from Typical VMT (%)`, 1)), " % less than actual")))]


trends_and_actual_plot_corr <-
  ggplot(diffs_corr_long[doy>0 & year == 2020], aes(x = date, y = VMT, 
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
  geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e07, label = "Jan. 17\nCold Snap"))+
  geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

plotly_corr_actual <- plotly::ggplotly(trends_and_actual_plot_corr, tooltip = 'text')

plotly_corr_actual

saveWidget(plotly_corr_actual, file="metro-area-vmt-total-2020-03-22.html")

actions <- cbind(
  date = c('2020-03-06', '2020-03-11', '2020-03-13', '2020-03-16', '2020-03-18'),
  action = c('1st COVID-19 Case in MN', 'UMN Suspends Classes', 'Restaurants Closed',
             'Gatherings >250 ppl Banned', 'Public Schools Close')
)
actions <-data.table(actions)
actions[,date:=as.IDate(date)]

diffs_corr_long <- merge(diffs_corr_long, actions, all = T)

# MN state actions ####
actions_plot_corr <-
  ggplot(diffs_corr_long[doy>60 & year == 2020], aes(x = date, y = VMT, 
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
  # geom_text(color = 'black', aes(x = as.Date('2020-01-17'), y = 1e07, label = "Jan. 17\nCold Snap"))+
  # geom_text(color = 'black', aes(x = as.Date('2020-02-09'), y = 1e07, label = "Feb. 9\nSnow Storm"))

actions_plot_corr <- plotly::ggplotly(actions_plot_corr, tooltip = 'text')

actions_plot_corr

saveWidget(plotly_corr_actual, file="metro-area-vmt-actions-2020-03-22.html")

