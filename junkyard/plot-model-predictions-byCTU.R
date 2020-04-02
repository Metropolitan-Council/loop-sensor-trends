


diffs_ctu <- mctu2df[,lapply(.SD, FUN = function(x) sum(x, na.rm = T)),
                     .SDcols = c('avg.diff'), 
                     by = .(date, dow, doy, year, woy, weekday, monthday, CTU_NAME.x)]
# diffs_ctu[,c("vmt.sum", "vmt.predict"):=list(volume.sum*0.5, volume.predict * 0.5)]
diffs_ctu[,'Difference from Typical VMT (%)':=round(avg.diff, 1)]
diffs_ctu[,"CTU Name":=CTU_NAME.x]


diff_from_normal_plot_corr <-
  ggplot(diffs_ctu[doy>0], aes(x = date, y = `Difference from Typical VMT (%)`, 
                               color = `CTU Name`,
                               group = `CTU Name`,
                               text = paste0(`CTU Name`,'<br>Date: ', date," (", weekday, ")",
                                             '<br>Difference from Typical VMT: ', `Difference from Typical VMT (%)`, " %")))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "week", date_labels = '%b %d\n(%A)')+
  geom_hline(yintercept = 0)+
  ggtitle("Metro Area Traffic")+
  labs(x = "Date", y = "Difference from Typical VMT\n(Vehicle Miles Traveled)")+
  geom_text(aes(x = as.Date('2020-01-11'), y = -26, label = "Jan. 17 Cold Snap"), color = 'black')
diff_from_normal_plot_corr

plotly_corr <- plotly::ggplotly(diff_from_normal_plot_corr, tooltip = 'text')

plotly_corr

library(htmlwidgets)
saveWidget(plotly_corr, file="metro-area-vmt-total-by-CTU-2020-03-21.html")
