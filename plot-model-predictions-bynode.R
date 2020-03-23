##############
library(ggplot2)
library(plotly)
#############


predicted_and_observed_plot<-
ggplot(this_dat, aes(x = date, y = volume.sum, color = factor(year)))+
  theme_minimal()+
  geom_ribbon(aes(ymin = volume.predict-volume.predict.se, ymax = volume.predict + volume.predict.se, fill = factor(year)),
              alpha = 0.5, color = NA)+
  geom_point()+
  geom_line()+
  facet_wrap(year~., scales = "free_x", nrow = 3)+
  scale_x_date(date_breaks = "2 weeks", date_labels = '%b %d')+
  geom_hline(yintercept = 0)+
  ggtitle(this_dat$r_node_name)+
  theme(legend.position = 'none')+
  labs(x = "Date", y = "Total Daily Volume, Predicted and Observed")

diff_from_normal_plot<-
  ggplot(this_dat, aes(x = date, y = volume.diff, color = factor(year)))+
  theme_minimal()+
  geom_point()+
  geom_line()+
facet_wrap(year~., scales = "free", nrow = 3)+
scale_x_date(date_breaks = "week", date_labels = '%b %d')+
geom_hline(yintercept = 0)+
ggtitle(this_dat$r_node_name)+
theme(legend.position = 'none')+
labs(x = "Date", y = "Difference from Predicted Volume")

grid.arrange(predicted_and_observed_plot, diff_from_normal_plot, nrow = 1)