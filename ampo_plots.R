
library(ggplot2)
library(plotly)
library(data.table)
options(scipen = 9999)
source("ampo_theme.R")
load("/Volumes/shared/MTS//Working/Modeling/MetroLoopDetectors/loop-sensor-trends/councilcolors.Rdata")
##########################



############# DATA #############
# number of households in metro area (estimated 2018)
hh_total <- 1213980 # https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2018-Population-Estimates-(FINAL,-July-2019)-(1).aspx

diffs_4plot <- fread(paste0("/Volumes/shared/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/pred-and-act-vol-region.csv"))
diffs_4plot[, date := as.IDate(date)]

diffs_4plot[, diffvol_use := ifelse(date %in% c(as.Date("2020-07-03"), as.Date("2020-07-04")), NA, `Difference from Typical VMT (%)`)]
diffs_4plot[, rollingavg := shift(frollapply(diffvol_use, 7, mean, align = "left", na.rm = T))]

# ggplot(diffs_4plot, aes(x = date)) +
#   geom_point(aes(y = vmt.predict, color = "Predicted VMT")) +
#   geom_line(aes(y = vmt.predict, color = "Predicted VMT")) +
#   geom_point(aes(y = vmt.sum, color = "Actual VMT")) +
#   geom_line(aes(y = vmt.sum, color = "Actual VMT")) +
#   scale_x_date(date_breaks = "3 days") +
#   scale_color_manual(values = c(mtsRed, "black"))

#########################
# MNDOT Traffic Trends
yesterday <- Sys.Date() - 1 # change back to -1 when new data available
# yesterday <- as.IDate(yesterday)
# yesterday <- paste0(month(yesterday), "-", mday(yesterday), "-", year(yesterday))
# yesterday <- format(yesterday, format = '%m-%d-%Y')
# mndotdat <- fread(paste0("http://www.dot.state.mn.us/traffic/data/reports/COVID19/Daily_Volume_Change_", yesterday, "_update.csv"))

mndotdat <- fread(paste0("/Volumes/shared/MTS/Working/Modeling/MetroLoopDetectors/loop-sensor-trends/data/mndot-data/Daily_Volume_Change_2020-09-27_update.csv"),header = TRUE)
# , yesterday, "_update.csv"))


mndotdat <- mndotdat[District %in% c("MnDOT Statewide")]
mndotdat <- melt(mndotdat, id.vars = c("District"),
                 variable.name = "date", value.name = "Difference from Typical VMT (%)")
mndotdat[, date := as.IDate(date, format = "%m/%d/%Y")]
# fwrite(mndotdat, paste0("/Volumes/shared/MTS//Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/diff-vol-state.csv"), row.names = F)
# fwrite(mndotdat, paste0("/Volumes/shared/MTS//Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/data-raw/diff-vol-state.csv"), row.names = F)
mndotdat[, date := as.IDate(date)]

mndotdat[, diffvol_use := ifelse(date %in% c(as.Date("2020-07-03"), as.Date("2020-07-04")), NA, `Difference from Typical VMT (%)`)]
mndotdat[, rollingavg := shift(frollapply(diffvol_use, 7, mean, align = "left", na.rm = T))]

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
    "2020-03-28", # Gov. Walz announces a "stay-at-home" order\nwill take effect Mar. 27
    # '2020-04-27', # Gov. Walz announces
    "2020-05-18"
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

actions <- data.table(actions)
actions[, date := as.IDate(date)]
actions <- merge(actions, diffs_4plot, all.x = T, all.y = F)
actions[, arrow_end := `Difference from Typical VMT (%)` - 0.1]
actions[, arrow_start := c(
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


# Static plot -------------------------------------------------------------------

my_shapes <- c(16,16)
my_linetype <- c(
  0,
  0)


plot_dat <- diffs_4plot[doy > 66 & year == 2020] %>% 
  mutate(source = "MnDOT Metro Freeways\n(1000+ Stations)\n") %>% 
  select(date,
         `Difference from Typical VMT (%)`, 
         source,
         rollingavg) %>% 
  rbind(mndotdat[mndotdat$date > "2020-03-06", ][
    , source := "MnDOT Statewide\n(105 Stations)\n" ] %>% 
      select(date,
             `Difference from Typical VMT (%)`, 
             source,
             rollingavg))



static_plot <-
  ggplot() +
  # shaded rectangle for stay-at-home order:
  annotate("rect",
           xmin = as.Date("2020-03-28"), 
           xmax = as.Date("2020-05-18"), ymin = -Inf, ymax = Inf,
           alpha = .15
  ) +
  # horizontal line at zero:
  geom_hline(yintercept = 0) +
  
  ## POINTS ----------------------------------------------------------------------
geom_point(data = plot_dat,
           aes(x = date,
               y = `Difference from Typical VMT (%)`,
               color = source),
           size = 3) +
  
  ## LINES -----------------------------------------------------------------------
  geom_line(data = plot_dat,
            aes(x = date,
                y = rollingavg,
                color = source,
                linetype = "rolling"),
            size = 1,
            show.legend = TRUE) +
  # global options:
  theme_minimal() +
  cowplot::theme_cowplot() +
  ampo_theme() +
  # axes:
  labs(x = "",
       y = "% difference from typical traffic") +
  scale_x_date(breaks = seq(as.Date("2020-03-08"),
                            max(mndotdat[,date]) + 3, by = "2 weeks"),
               date_labels = "%m/%d",
               limits = c(as.Date("2020-03-06"), max(mndotdat$date) + 3)) +
  scale_y_continuous(limits = c(-75, 15),
                     breaks = seq(from = -70, to = 10, by = 10)) +
  #  colors:
  scale_linetype_manual(values = 1,
                        name = "Trend Line",
                        labels = c("7-day rolling average"),
                        guide = guide_legend(order = 2,
                                             nrow = 2,
                                             linetype = 0,
                                             title.position = "top")
  ) + 
  scale_color_manual(
    values = c(councilBlue, "black"),
    labels = c("MnDOT Metro Freeways\n(1000+ Stations)",
               "MnDOT Statewide\n(105 Stations)"),
    name = "Traffic Sensor Group",
    guide = guide_legend(override.aes = 
                           list(
                             shape = my_shapes,
                             linetype = c(0,0)),
                         nrow = 2,
                         title.position =  "top",
                         byrow = TRUE, 
                         order = 1)) +
  theme(
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90"),
    legend.position = "bottom", 
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title.align = 0,
    legend.box.margin = margin(rep(0,4))) 

static_plot

ggsave("output/AMPO/plot.png", static_plot, height = 10, width = 17, units = "in", dpi = 300)
# ggsave('/Volumes/shared/MTS//Working/Modeling/MetroLoopDetectors/loop-sensor-trends/covid.traffic.trends/inst/app/www/traffic-trends-actions.png',static_plot, height = 7, width = 10, units = 'in', dpi = 300)

# pdf("/output/AMPO/traffic.pdf", width = 12, height = 17)
# static_plot
# dev.off()

# pdf('C:/Users/AsmusAL/OneDrive - Metropolitan Council/TBIHouseholdSurvey/CovidPanel/PresentationFigures/traffic.pdf', width =10, height = 7, family = "ArialMT")
# static_plot
# dev.off()

# ### Plot Weekly Trends
# weekly_diffs <- diffs_4plot[date > "2020-03-01"
#                             & weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
# 
# weekly_diffs[, woy := week(date - 5)] # adjust to Monday
# weekly_diffs$weekday <- factor(weekly_diffs$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# 
# mndotdat[, date := as.IDate(date)]
# mndotdat[, woy := week(date - 5)] # adjust to Monday
# mndotdat[, weekday := weekdays(date)]
# mndotdat$weekday <- factor(mndotdat$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# 
# weekly_mndot <- mndotdat[date > "2020-03-01"
#                          & weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
# 
# 
# 
# # aggregate to weekly scale
# weekly_mndot <- weekly_mndot[, lapply(.SD, mean),
#                              .SDcols = "Difference from Typical VMT (%)",
#                              by = .(woy)
# ]
# weekly_diffs <- weekly_diffs[, lapply(.SD, mean),
#                              .SDcols = "Difference from Typical VMT (%)",
#                              by = .(woy)
# ]
# weekly_mndot[, `Traffic Sensor Group` := "MnDOT Statewide\n(105 Stations)\n"]
# weekly_diffs[, `Traffic Sensor Group` := "MnDOT Metro Freeways\n(1000+ Stations)\n"]
# 
# weekly_dat <- rbind(weekly_mndot, weekly_diffs)
# 
# weekly_dat <- merge(weekly_dat, unique(mndotdat[mndotdat$weekday == "Monday", c("woy", "date")]))
# weekly_dat[, date := as.IDate(date)]
# weekly_dat[, week_of := format(date, "%b %d")]
# weekly_dat[, week_of := paste0("Week of\n", week_of)]
# weekly_dat[, week_of := factor(week_of, levels = unique(weekly_dat$week_of))]
# weekly_dat$woy2 <- format(weekly_dat$woy, "%m-%d")
# 
# plot2 <-
#   ggplot(
#     weekly_dat,
#     aes(
#       x = week_of,
#       y = (`Difference from Typical VMT (%)`), fill = `Traffic Sensor Group`
#     )
#   ) +
#   theme_minimal() +
#   
#   # # shaded rectangle for stay-at-home order:
#   # annotate("rect", xmin = 12.5, xmax = 17.5, ymin = -Inf, ymax = Inf,
#   #          alpha = .15)+
#   
#   geom_bar(stat = "identity", position = "dodge", width = 0.9) +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous(limits = c(-55, 10), breaks = seq(from = -50, to = 0, by = 10)) +
#   # scale_x_continuous(limits = c(8.5, 17.5), breaks = seq(from = 9, to = 17, by = 1))+
#   cowplot::theme_cowplot() +
#   theme(legend.position = "right") +
#   labs(x = "", y = "% difference from typical traffic") +
#   geom_text(aes(
#     y = (`Difference from Typical VMT (%)`),
#     label = paste0(formatC(round(`Difference from Typical VMT (%)`), flag = "+"), "%")
#   ),
#   vjust = 1, size = 3.7, hjust = 0.5, position = position_dodge(width = 1), color = "gray40"
#   ) +
#   scale_fill_manual(values = c(councilBlue, "black"), name = "Traffic Sensor Group")
# # ggtitle(paste0("Weekly Average Traffic on MnDOT Roads\nUpdated ", Sys.Date()))+
# # scale_x_date(date_breaks = "3 days", date_labels = '%m/%d\n(%a)', minor_breaks = "days")+
# # annotation_raster(mypng, ymin = 2, ymax= 20,xmin = 7, xmax = 9)
# plot2
# 
# ggsave(paste0("/Volumes/shared/MTS//Working/Modeling/MetroLoopDetectors/loop-sensor-trends/output/AMPO/weekly-traffic-trends.png"), plot2, height = 7, width = 10, units = "in", dpi = 300)
