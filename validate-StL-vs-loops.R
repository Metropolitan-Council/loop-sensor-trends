# compare loop sensor data to StL Volume Estimates
library(data.table)
library(tidyverse)
library(sf)
load('councilcolors.RData')
stldat <- openxlsx::read.xlsx('data/county_vmt_download_update_5.11.xlsx')

stldat <- stldat %>%
  mutate(ref_dt = openxlsx::convertToDate(ref_dt))%>%
  filter(state_name == 'Minnesota')

stlmetro <- stldat %>%
  filter(county_name %in% c('Anoka', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington', 'Carver'))%>%
  group_by(ref_dt) %>%
  summarize(vmt.sum.stl = sum(county_vmt), vmt.jan.stl = sum(jan_avg_vmt))%>%
  mutate(diff.pct.stl = (vmt.sum.stl - vmt.jan.stl)/vmt.jan.stl * 100)%>%
  rename(date = ref_dt)

stlstate <- stldat %>%
  # filter(county_name %in% c('Anoka', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington', 'Carver'))%>%
  group_by(ref_dt) %>%
  summarize(vmt.sum.stl.state = sum(county_vmt), vmt.jan.stl.state = sum(jan_avg_vmt))%>%
  mutate(diff.pct.stl.state = (vmt.sum.stl.state - vmt.jan.stl.state)/vmt.jan.stl.state * 100)%>%
  rename(date = ref_dt)

stlgreaterMN<- stldat %>%
  filter(!county_name %in% c('Anoka', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington', 'Carver'))%>%
  group_by(ref_dt) %>%
  summarize(vmt.sum.stl.gmn = sum(county_vmt), vmt.jan.stl.gmn = sum(jan_avg_vmt))%>%
  mutate(diff.pct.stl.gmn = (vmt.sum.stl.gmn - vmt.jan.stl.gmn)/vmt.jan.stl.gmn * 100)%>%
  rename(date = ref_dt)

stldat2 <- left_join(stlmetro, stlstate)
stldat2 <- left_join(stldat2, stlgreaterMN)

stldat2$date <- as.Date(stldat2$date, format = '%m/%d/%Y')

# state ATR data:
atrdat <- fread('output/diff-vol-state.csv')
atrdat <- atrdat %>% select(date, `Difference from Typical VMT (%)`)%>%
  rename(diff.pct.atr = `Difference from Typical VMT (%)`) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'))

# loop detector data:
loopdat <- fread('output/pred-and-act-vol-region.csv')
loopdat <- loopdat %>% select(date, dow, year, woy, weekday, monthday, vmt.sum, vmt.predict, `Difference from Typical VMT (%)`)%>%
  rename(diff.pct.loop = `Difference from Typical VMT (%)`,
         vmt.sum.loop = vmt.sum,
         vmt.sum.looppred = vmt.predict)%>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'))

# merge:
loopstl <- left_join(loopdat, stldat2)
loopstl <- left_join(loopstl, atrdat)

loopstl$weekday <- factor(loopstl$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

loopstl$weekof <- ifelse(loopstl$date < '2020-03-08', 'Week of March 1',
                         ifelse(loopstl$date < '2020-03-15', 'Week of March 8',
                                ifelse(loopstl$date < '2020-03-22', 'Week of March 15',
                                       ifelse(loopstl$date < '2020-03-29', 'Week of March 22',
                                              ifelse(loopstl$date < '2020-04-05', 'Week of March 29',
                                                     ifelse(loopstl$date < '2020-04-12', 'Week of April 5', 'Week of April 12'))))))

loopstl$weekof <- factor(loopstl$weekof, levels = 
                           c('Week of March 1', 'Week of March 8', 'Week of March 15','Week of March 22', 'Week of March 29', 'Week of April 5', 'Week ofApril 12'))

loopstl$date <- as.IDate(lubridate::fast_strptime(loopstl$date, format = "%Y-%m-%d"))

ggplot(loopstl, aes(x = diff.pct.loop, y =diff.pct.stl))+
  geom_point()+
  cowplot::theme_cowplot()+
  geom_hline(yintercept = 0, color = 'gray50')+
  geom_vline(xintercept = 0, color = 'gray50')+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'black') + 
  scale_x_continuous(limits = c(-90, 70))+
  scale_y_continuous(limits = c(-90, 70))+
  facet_grid(~weekday)

loop_compare <- 
ggplot(loopstl, aes(x = date))+
  geom_point(aes(y =diff.pct.stl, color = "StreetLight VMT,\nMetro Counties\n"))+
  geom_line(aes(y =diff.pct.stl, color = "StreetLight VMT,\nMetro Counties\n"))+
  
  # geom_point(aes(y =diff.pct.stl.state, color = "StreetLight,\nAll MN Counties"))+
  # geom_line(aes(y =diff.pct.stl.state, color = "StreetLight,\nAll MN Counties"))+
  
  geom_point(aes(y =diff.pct.stl.gmn, color = "StreetLight VMT,\nGreater MN Counties\n"))+
  geom_line(aes(y =diff.pct.stl.gmn, color = "StreetLight VMT,\nGreater MN Counties\n"))+
  
  geom_point(aes(y = diff.pct.loop, color = "Metro Loop Detectors\n"))+
  geom_line(aes(y =diff.pct.loop, color = "Metro Loop Detectors\n"))+
  
  geom_point(aes(y = diff.pct.atr, color = "State ATRs\n"))+
  geom_line(aes(y =diff.pct.atr, color = "State ATRs\n"))+
  
  scale_color_manual(values = c(councilBlue, 'black', 'gray50', 'gray70'),
                     name = 'Data Source')+
  
  cowplot::theme_cowplot()+
  theme(panel.grid.major.x = element_line(color = 'gray90'),
        panel.grid.major.y = element_line(color = 'gray90'))+
  geom_hline(yintercept = 0, color = 'black')+
  scale_y_continuous(breaks = seq(from = -100, to = 0, by = 10), limits = c(-100, 0), name = '% Difference from Typical')+
  scale_x_date(breaks = seq(as.Date('2020-03-29'), Sys.Date(),by="week"),
               date_labels = "%b %d\n(%A)", 
               limits = c(as.Date('2020-03-29'), Sys.Date()))

ggsave(paste0('output/streetlight-vs-trafficsensors.png'),loop_compare, height = 7, width = 13, units = 'in', dpi = 300)


# 
#   geom_abline(slope = 1, linetype = 'dotdash', color = 'black', intercept = -15)+
#   geom_abline(slope = 0.5, linetype = 'dash', color = 'gray40', intercept = -14)
#   


cor(loopstl$diff.pct.loop, loopstl$diff.pct.stl, use = "complete.obs") # R = 0.93

mod <- with(loopstl, lm(diff.pct.loop ~ diff.pct.stl))
summary(mod) # r2 = 0.87; est = -14, slope = 0.5

# 
# 
# library(DBI)
# db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")
# county_shp <- DBI::dbGetQuery(db,"SELECT
#                         *,
#                         SHAPE.STAsText() as geometry
#                         FROM GISLibrary.DBO.Counties;") %>%
#   st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
#   st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
# 
# metro_shp <- DBI::dbGetQuery(db,"SELECT
#                         *,
#                         SHAPE.STAsText() as geometry
#                         FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;") %>%
#   st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
#   st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
# 
# 
# config <- fread('Configuration of Metro Detectors 2020-03-24.csv')
# config_shp <- st_as_sf(config, coords = c('r_node_lon', 'r_node_lat'),
#                        crs = 4326)
# 
# # join loop detector data to county level data
# loop_counties <- st_join(config_shp, county_shp)
# head(loop_counties)
# uniqueN(loop_counties$CO_NAME) # 34 counties with loop sensors in them
# 
# # flag counties that contain loop detector data
