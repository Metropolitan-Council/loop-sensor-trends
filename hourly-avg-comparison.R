library(doParallel)
library(foreach)
library(data.table)
library(sf)
library(leaflet)
library(tidyverse)

# Read in hourly data by node and combine.
# READ DATA (takes about 1.5 minutes)----
setwd('N:\\MTS\\Working\\Modeling\\MetroLoopDetectors\\loop-sensor-trends')
sensor_config <-
  fread('data/Configuration of Metro Detectors 2020-03-24.csv')

node_files <- list.files('data/data_hourly_node/')
node_names <- gsub('.csv', '', node_files)
corr_nodes <- sensor_config[corridor_route %in% c("I-94", "I-694", "I-494", "I-35E")
                           # If you want to exclude entry/exit ramps:
                           & r_node_n_type == 'Station', ]
corr_nodes <- unique(corr_nodes[, c('r_node_name', 'r_node_n_type')])

corr_files <- node_files[node_names %in% corr_nodes$r_node_name]


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

hourlydat <-
  rbindlist(foreach(i = corr_files) %dopar% {
    library(data.table)
    dat <- fread(paste0('data/data_hourly_node/', i))
    
    if (nrow(dat) < 2) {
      
    } else {
      # dat[, date := as.IDate(date)]
      # # Trim to after March 1 2020:
      dat <- dat[date %in% c(
        seq(
          from = as.Date('2018-08-01'),
          to = as.Date('2018-09-26'),
          by = 'days'
        ),
        seq(
          from = as.Date('2019-08-01'),
          to = as.Date('2019-09-26'),
          by = 'days'
        ),
        seq(
          from = as.Date('2020-08-01'),
          to = as.Date('2020-09-26'),
          by = 'days'
        )
      ), ]
      # labor day:
      dat <-
        dat[!date %in% c('2018-09-03', '2019-09-02', '2020-09-07')]
      dat <- dat[, dow := wday(date)]
      dat <- dat[, wk := week(date)]
      # dat <- dat[date <= Sys.Date()-1]
      # dat <- unique(dat)
      # Average volume for this day of week and hour in 2018-2019:
      dat_av <- dat[year(date) %in% c(2018:2019), lapply(.SD, mean),
                    .SDcols = c('volume.sum'),
                    by = c('r_node_name', 'dow', 'hour')]
      names(dat_av) <- c('r_node_name', 'dow', 'hour', 'vol_18_19')
      # Actual volume for this day of week and hour in 2020: 
      dat_20 <- dat[year(date) == 2020, lapply(.SD, mean),
                    .SDcols = c('volume.sum'),
                    by = c('r_node_name', 'dow', 'hour', 'date', 'wk')]
      names(dat_20) <-
        c('r_node_name', 'dow', 'hour', 'date', 'wk', 'vol_20')
      dat_ret <- merge(dat_av, dat_20)
    }
  })

stopCluster(cl)


# bind back to config for corrdiors: 
hourlydat <- merge(hourlydat, unique(sensor_config[,c('r_node_name', 'corridor_route')]))


# hm...issue with day of week.
hourlydat <- hourlydat %>% mutate(dow = wday(as.Date(date)))

# add a date for day of week: 
hourlydat <- hourlydat %>% 
  mutate(weekof= cut(as.Date(date), "week", start.on.monday = FALSE)) %>%
  mutate(weekof = format.Date(weekof, "%B %d"))

# Aggregate up from nodes to corridors: add up all the observed and expected traffic for each date
hr_corr_sum <- hourlydat %>%
  group_by(corridor_route, date, weekof, dow, hour) %>%
  summarize(vol_20 = sum(vol_20, na.rm = T), 
            vol_18_19 = sum(vol_18_19, na.rm = T)) %>%
  ungroup()

# Get difference of observed-expected, in total volume and pct: 
hr_corr_diff <- hr_corr_sum %>%
  mutate(vol_diff = vol_20 - vol_18_19, 
         vol_diff_pct = 100 * (vol_20-vol_18_19)/vol_18_19)

# Aggregate across the 7 days of the week -- get averages for M-F, Weekends: 
# Create day hr_corr_diff groups (M-F, Weekends)
hr_corr_av <- hr_corr_diff  %>%
  mutate(day_type = case_when(dow == 1 ~ "Sun",
                      dow == 7  ~ "Sat", 
                      dow %in% c(2:6) ~ "M-F")) %>%
  group_by(corridor_route, weekof, day_type, hour) %>%
  summarize(vol_diff_av = mean(vol_diff, na.rm = T), 
            vol_diff_pct_av = mean(vol_diff_pct, na.rm = T)) %>%
  ungroup() %>%
  droplevels()

# Plot for each corridor:
ggplot(hr_corr_av, aes(x = hour, y = vol_diff_av, color = factor(weekof))) +
  geom_line() +
  geom_hline(yintercept = 0)+
  facet_grid(corridor_route ~ day_type, scales = 'free_y') +
  cowplot::theme_cowplot()



# Plot for the whole system -----------------------

# Aggregate up from nodes to corridors: add up all the observed and expected traffic for each date
hr_sys_sum <- hourlydat %>%
  group_by(date, weekof, dow, hour) %>%
  summarize(vol_20 = sum(vol_20, na.rm = T), 
            vol_18_19 = sum(vol_18_19, na.rm = T)) %>%
  ungroup()

# Get difference of observed-expected, in total volume and pct: 
hr_sys_diff <- hr_sys_sum %>%
  mutate(vol_diff = vol_20 - vol_18_19, 
         vol_diff_pct = 100 * (vol_20-vol_18_19)/vol_18_19)

# Aggregate across the 7 days of the week -- get averages for M-F, Weekends: 
# Create day hr_sys_diff groups (M-F, Weekends)
hr_sys_av <- hr_sys_diff  %>%
  mutate(day_type = case_when(dow == 1 ~ "Sun",
                              dow == 7  ~ "Sat", 
                              dow %in% c(2:6) ~ "M-F")) %>%
  group_by(weekof, day_type, hour) %>%
  summarize(vol_diff_av = mean(vol_diff, na.rm = T), 
            vol_diff_pct_av = mean(vol_diff_pct, na.rm = T)) %>%
  ungroup() %>%
  droplevels()

# Plot for each corridor:
ggplot(hr_sys_av, aes(x = hour, y = vol_diff_av, color = factor(weekof))) +
  geom_line() +
  geom_hline(yintercept = 0)+
  facet_grid( ~ day_type) +
  cowplot::theme_cowplot()
