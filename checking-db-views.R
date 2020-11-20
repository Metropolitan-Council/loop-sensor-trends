#Opening the toolbox-------------------------------
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle) # Moment of truth...does it load?
library(ggplot2)
library(data.table)
library(lubridate)

#Connecting to the database: the other 25% of the battle -------------------------------
connect.string = '(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))'
tbidb = ROracle::dbConnect(dbDriver("Oracle"), 
                           dbname = connect.string, 
                           username = 'mts_planning_data', # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
                           password = rstudioapi::askForPassword("database password"))

#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")



#Checking system-wide trends-------------------------------
tictoc::tic()
sys_diffs <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_DAILY_SYSTEM_DIFF")
tictoc::toc() # 2.7 minutes

head(sys_diffs)
sys_diffs <- data.table(sys_diffs)
# sys_diffs <- data.table(sys_diffs)[,date:=substr(DAY, start = 1, stop = 10)]

setnames(sys_diffs, old = 'DATA_DATE', new = 'date')
sys_diffs[,date:=as.IDate(date, format = "%Y-%m-%d")]
setkey(sys_diffs, date)

old_diffs <- fread('output/pred-and-act-vol-region.csv')
old_diffs <- old_diffs[date < '2020-10-15']
head(old_diffs)
setkey(old_diffs, date)


diffs <- old_diffs[sys_diffs]
summary(diffs)

diffs[,old_diff:=(volume.sum - volume.predict)/volume.predict]
diffs[,new_diff:=(TOTAL_VOLUME_DIFF/TOTAL_PREDICTED_VOLUME)]

ggplot(diffs, aes(x = date)) +
  geom_point(aes(y = new_diff, color = "New"))+
  geom_point(aes(y = old_diff, color = "Old")) +
  geom_line(aes(y = new_diff, color = "New"))+
  geom_line(aes(y = old_diff, color = "Old")) + 
  theme_minimal() + 
  scale_x_date(date_breaks = "2 weeks",  date_labels = "%m-%d")


ggplot(diffs, aes(x = date)) +
  geom_point(aes(y = volume.predict, color = "New"))+
  geom_point(aes(y = TOTAL_PREDICTED_VOLUME, color = "Old"))

ggplot(diffs, aes(x = old_diff, y = new_diff)) + 
  geom_point(aes(color = date))+
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_viridis_c()


ggplot(diffs, aes(x = volume.sum, y = TOTAL_VOLUME)) + 
  geom_point(aes(color = date))+
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_viridis_c()

#Checking node-by-node trends -------------------------------
node_diffs <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_DAILY_NODE_DIFF")
head(node_diffs)
node_diffs<- data.table(node_diffs)
node_diffs[,date:=as.IDate(DATA_DATE, format = "%Y-%m-%d")]
setnames(node_diffs, old = "NODE_NAME", new = "r_node_name")
setkey(node_diffs, date, r_node_name)
node_diffs <- node_diffs[date <= '2020-10-21',]

old_node_diffs <- fread('output/pred-and-act-vol-by-node.csv')
head(old_node_diffs)
setkey(old_node_diffs, date, r_node_name)


nodes <- old_node_diffs[node_diffs]


nodes[,old_diff:=(volume.sum - volume.predict)/volume.predict]
nodes[,new_diff:=(VOLUME_DIFF/VOLUME_PREDICT)]
nodes <- nodes[date <= '2020-10-21',]

with(nodes, cor(old_diff, new_diff, use = "complete"))
with(nodes, cor(volume.sum, TOTAL_VOLUME, use = "complete"))

nodes[new_diff>20]

ggplot(nodes, aes(x = old_diff, y = new_diff)) + 
  geom_point(aes(color = date))+
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_viridis_c()

lab_dates <- pretty(nodes$date)

ggplot(nodes, aes(x = volume.sum, y = TOTAL_VOLUME)) + 
  geom_point(aes(color = date), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates, option = "magma") + 
  theme_minimal()


#Checking raw data for a sensor  -------------------------------
test_sensor <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_HOURLY_NODE WHERE NODE_NAME = 'rnd6_9'")
test_sensor <- data.table(test_sensor)
test_sensor[,date:=date(DATA_DATE)]


setnames(test_sensor, old = c('NODE_NAME', 'TOTAL_VOLUME', 'DATA_HOUR'), new = c('r_node_name', 'volume.sum.new', 'hour'))


old_sensor <- fread('data/data_hourly_node/rnd6_9.csv')
old_sensor <- unique(old_sensor)
old_sensor[,r_node_name:=as.character(r_node_name)]
setkey(old_sensor, r_node_name, date, hour)
setkey(test_sensor, sensor, date, hour)


my_sensor <- merge(test_sensor, old_sensor)
my_sensor[date == '2020-03-08']
my_sensor[,date:=as.IDate(date)]

lab_dates <- pretty(my_sensor$date)
ggplot(my_sensor[date<'2020-10-01'], aes(x = volume.sum.new, y = volume.sum)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates, option = "magma") + 
  theme_minimal()


my_sensor[,pct_diff:=100 * ((volume.sum.new - volume.sum)/volume.sum)]
ggplot(my_sensor[date<'2020-03-15'], aes(x = date, y = pct_diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(breaks = "days", date_labels = "%m-%d")

ggplot(my_sensor[date>'2020-10-01'], aes(x = date, y = pct_diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(breaks = "days", date_labels = "%m-%d")

ggplot(my_sensor, aes(x = hour, y = pct_diff)) + 
  geom_point()+
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates) + 
  theme_minimal() + 
  labs(y = "New Volume vs. Old Volume (%)")




#Checking raw data for another sensor  -------------------------------
test_sensor <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_HOURLY_DETECTOR WHERE DETECTOR_NAME = '2406'")
test_sensor <- unique(test_sensor)
test_sensor <- as.data.table(test_sensor)
test_sensor[,c("date", "time"):=IDateTime(START_DATETIME, format = "%Y-%m-%d %H:%m:%s")]
test_sensor <- test_sensor[,sum(VOLUME_SUM), by = .(DETECTOR_NAME, date, hour(time))]
setnames(test_sensor, old = c('DETECTOR_NAME', 'V1'), new = c('sensor', 'volume.sum.new'))

old_sensor <- fread('data/data_hourly_raw/Sensor 2406.csv')
old_sensor <- unique(old_sensor)
old_sensor[,sensor:=as.character(sensor)]
setkey(old_sensor, sensor, date, hour)
setkey(test_sensor, sensor, date, hour)



my_sensor <- merge(test_sensor, old_sensor)
lab_dates <- pretty(my_sensor$date)
ggplot(my_sensor, aes(x = volume.sum, y = volume.sum.new)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates) + 
  theme_minimal()+
  labs(x = "Old Volume", y = "New Volume")

my_sensor[,pct_diff:=100 * ((volume.sum.new - volume.sum)/volume.sum)]
ggplot(my_sensor[date<'2020-03-15'], aes(x = date, y = pct_diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(breaks = "days", date_labels = "%m-%d")

ggplot(my_sensor[date>'2020-10-01'], aes(x = date, y = pct_diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(breaks = "days", date_labels = "%m-%d")

ggplot(my_sensor, aes(x = hour, y = pct_diff)) + 
  geom_point()+
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates) + 
  theme_minimal() + 
  labs(y = "New Volume vs. Old Volume (%)")


#Checking raw data for another sensor  -------------------------------
test_sensor <- ROracle::dbGetQuery(tbidb,"SELECT * FROM RTMC_5MIN_TEMP WHERE DETECTOR_NAME = '5558'")
test_sensor <- unique(test_sensor)
test_sensor <- as.data.table(test_sensor)
test_sensor[,c("date", "time"):=IDateTime(START_DATETIME, format = "%Y-%m-%d %H:%m:%s")]
test_sensor <- test_sensor[,sum(VOLUME_SUM), by = .(DETECTOR_NAME, date, hour(time))]
setnames(test_sensor, old = c('DETECTOR_NAME', 'V1'), new = c('sensor', 'volume.sum.new'))

old_sensor <- fread('data/data_hourly_raw/Sensor 5558.csv')
old_sensor <- unique(old_sensor)
old_sensor[,sensor:=as.character(sensor)]
setkey(old_sensor, sensor, date, hour)
setkey(test_sensor, sensor, date, hour)


my_sensor <- merge(test_sensor, old_sensor)
lab_dates <- pretty(my_sensor$date)
ggplot(my_sensor, aes(x = volume.sum, y = volume.sum.new)) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates) + 
  theme_minimal()+
  labs(x = "Old Volume", y = "New Volume")+
  ggtitle("Hourly Volumes, Sensor 5558")

my_sensor[,pct_diff:=100 * ((volume.sum.new - volume.sum)/volume.sum)]
ggplot(my_sensor[date<'2020-03-15'], aes(x = date, y = pct_diff)) +
  geom_point(aes(color = hour), size = 2)+
  scale_color_viridis_c() +
  geom_hline(yintercept = 0)+
  scale_x_date(breaks = "days", date_labels = "%m-%d")+
  theme_minimal()+
  labs(y = "% Difference in Volume, Old vs. New Data")+
  ggtitle("Hourly Volumes, Sensor 5558")

ggplot(my_sensor[date>'2020-10-01'], aes(x = date, y = pct_diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(breaks = "days", date_labels = "%m-%d")

ggplot(my_sensor, aes(x = hour, y = pct_diff)) + 
  geom_point()+
  geom_point(aes(color = date))+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates) + 
  theme_minimal() + 
  labs(y = "New Volume vs. Old Volume (%)")



