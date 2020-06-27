#import libraries
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
setwd('/Users/thanhhuyen/Desktop/Projects/Uber Data Analysis')

#create vector of colors
colors = c("#B2182B","#D6604D","#F4A582","#FDDBC7","#D1E5F0","#92C5DE","#4393C3","#2166AC")

#read the files
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14.csv")

#format the date & time column
data_2014 <- rbind(apr, may, jun, jul, aug, sep)
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

#create vectors of day, month, year, day of the week, hour, minute, second
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#group trips by hour
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

#plot trips by the hour in a day
plot1 = ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "cornflowerblue", color = "tomato") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
plot1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by month
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

#plot trips by the hour and month
plot2 = ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)
plot2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by day
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

#plot trips every day
plot3 = ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "maroon") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
plot3 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by day of week and month
dayofweek_month_group <- data_2014 %>%
  group_by(dayofweek, month) %>%
  dplyr::summarize(Total = n())
datatable(dayofweek_month_group)

#plot trips by day and month
plot4 = ggplot(dayofweek_month_group, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position='dodge') +
  ggtitle("Trips by Day of Week and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)
plot4 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by month
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

#plot trips by month
plot5 = ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)
plot5 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#plot trips by bases
plot6 = ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "cornflowerblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")
plot6 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#plot trips by bases and month
plot7 = ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)
plot7 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#plot trips by bases and day of week
plot8 = ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)
plot8 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# group trips by day and hour
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

#plot heatmap by hour and day
plot9 = ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")
plot9 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# group trips by day and hour
month_dayofweek <- data_2014 %>%
  group_by(dayofweek, month) %>%
  dplyr::summarize(Total = n())

#plot heatmap by month and day of week
plot10 = ggplot(month_dayofweek, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")
plot10 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by month and bases
month_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

#plot heatmap by month and bases
plot11 = ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")
plot11 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#group trips by bases and day of week
dayofweek_bases <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

#plot heatmap by bases and day of week
plot12 = ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")
plot12 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#create a map visualization of rides
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004
ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
