
library(ggplot2)
library(dplyr)
library(lubridate)

in_dir <- 'outfiles'
# out_dir <- 'milk_plan'


data_day <- read.csv(paste(in_dir , 'integrated_file_sample.csv', sep = '/'))

data_day <- data_day%>%group_by(case_no)%>%arrange(event_date)%>%
  mutate(d_milk =  milk_day - lag(milk_day, default = first(milk_day)))%>%
  mutate(d_speed =  speed2 - lag(speed2, default = first(speed2)))


ggplot(data_day, aes(x = d_temp, y = d_milk)) + geom_point()
