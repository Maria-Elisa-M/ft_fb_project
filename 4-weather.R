# Maria Elisa Montes
# May 2022
#
# Part 4
# date range 2015 - 2021

library(dplyr)
library(lubridate)
library(stringr)

# get data ----
source <- 'weather'
in_dir <-  paste("infiles", source, sep ='/')
out_dir <- "outfiles"
  
# weather in days
file_list <- list.files(in_dir)
fileNames <- paste(in_dir, file_list, sep = "/")
weather_d <- read.csv(fileNames[1])
for(i in 2:length(fileNames)){
  temp <- read.csv(fileNames[i])
  weather_d <- rbind(weather_d, temp)
}
rm(temp)
weather_d <- weather_d%>%filter(station == 'C65')
weather_d$event_date <- ymd_hms(weather_d$event_date)

weather_d <- weather_d%>%
  mutate(temp = (temperature-32) * 5/9)%>%
  mutate(thi = (1.8 * temp +32) - (0.55-0.0055*relative_humidity)*(1.8*temp -26))

# more daily weather
weather_day <- weather_d%>%
  arrange(event_date)%>%
  mutate(d_temp = temperature - lag(temperature, default = first(temperature)))%>%
  mutate(d_thi = thi- lag(thi, default = first(thi)))

# add season 
weather3 <- mutate(weather_day, day = str_pad(day(event_date), 2, pad= 0))%>%
  mutate(md = as.numeric(paste(month(event_date), day, sep ='')))%>%
  mutate(season = ifelse(md <= 320 | md >=  1221, 'winter', 
                         ifelse(md >= 321 & md <= 620, 'spring', 
                                ifelse(md >= 621 & md <= 920, 'summer', 
                                       ifelse(md >= 921 & md <= 1220, 'fall', NA)))))

weather3 <- weather3%>%select('event_date', 'season', 'temp','thi', 'maximum_temperature', 'temperature', 'minimum_temperature', 
                              'd_temp', 'd_thi')

write.csv(weather3, paste(out_dir, paste(source, "daily.csv", sep = '_'), sep = '/'), row.names = FALSE, na  = "")