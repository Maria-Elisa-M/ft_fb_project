# Maria Elisa Montes
# May 2022
#
# Part 1. Cleaning feeding records from foerster technik calf-cloud
# Find unique animals, assign a project identification number (case_no). 

# Settings ----
source <- 'foerster_technik'
section <- 'calfcloud'

in_dir <-  paste('infiles', source, sep = '/')
out_dir <- 'outfiles/'

# libraries 
library(dplyr)
library(lubridate)
library(stringr)

# foerster - technik data ----
file_in <- 'ft_calfcloud_2019_2021.csv'
data_ft <- read.csv(paste(in_dir, file_in, sep='/'))

# column names and data formats ----
# rename columns so that they have the same names as historic version
data_ft <- data_ft%>%
  rename(visits_w_milk = visit_w_ent)%>%
  rename(visits_wo_milk = visit_wo_ent)%>%
  rename(transmitter_number = calf)%>%
  mutate(milk_day = milk_consumption/1000)

# translate feeder numbers to old format (refer to metatda FT historic for translation)
data_ft <- data_ft%>%
  mutate(feeder = ifelse(feeder_num == 3625, 1,
                         ifelse(feeder_num == 3719, 2,
                                ifelse(feeder_num == 7550, 3,
                                       ifelse(feeder_num == 7597, 4,
                                              ifelse(feeder_num == 7598, 5,
                                                     ifelse(feeder_num == 7599, 6,
                                                            ifelse(feeder_num == 7600, 7,
                                                                   ifelse(feeder_num == 7602, 8,NA)))))))))

#  select columns of interest
data_ft <- select(data_ft, c('farm_animal_id', 'event_date', 'transmitter_number', 'initial_date', 
                             'speed_absolute', 'speed_relative', 'milk_day', 
                             'feeding_day', 'visits_w_milk', 'visits_wo_milk', 'feeder', 'feed'))

data_ft <- data_ft%>%
  mutate(day_ent = feed/10)
data_ft$feed <- NULL

# change variables to date format
data_ft$initial_date <- as.Date(data_ft$event_date, '%Y-%m-%d')
data_ft$event_date <- as.Date(data_ft$event_date, '%Y-%m-%d')

# From this columns eliminate duplicated records
# before removing duplicated records
sprintf("before removing duplicated records: %i", nrow(data_ft))

data_ft <- data_ft%>%unique()

# after removing duplicated records
sprintf("after removing duplicated records: %i", nrow(data_ft))

# select data from unique calves in ft -----

id_transmitter <- data_ft%>%
  select(c('transmitter_number', 'farm_animal_id'))%>%
  unique()

# find unique cases-----
# find the minimum initial date for each combination trasmitter number - animal id
data_count<- data_ft%>%group_by(transmitter_number, farm_animal_id)%>%filter(transmitter_number != '')%>%
  summarize(count = n(), initial = min(initial_date))%>%
  ungroup()

# Assign a case number to each combination
data_count <- data_count%>%mutate(case_no = row_number())

# case numbers to daily data ----
# give the case number to the daily data
daily_data_unique <- merge(data_ft, data_count, by = c('transmitter_number', 'farm_animal_id'), all.y = TRUE)

# get the new initial day as a day
daily_data_unique$initial <- as_date(daily_data_unique$initial)

# subtract initial from event date to get feeding day
daily_data_unique <- daily_data_unique%>%
  mutate(feeding_day = as.numeric(event_date- initial, units = 'days'))

# change initial_date to be initial
daily_data_unique <- daily_data_unique%>%
  mutate(initial_date = initial)%>%
  mutate(section = 'CC')


# eliminate extra columns  
daily_data_unique$initial <- NULL

daily_data_unique$case_no <- paste('C', str_pad(daily_data_unique$case_no, 6, pad = '0'), sep='')

daily_data_summary <- unique(select(daily_data_unique, c('farm_animal_id', 'initial_date', 'count', 'transmitter_number', 'case_no')))%>%
  mutate(section = 'CC')

file_out <- paste(out_dir, paste(source, section, sep= '_'), sep = '/')
write.csv(daily_data_unique, paste(file_out, 'daily.csv', sep  ='_'), na = '', row.names = FALSE)
write.csv(daily_data_summary, paste(file_out, 'summary.csv', sep  ='_'), na = '', row.names = FALSE)

