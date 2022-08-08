# Maria Elisa Montes
# May 2022
#
# Part 1. Cleaning feeding records from foerster technik calf-cloud
# Find unique animals, assign a project identification number (case_no). 
#
# date range 2015 -2019

# Settings ----
source <- 'foerster_technik'
section <- 'historic'

in_dir <-  paste('infiles', source, sep = '/')
out_dir <- 'outfiles/'

# libraries 
library(dplyr)
library(lubridate)
library(stringr)

# foerster - technik data ----
file_name <- 'ft_historic_2015_2019.csv'
data_ft <- read.csv(paste(in_dir, file_name, sep='/'))

# change variables to date format
data_ft$initial_date <- as.Date(data_ft$initial_date, '%d.%m.%Y')
data_ft$event_date <- as.Date(data_ft$event_date, '%d.%m.%Y')

data_ft$feeder <- str_extract(data_ft$feeder, '\\d{1}')

#  select columns of interest
data_ft <- select(data_ft, c('farm_animal_id', 'event_date', 'transmitter_number', 'initial_date', 
                             'speed_absolute', 'speed_relative', 'total_milk_historic', 
                             'feeding_day',  'visits_w_milk', 'visits_wo_milk', 'feeder', 'daily_milk_plan'))

data_ft <- data_ft%>%rename(milk_day = total_milk_historic)%>%
  rename(day_ent = daily_milk_plan)

# From this columns eliminate duplicated records
# before removing duplicated records
sprintf("before removing duplicated records: %i", nrow(data_ft))

data_ft <- data_ft%>%unique()

# after removing duplicated records
sprintf("after removing duplicated records: %i", nrow(data_ft))

# select data from unique calves in ft ----
# find the minimum initial date for each combination trasmitter number
data_count<- data_ft%>%group_by(transmitter_number, farm_animal_id)%>%
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
  mutate(section = 'KM')

# eliminate extra columns  
daily_data_unique$initial <- NULL

daily_data_unique$case_no <- paste('H', str_pad(daily_data_unique$case_no, 6, pad = '0'), sep='')

daily_data_summary <- unique(select(daily_data_unique, c('farm_animal_id', 'initial_date', 'count', 'transmitter_number', 'case_no')))%>%mutate(section = 'KM')

file_out <- paste(out_dir, paste(source, section, sep= '_'), sep = '/')
write.csv(daily_data_unique, paste(file_out, 'daily.csv', sep  ='_'), na = '', row.names = FALSE)
write.csv(daily_data_summary, paste(file_out, 'summary.csv', sep  ='_'), na = '', row.names = FALSE)



