# Maria Elisa Montes
# May 2022
#
# Part 5

library(dplyr)

# get data ----

in_dir <- "outfiles"
out_dir <- "outfiles"

# demographic files -----
source <- 'dc_demo'
demo_data <- read.csv(paste(in_dir, paste(source, 'master.csv', sep = '_'), sep = '/'))

# foerster technik files -----
source <- 'foerster_technik'
ft_data <- read.csv(paste(in_dir, paste(source, 'daily.csv', sep = '_'), sep = '/'))
ft_data$event_date <- as.Date(ft_data$event_date)

# events files -----
source <- 'dc_events'
weight_data <- read.csv(paste(in_dir, paste(source, 'bweight.csv', sep = '_'), sep = '/'))
brd_data <- read.csv(paste(in_dir, paste(source, 'brd_daily.csv', sep = '_'), sep = '/'))
brd_data$Date <- as.Date(brd_data$Date)

# weather files -----
source <- 'weather'
weather_data <- read.csv(paste(in_dir, paste(source, 'daily.csv', sep = '_'), sep = '/'))
weather_data$event_date <- as.Date(weather_data$event_date)

# merge files -----
ft_demo <- merge(ft_data, demo_data, by = c('case_no'), all.x = TRUE)
ft_demo_weight <- merge(ft_demo, weight_data,  by = c('case_no'), all.x = TRUE)
ft_demo_weight_brd <- merge(ft_demo_weight, brd_data,  by.x = c('case_no', 'event_date'), by.y = c('case_no', 'Date'), all.x = TRUE)

integrated_df <- merge(ft_demo_weight_brd, weather_data,  by= c('event_date'), all.x = TRUE)

write.csv(integrated_df, paste(out_dir, 'integrated_file.csv', sep = '/'), row.names = FALSE)

