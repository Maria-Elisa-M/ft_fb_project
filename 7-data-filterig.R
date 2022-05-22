# Maria Elisa Montes
# May 2022
#
# Part 7

library(dplyr)
library(tidyr)
library(lubridate)

# get data ----
in_dir <- "outfiles"
out_dir <- "outfiles"

ft_daily <- read.csv(paste(in_dir, 'integrated_file_v2.csv', sep = '/'))
ft_daily$event_date <- as_date(ft_daily$event_date)

# subsample by gender and breed
ft_daily_fh <- ft_daily%>%filter(gender == 'F', breed == 'H') 

# subsample by weight 
weight_data <- ft_daily_fh%>%select(c('case_no','weight_kg'))%>%unique()

mean_weight <- mean(weight_data$weight_kg, na.rm = TRUE)
sd_weight <- sd(weight_data$weight_kg, na.rm = TRUE)
ll_weight <- mean_weight - 4*sd_weight
ul_weight <- mean_weight + 4*sd_weight

weight_data <- weight_data%>%mutate(weight_2 = ifelse(weight_kg >= ll_weight & weight_kg <= ul_weight, weight_kg, NA))
weight_sample <- weight_data%>%filter(!is.na(weight_2))

# only feeding days to 32
ft_daily_sample <- ft_daily%>%subset(case_no %in% weight_sample$case_no)

# if milk is 0 then speed cant be 0 
ft_daily_sample <- ft_daily_sample%>%mutate(speed2= ifelse((speed2 == 0& milk_day == 0), NA, speed2))
# make speed only within limits
ft_daily_sample <- ft_daily_sample%>%mutate(d_speed = ifelse(speed2 > 1500 | speed2 < 80, NA, speed2))

# write files 
ft_daily_sample32 <- ft_daily_sample%>%filter(feeding_day <= 32)

write.csv(ft_daily_sample, paste(out_dir, 'integrated_file_sample.csv', sep = '/'), row.names = FALSE)
write.csv(ft_daily_sample32, paste(out_dir, 'integrated_file_sample32.csv', sep = '/'), row.names = FALSE)
