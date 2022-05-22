# Maria Elisa Montes
# May 2022
#
# Part 5

library(dplyr)
library(tidyr)
library(lubridate)

# get data ----

in_dir <- "outfiles"
out_dir <- "outfiles"

ft_daily <- read.csv(paste(in_dir, 'integrated_file.csv', sep = '/'))
ft_daily$event_date <- as_date(ft_daily$event_date)

# feeder -------
# feeder fill
# Fill up feeder since last record in a feeder contains the feeder number
ft_daily <- ft_daily%>%
  group_by(case_no)%>%
  arrange(event_date, .by_group = TRUE)%>%
  fill(feeder, .direction = "up")%>%
  ungroup()


# look up for days with multiple records
ft_daily_2 <- ft_daily%>%
  unique()%>% # make sure they are unique records
  group_by(case_no, event_date)%>%
  mutate(count_day = row_number())%>%
  mutate(double = ifelse(any(count_day==2), 1, 0))%>%
  ungroup() # enumerate different records

ft_daily_dup <- ft_daily_2%>%
  group_by(case_no)%>%
  filter(any(count_day==2))%>%
  arrange(event_date)%>%
  mutate(lag = feeding_day - lag(feeding_day))%>%
  mutate(event_date = ifelse(lag > 1 & double ==1, event_date - 1, event_date))

ft_daily_dup$event_date <- as_date(ft_daily_dup$event_date)

# get the rest of the data 
ft_daily_unique <- ft_daily_2%>%
  group_by(case_no)%>%
  filter(!any(count_day==2))%>%
  mutate(lag = NA)

ft_daily_3 <- rbind(ft_daily_unique, ft_daily_dup)

# compile records into one per day----
ft_daily_3 <- ft_daily_3%>%
  group_by(case_no, event_date)%>%
  mutate(time = (milk_day*1000/speed_absolute))%>% 
  mutate(total_time = sum(time, na.rm = TRUE))%>% # total time drinking milk
  mutate(total_milk = sum(abs(milk_day), na.rm = TRUE)*1000)%>%  # there are negative milk intakes
  mutate(speed2 = total_milk/total_time)%>%
  mutate(visits_w_milk = sum(visits_w_milk))%>%
  mutate(visits_wo_milk = sum(visits_wo_milk))

# change infinite and NAN for NA
ft_daily_3 <- ft_daily_3%>%
  mutate(speed2 = ifelse(is.nan(speed2), NA, speed2))%>%
  mutate(speed2 = ifelse(is.infinite(speed2), NA, speed2))%>%
  mutate(total_time = ifelse(is.nan(total_time), NA, total_time))%>%
  mutate(time = ifelse(is.infinite(time), NA, time))%>%
  mutate(total_milk = total_milk/1000)

ft_daily_3 <- ft_daily_3%>%
  group_by(case_no, event_date)%>%
  mutate(count_day = row_number())%>%
  mutate(double = ifelse(any(count_day==2), 1, 0))%>%
  ungroup() # enumerate different records


# now keep just one 
ft_daily_4 <- ft_daily_3%>%
  filter(count_day == 1)

# feeding stage ---
ft_daily_5 <- ft_daily_4%>%
  mutate(fd_group = ifelse(feeding_day <= 10, 1,
                           ifelse(feeding_day >10 & feeding_day <= 20, 2,
                                  ifelse(feeding_day >20 & feeding_day <= 30, 3,
                                         ifelse(feeding_day >30 & feeding_day <= 40, 4,
                                                ifelse(feeding_day >40 & feeding_day <= 50, 5,
                                                       ifelse(feeding_day >50 & feeding_day <= 65, 6, 0)))))))

# weigh in kg ----
ft_daily_5 <- ft_daily_5%>%mutate(weight_kg = weight * 0.454)

# parity ------
ft_daily_5 <- ft_daily_5%>%mutate(parity = ifelse(LACT >= 3, 3, LACT))
  
# days in feeder ----
calf_data <- ft_daily_5%>%
  group_by(case_no)%>%
  summarise(feeding_daymax = max(feeding_day), 
            feeding_daymin = min(feeding_day),
            days = n())

# days in feeder ----
calf_sample <- calf_data%>%filter(feeding_daymax >= 50 & feeding_daymax <= 150 & days >= 50)
daily_sample <- subset(ft_daily_5, case_no %in% calf_sample$case_no)

rm(ft_daily_5, ft_daily_4, ft_daily_3, ft_daily_2)

# day relative to treatment -----

# derive detection date by incidence_no----
pneu_evets <- daily_sample%>%
  group_by(case_no)%>%
  summarise(inc_pneu = max(inc_pneu, na.rm = TRUE))

pneu_evets3 <- pneu_evets%>%group_by(inc_pneu)%>%summarise(count = n())
max_inc <- max(pneu_evets3$inc_pneu)
daily_data2 <-daily_sample
i <- 1

while( i <= max_inc){
  colname <- paste('dd_pneu', i, sep = '_')
  daily_data2 <- daily_data2%>%
    group_by(case_no)%>%
    mutate('dd_pneu_{{i}}' := ifelse(inc_pneu == i, event_date, NA))%>%
    fill(all_of(colname), .direction = 'downup')
  daily_data2[[colname]] <- as_date(daily_data2[[colname]])
  
  i <- i +1
}

# get the days from detection for each of de incidences
daily_data3 <- daily_data2 %>%
  group_by(case_no)%>%
  mutate(rdd_pneu_1 = ifelse(!is.na(dd_pneu_1),  event_date - dd_pneu_1, NA))%>%
  mutate(rdd_pneu_2 = ifelse(!is.na(dd_pneu_2),  event_date - dd_pneu_2, NA))%>%
  mutate(rdd_pneu_3 = ifelse(!is.na(dd_pneu_3),  event_date - dd_pneu_3, NA))%>%
  mutate(rdd_pneu_4 = ifelse(!is.na(dd_pneu_4),  event_date - dd_pneu_4, NA))

daily_data3 <- daily_data3%>%
  group_by(case_no)%>%
  arrange(event_date)%>%
  fill(inc_pneu, .direction = 'down')%>%
  mutate(inc_pneu = ifelse(is.na(inc_pneu), 0, inc_pneu))

daily_data4 <- daily_data3%>%
  mutate(inc_pneu_f = inc_pneu)%>%
  mutate(inc_pneu_f = ifelse(!is.na(dd_pneu_1) & rdd_pneu_1 >= -5 & rdd_pneu_1 <= 5, 1, inc_pneu_f))%>%
  mutate(inc_pneu_f = ifelse(!is.na(dd_pneu_2) & rdd_pneu_2 >= -5 & rdd_pneu_2 <= 5, 2, inc_pneu_f))%>%
  mutate(inc_pneu_f = ifelse(!is.na(dd_pneu_3) & rdd_pneu_3 >= -5 & rdd_pneu_3 <= 5, 3, inc_pneu_f))%>%
  mutate(inc_pneu_f = ifelse(!is.na(dd_pneu_4) & rdd_pneu_4 >= -5 & rdd_pneu_4 <= 5, 4, inc_pneu_f))

daily_data5 <- daily_data4%>%
  mutate(rdd_pneu_all = 7)%>%
  mutate(rdd_pneu_all = ifelse(!is.na(dd_pneu_1) & rdd_pneu_1 >= -5 & rdd_pneu_1 <= 5, rdd_pneu_1, rdd_pneu_all))%>%
  mutate(rdd_pneu_all = ifelse(!is.na(dd_pneu_2) & rdd_pneu_2 >= -5 & rdd_pneu_2 <= 5, rdd_pneu_2, rdd_pneu_all))%>%
  mutate(rdd_pneu_all = ifelse(!is.na(dd_pneu_3) & rdd_pneu_3 >= -5 & rdd_pneu_3 <= 5, rdd_pneu_3, rdd_pneu_all))%>%
  mutate(rdd_pneu_all = ifelse(!is.na(dd_pneu_4) & rdd_pneu_4 >= -5 & rdd_pneu_4 <= 5, rdd_pneu_4, rdd_pneu_all))  

daily_data0 <- daily_data5%>%
  mutate(rdd_pneu_all = ifelse(inc_pneu_f == 0,  -7, rdd_pneu_all))

rm(daily_data2, daily_data3, daily_data4, daily_data5)

drop <- c('rdd_pneu_1', 'rdd_pneu_2', 'rdd_pneu_3', 'rdd_pneu_4', 'dd_pneu_1','dd_pneu_2','dd_pneu_3', 'dd_pneu_4')

daily_data <- daily_data0%>%select(-all_of(drop))

write.csv(daily_data, paste(out_dir, 'integrated_file_v2.csv', sep = '/'), row.names = FALSE)
