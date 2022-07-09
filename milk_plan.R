
library(ggplot2)
library(dplyr)
library(lubridate)

in_dir <- 'outfiles'
out_dir <- 'milk_plan'

# read data ----
data_day <- read.csv(paste(in_dir , 'integrated_file_sample.csv', sep = '/'))
data_visit <- read.csv(paste('infiles/visits', 'visits_file1.csv', sep = '/'))
data_day <- data_day%>%filter(feeding_day <= 60)


data_day_nonsense <-data_day%>%filter(day_ent >= 30 |day_ent< 0)

write.csv(data_day_nonsense, paste(out_dir, 'milk_plan_nonsense.csv', sep = '/'), row.names = FALSE)

data_day$event_date <- as.Date(data_day$event_date)



data_visit <- data_visit%>%mutate(visit_ent = Entitlement/1000)

data_day_sum <- data_day%>%group_by(feeding_day)%>%summarise(med_ent = median(day_ent,na.rm = TRUE),
                                                             min_ent = min(day_ent, na.rm = TRUE), 
                                                             max_ent = max(day_ent, na.rm = TRUE))

write.csv(data_day_sum, paste(out_dir, 'milk_plan_day.csv', sep = '/'), row.names = FALSE)


data_visit_sum <- data_visit%>%group_by(feedingDay)%>%summarise(med_ent = median(visit_ent,na.rm = TRUE),
                                                             min_ent = min(visit_ent, na.rm = TRUE), 
                                                             max_ent = max(visit_ent, na.rm = TRUE))
write.csv(data_visit_sum,  paste(out_dir,'milk_plan_visit.csv', sep = '/'), row.names = FALSE)

ggplot(data_day_sum, aes(x = feeding_day, y = med_ent)) + geom_point() +
  ylim(c(0,25))+
  theme_classic()+
  scale_x_continuous(breaks = seq(0, 60, 1))+
  scale_y_continuous(breaks = seq(0, 25, 0.5))

ggplot(data_visit, aes(x = feedingDay, y = visit_ent)) + geom_point() +
  xlim(c(0,70))+
  ylim(c(0,3))+
  theme_classic()+
  scale_x_continuous(breaks = seq(0, 70, 1))+
  scale_y_continuous(breaks = seq(0, 3, 0.25))


# before 2019
data_day_old <- data_day%>%filter(year(event_date) <= 2019)
data_day_sum_old <- data_day_old%>%group_by(feeding_day)%>%summarise(med_ent = median(day_ent,na.rm = TRUE),
                                                             min_ent = min(day_ent, na.rm = TRUE), 
                                                             max_ent = max(day_ent, na.rm = TRUE))
# above 24 before 40 days
# before 2019
data_day_40 <- data_day%>%filter(feeding_day <= 40 & day_ent >=0 &day_ent< 100)%>%
  mutate(ad_lib = ifelse(day_ent >= 20, 1,0))%>%
  mutate(ten_plus = ifelse(day_ent >= 10, 1,0))


data_day_40_calf <- data_day_40%>%group_by(case_no)%>%summarise(ad_lib_days = sum(ad_lib),
                                                                tenplus_days = sum(ten_plus),
                                                                mean_ent = mean(day_ent), 
                                                                intake = sum(milk_day), 
                                                                ent = sum(day_ent))

ggplot(data_day_40_calf, aes(x = ad_lib_days)) + geom_histogram() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,40, 1))

ggplot(data_day_40_calf, aes(x = tenplus_days)) + geom_histogram() +
  theme_classic()+
  scale_x_continuous(breaks = seq(0,40, 1))


ggplot(data_day_40_calf, aes(x = mean_ent)) + geom_histogram() +
  theme_classic() +
  scale_x_continuous(breaks = seq(5, 25, 1))


# exceeding entitlement
data_day_40_SE <- data_day_40_calf%>%mutate(int_ent = ifelse(intake > ent, 1, 0))


# counts 
calfs32 <- nrow(data_day_40_calf%>%filter(ad_lib_days >= 32))
calfs32/nrow(data_day_40_calf)

# counts 
calfs10 <- nrow(data_day_40_calf%>%filter(tenplus_days >= 32))
calfs10/nrow(data_day_40_calf)


# raw files
old <- read.csv('infiles/foerster_technik/ft_historic_2015_2019.csv')

groups <- old$feeder_name%>%unique()
old_s <- old%>%select(farm_animal_id, feeder_name, daily_milk_plan, feeding_day)

ggplot(old_s, aes(x = daily_milk_plan)) + geom_histogram() +
  theme_classic() + facet_wrap(.~feeder_name)


# raw files
new <- read.csv('infiles/foerster_technik/ft_calfcloud_2019_2021.csv')

groups <- new$feeder_name%>%unique()
new_s <- new%>%select(farm_animal_id, group, feed, feeding_day)

new_s <- new_s%>%filter(feed < 300 & feed >= 0)
ggplot(new_s, aes(x = feed)) + geom_histogram() +
  theme_classic() + facet_wrap(.~group)



