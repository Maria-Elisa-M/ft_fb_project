
library(ggplot2)
library(dplyr)
library(lubridate)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# read data ----
data_day <- read.csv(paste('outfiles', 'integrated_file_v2.csv', sep = '/'))
data_day <- data_day%>%filter(feeding_day <= 60)
data_day_nonsense <-data_day%>%filter(day_ent >= 30 |day_ent< 0)

data_day$event_date <- as.Date(data_day$event_date)

data_visit <- read.csv(paste('infiles/visits', 'visits_file1.csv', sep = '/'))
data_visit <- data_visit%>%mutate(visit_ent = Entitlement/1000)

data_day_sum <- data_day%>%group_by(feeding_day)%>%summarise(med_ent = median(day_ent,na.rm = TRUE),
                                                             min_ent = min(day_ent, na.rm = TRUE), 
                                                             max_ent = max(day_ent, na.rm = TRUE))
write.csv(data_day_sum, 'milk_plan_day.csv', row.names = FALSE)


data_visit_sum <- data_visit%>%group_by(feedingDay)%>%summarise(med_ent = median(visit_ent,na.rm = TRUE),
                                                             min_ent = min(visit_ent, na.rm = TRUE), 
                                                             max_ent = max(visit_ent, na.rm = TRUE))
write.csv(data_visit_sum, 'milk_plan_visit.csv', row.names = FALSE)

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
