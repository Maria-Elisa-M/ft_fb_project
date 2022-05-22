# Maria Elisa Montes
# May 2022
#
# Part 8

library(dplyr)
library(scales) 
library(tidyr)
library(emmeans)
library(lmerTest)
library(lubridate)

# get data ----

in_dir <- "outfiles"
out_dir <- "models"

# read data 
ft_data <- read.csv(paste(in_dir, 'integrated_file_sample32.csv', sep = '/'))

# center and re-scale data -----
list_rescale <- c('weight_kg', 'thi')
ft_data_cr <- ft_data
ft_data_cr[ ,list_rescale]  <- scale(ft_data_cr[ ,list_rescale], center = TRUE)

# data formats -----
ft_data_cr$event_date <- as_date(ft_data_cr$event_date)
ft_data_cr$parity <- as.factor(ft_data_cr$parity)
ft_data_cr$feeder <- as.factor(ft_data_cr$feeder)
ft_data_cr$rdd_pneu_all <- as.factor(ft_data_cr$rdd_pneu_all)

# milk consumption ------
# milk model 2 V1
fit_tm <- lmer(total_milk ~ parity + feeding_day + thi + weight_kg + rdd_pneu_all 
                + feeding_day * thi
                + feeding_day * weight_kg
                + (1| feeder/case_no), ft_data_cr)

saveRDS(fit_tm, paste(out_dir, "fit_tm.rds", sep = '/'))
summary(fit_tm)

# check variance inflation factor
car::vif(fit_tm)

# chek for normality of residuals
qqnorm(resid(fit_tm))
qqline(resid(fit_tm))
shapiro.test(sample(resid(fit_tm), 5000))

MuMIn::r.squaredGLMM(fit_tm)

# drinking speed ------
fit_ds <- lmer(d_speed ~ parity + feeding_day + thi + total_milk + weight_kg + rdd_pneu_all 
                + weight_kg * thi * feeding_day + (1| feeder/case_no), ft_data_cr)

saveRDS(fit_ds, paste(out_dir, "fit_ds.rds", sep = '/'))
summary(fit_ds)

# check variance inflation factor
car::vif(fit_ds)

# chek for normality of residuals
qqnorm(resid(fit_ds))
qqline(resid(fit_ds))
shapiro.test(sample(resid(fit_ds), 5000))

MuMIn::r.squaredGLMM(fit_ds)
