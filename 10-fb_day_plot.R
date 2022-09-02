# Maria Elisa Montes
# July 2022
#
# New model 

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(facetscales) # devtools::install_github("zeehio/facetscales")
 
# get data ----
out_dir <- "outfiles"


# FT data -----
data_full <- read.csv(paste(out_dir, 'integrated_file_sample.csv', sep = '/'))


# 60 days ----

data_full <- data_full%>%
  mutate(d_speed = ifelse(speed2 <= 0, NA, speed2), corrected_0 =  ifelse(speed2 <= 0, 1, 0))%>%
  mutate(visits_w_milk = ifelse(visits_w_milk <0 | visits_w_milk >1400, NA, visits_w_milk))%>%
  mutate(visits_wo_milk = ifelse(visits_wo_milk <0 | visits_wo_milk >1400, NA, visits_wo_milk))%>%
  group_by(feeding_day)%>%
  mutate(mean_speed = mean(d_speed, na.rm = TRUE), sd_speed = sd(d_speed, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(speed_clean = ifelse(d_speed < mean_speed - 4*sd_speed | d_speed > mean_speed + 4*sd_speed, NA, d_speed), 
         corrected_sd = ifelse(d_speed < mean_speed - 4*sd_speed | d_speed > mean_speed + 4*sd_speed,1, 0))


data60 <- data_full%>%
  filter(feeding_day <= 60)%>%
  select(c('feeding_day', 'visits_w_milk', 'visits_wo_milk', 'speed_clean', 'total_milk'))

rm(list = setdiff(ls(), c('data60')))


data60_long <- data60%>%
  pivot_longer(cols = c('visits_w_milk', 'visits_wo_milk', 'speed_clean', 'total_milk'), 
               names_to = 'variable', values_to = 'value')

data60_long_g <- data60_long%>%
  filter(!is.na(value))%>%
  group_by(feeding_day, variable)%>%
  summarize(n = n(),
            mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE),
            se = sd/sqrt(n-1),
            q25 = quantile(value, c(1/4)),
            q50 = quantile(value, c(2/4)),
            q75 = quantile(value, c(3/4)),
            ll = q25, 
            ul = q75)%>%
  mutate(f_stage = ifelse(feeding_day < 11, '2 L/2 hours', 
                          ifelse(feeding_day>=11 & feeding_day <= 20, '2.5 L/v2 hours', 
                                 ifelse(feeding_day >= 21 & feeding_day <= 32, '3 L/2 hours', ' weaning'))))%>%
  mutate(ll = ifelse(ll < 0 , 0, ll))%>%
  mutate(ll = ifelse(ll <100 & variable == 'speed_clean', 100, ll))%>%
  mutate(ul = ifelse(ul > 800 & variable == 'speed_clean', 800, ul))


scales_y <- list(
  `total_milk` = scale_y_continuous(limits = c(0, 15), breaks = seq(1, 15, by= 2)),
  `visits_w_milk` = scale_y_continuous(limits = c(0, 15), breaks = seq(1, 15, by= 2)),
  `visits_wo_milk` = scale_y_continuous(limits = c(0, 15), breaks = seq(1, 15, by= 2)),
  `speed_clean` = scale_y_continuous(limits = c(100, 800), breaks = seq(100, 800, by = 100)))

plot <- ggplot(data60_long_g, aes(x = feeding_day, y = mean)) + 
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = 'gray', alpha = 0.5)+
  geom_line(size = 1) +
  geom_line(aes(y = q50), linetype = 'dotted', size = 1) +
  facet_grid_sc(variable~ ., scales = list(y = scales_y), switch = "y",
             labeller = as_labeller(c(total_milk = "Daily milk consumption, L", 
                                      visits_w_milk = "Daily rewarded visits, count", 
                                      visits_wo_milk = "Daily unrewarded visits, count", 
                                    speed_clean = "Drinking speed, ml/min")) ) +
  geom_vline(xintercept = 32, size = 1, linetype = 'dashed') +
  geom_vline(xintercept = 32, size = 1, linetype = 'dashed') +
  geom_vline(xintercept = 32, linetype = "dashed", color= "black", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color= "black", size = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color= "black", size = 0.7) +
  theme_classic() +
  ylab(NULL) + # remove the word "values"
  theme(strip.background = element_blank(), # remove the background
        strip.placement = "outside", 
        strip.text = element_text(size = 20,family = "serif")) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = margin( t= 0, r = 15, b = 0, l = 0, unit = "pt"),
        axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        text = element_text(family = "serif")) +
  scale_x_continuous(breaks = seq(0,60, by =5), expand=c(0,0)) +
  xlab('Feeding day') 

plot_data <- data.frame(feeding_day = seq(0,60, by = 1))%>%mutate(y = 800) 
  

plot2 <- ggplot(plot_data, aes(x = feeding_day, y = y)) +
  theme_classic() +
theme(panel.border = element_rect(colour = "black", fill=NA),
      plot.margin = margin( t= 0, r = 15, b = 0, l = 0, unit = "pt"),
      axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
      text = element_text(family = "serif")) +
  annotate('text', y = 20, x = 5, label = '2 L/2h', family = 'serif', size = 8) +
  annotate('text', y = 20, x = 15, label = '2.5 L/2h', family = 'serif', size = 8) +
  annotate('text', y = 20, x = 27, label = '3 L/2h', family = 'serif', size = 8) +
  geom_vline(xintercept = 32, size = 1, linetype = 'dashed') +
  geom_vline(xintercept = 32, size = 1, linetype = 'dashed') +
  geom_vline(xintercept = 32, linetype = "dashed", color= "black", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color= "black", size = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color= "black", size = 0.7) +
  scale_x_continuous(breaks = seq(0,60, by =5), expand=c(0,0), limits = c(0,60)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(color = "white"),
        axis.ticks.y=element_line(color = "white"),
        axis.text.y=element_text(color = 'white')
        )

plot3 <- ggplot(plot_data, aes(x = feeding_day, y = y)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = margin( t= 0, r = 15, b = 0, l = 0, unit = "pt"),
        axis.title = element_text(size = 18), axis.text = element_text(size = 15), 
        text = element_text(family = "serif")) +
  annotate('text', y = 20, x = 15, label = 'up to 24 L/day', family = 'serif', size = 8) +
  annotate('text', y = 20, x = 45, label = 'gradual weaning', family = 'serif', size = 8) +
  geom_vline(xintercept = 32, size = 1, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(0,60, by =5), expand=c(0,0), limits = c(0,60)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(color = "white"),
        axis.ticks.y=element_line(color = "white"),
        axis.text.y=element_text(color = 'white')
  )
plot <- cowplot::plot_grid(plot3, plot2, plot, ncol = 1, rel_heights = c(0.025,0.025,0.95))

pdf('plots/all_fb_fd.pdf', height= 17, width = 15)
print(plot)
dev.off()



