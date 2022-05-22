# Maria Elisa Montes
# february 2022

# marginal effects
library(ggplot2) # plots
library(dplyr) # code syntax

in_dir <- 'outfiles'
out_dir <- 'plots'

females_32_2 <- read.csv(paste(in_dir, 'integrated_file_sample32.csv', sep = '/'))
females_60 <- read.csv(paste(in_dir, 'integrated_file_sample.csv', sep = '/'))

females_60_2 <- females_60%>%filter(feeding_day <= 60)

# Milk visits plot 

milk_summary <- females_60_2%>%group_by(feeding_day)%>%summarize(mean = mean(total_milk, na.rm= TRUE), sdev = sd(total_milk, na.rm = TUE))

ggplot(females_60_2, aes(x= feeding_day, y = total_milk)) + theme_classic() + 
  stat_summary(geom = 'errorbar', fun.data = mean_sdl,
               fun.args = list(mult = 1)) +
  stat_summary(geom = 'point', fun = mean) +
  ylab('Milk consumption L') + xlab('Feeding day') + scale_x_continuous(breaks = seq(0, 60, by =5)) +
  scale_y_continuous(breaks = seq(0, 16, by =2))

ggplot(females_60_2, aes(x= feeding_day)) + theme_classic() + 
  #stat_summary(aes(y = visits_w_milk), geom = 'errorbar', fun.data = mean_sdl, fun.args = list(mult = 1), color = 'coral') +
  stat_summary(aes(y = visits_w_milk), geom = 'point', fun = mean, color = 'coral') +
  # stat_summary(aes(y = visits_wo_milk), geom = 'errorbar', fun.data = mean_sdl, fun.args = list(mult = 1)) +
  stat_summary(aes(y = visits_wo_milk), geom = 'point', fun = mean) +
  ylab('Number of visits') + xlab('Feeding day') + scale_x_continuous(breaks = seq(0, 60, by =5)) + 
  scale_y_continuous(breaks = seq(0, 25, by =2), limits = c(0,24))


ggplot(females_60_2, aes(x= feeding_day)) + 
  geom_smooth(aes(y = milk), color = "black", se = TRUE, size = 2) + 
  ylab('Milk consumption L') + xlab('Feeding day') + 
  scale_x_continuous(breaks = seq(0, 60, by =5)) + 
  scale_y_continuous(breaks = seq(0, 14, by =2)) +
  theme_classic()

# save this plot -----
clr <- c("milk" = "black", "rewarded visits"="grey", "unrewarded visits" = "coral")
p <- ggplot(females_60_2, aes(x= feeding_day)) +  
  geom_smooth(aes(y = total_milk, color = "milk"), se = FALSE, size = 2)+
  geom_smooth(aes(y = visits_w_milk, color = "rewarded visits"), se = FALSE, size = 2) + 
  geom_smooth(aes(y = visits_wo_milk, color =  "unrewarded visits"), se = FALSE, size = 2) + 
  ylab('Milk consumption L / Number of visits') + xlab('Feeding day') + 
  scale_color_manual(values = clr, name = "") +
  scale_x_continuous(breaks = seq(0, 60, by =5)) + 
  scale_y_continuous(breaks = seq(0, 16, by =2)) +
  geom_vline(xintercept = 32, linetype = "dashed", color= "black", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color= "black", size = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color= "black", size = 0.7) +
  theme_classic() + theme(axis.text = element_text(size = 25),
                          axis.title = element_text(size = 20),
                          legend.text = element_text(size = 20), 
                          legend.position="bottom")

pdf(paste(out_dir, 'feedingday_milkvisit_plot.pdf', sep = '/'), width = 15, height = 12)
  print(p)
dev.off()

# plot for drinking speed
clr <- c("drinking speed" = "black")
ggplot(females_60_2, aes(x= feeding_day)) +  
  geom_smooth(aes(y = speed2, color = "drinking speed"), se = FALSE, size = 2) +
  scale_color_manual(values = clr, name = "") +
  ylab('Drinking speed ml/min') + xlab('Feeding day') + 
  scale_x_continuous(breaks = seq(0, 60, by =5)) + 
  scale_y_continuous(breaks = seq(400, 800, by =100)) +
  geom_vline(xintercept = 32, linetype = "dashed", color= "black", size = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color= "black", size = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color= "black", size = 0.7) +
  theme_classic() + theme(axis.text = element_text(size = 25),
                          axis.title = element_text(size = 20),
                          legend.text = element_text(size = 20), 
                          legend.position="bottom")

pdf(paste(out_dir, 'feedingday_dspeed_plot.pdf', sep = '/'), width = 15, height = 12)
  print(p)
dev.off()


# plot demographic data from the sample
demo_data <- females_32_2%>%group_by(case_no)%>%summarise(brd = max(inc_pneu), 
                                                        lact_no = max(LACT), 
                                                        weight= max(weight))

ggplot(demo_data , aes(x = lact_no)) +  geom_bar() + theme_classic() + 
  theme(text = element_text(size = 35)) +
  scale_y_continuous(breaks = seq(0, 5000, by =500)) + 
  scale_x_continuous(breaks = seq(0, 7, by =1)) +
  ylab('Number of calves') + xlab('Parity of the dam')

ggplot(demo_data , aes(x = brd)) +  geom_bar() + theme_classic() + 
  theme(text = element_text(size = 35)) +
  scale_y_continuous(breaks = seq(0, 6000, by =500)) + 
  ylab('Number of calves') + xlab('BRD incidences 0-32 days')

ggplot(demo_data , aes(x = weight)) +  geom_histogram() + theme_classic() + 
  scale_y_continuous(breaks = seq(0, 1400, by =200)) +
  theme(text = element_text(size = 35)) + 
  ylab('Number of calves') + xlab('Birth weight kg')

females_32_2$event_date <- as.Date(females_32_2$event_date)
females_32_2$BDAT <- as.Date(females_32_2$BDAT)
max(females_32_2$BDAT)
min(females_32_2$BDAT)

females_32_2$parity <- as.factor(females_32_2$parity)
# Lactation plots -----
# inc of BRD by lact
pneu_inc <- females_32_2%>%group_by(case_no, parity)%>%summarise(pneu= max(inc_pneu))
ggplot(pneu_inc, aes(x = pneu)) + geom_bar() + facet_wrap(.~parity)
pneu_inc <- pneu_inc%>%mutate(yn_pneu = ifelse(pneu >0, 1, 0))
ggplot(pneu_inc, aes(x = yn_pneu)) + geom_bar() + facet_wrap(.~parity)
inc_par <- pneu_inc%>%group_by(parity)%>%summarise(n = n(), pneu = sum(yn_pneu), perc = pneu/n)

# inc of injury by lact
pneu_inc <- females_32_2%>%mutate(inj = ifelse(is.na(injury), 0, 1))%>%group_by(case_no, parity)%>%summarise(pneu= max(inj, na.rm = T))
ggplot(pneu_inc, aes(x = pneu)) + geom_bar() + facet_wrap(.~LACT)
pneu_inc <- pneu_inc%>%mutate(yn_pneu = ifelse(pneu >0, 1, 0))
ggplot(pneu_inc, aes(x = yn_pneu)) + geom_bar() + facet_wrap(.~parity)
inc_par <- pneu_inc%>%group_by(LACT)%>%summarise(n = n(), pneu = sum(yn_pneu), perc = pneu/n)

# inc of scour by lact
pneu_inc <- females_32_2%>%mutate(inj = ifelse(is.na(inc_scour), 0, 1))%>%group_by(case_no, parity)%>%summarise(pneu= max(inj, na.rm = T))
ggplot(pneu_inc, aes(x = pneu)) + geom_bar() + facet_wrap(.~parity)
pneu_inc <- pneu_inc%>%mutate(yn_pneu = ifelse(pneu >0, 1, 0))
ggplot(pneu_inc, aes(x = yn_pneu)) + geom_bar() + facet_wrap(.~parity)
inc_par <- pneu_inc%>%group_by(LACT)%>%summarise(n = n(), pneu = sum(yn_pneu), perc = pneu/n)

# birth weight
bw <- females_32_2%>%group_by(case_no, parity)%>%summarise(weight= max(weight, na.rm = T))
bw$parity <- as.factor(bw$parity)
ggplot(bw , aes(y = weight)) + geom_boxplot(aes(fill= parity))


# twins by lact
data$LACT <- as.factor(data$parity)
data <- females_32_2%>%group_by(case_no)%>%filter(feeding_day == min(feeding_day))%>%select(c('season', 'case_no', 'parity'))%>%unique()
