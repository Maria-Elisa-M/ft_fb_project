# Maria Elisa Montes
# february 2022

# marginal effects
library(ggplot2) # plots
library(dplyr) # code syntax
library(emmeans) # ls means
library(stringr) # text 

in_dir <- 'models'
out_dir <- 'plots'

fit_ds <- readRDS(paste(in_dir, 'fit_ds.rds', sep = '/'))
fit_tm <- readRDS(paste(in_dir, 'fit_tm.rds', sep = '/'))

ds_contrasts <-  emmeans(fit_ds, pairwise ~ rdd_pneu_all)
tm_contrasts <-  emmeans(fit_tm, pairwise ~ rdd_pneu_all)

# Drinking speed -----

ds_contrasts_df <- data.frame(ds_contrasts$emmeans)
ds_healthy <- ds_contrasts_df%>%filter(rdd_pneu_all == - 7)
ds_sick <- ds_contrasts_df%>%filter(rdd_pneu_all != -7 & rdd_pneu_all != 7)
ds_significant <-data.frame(ds_contrasts$contrasts)%>%
  filter(p.value <=0.05)
ds_significant$daytemp <-str_replace_all(ds_significant$contrast,'[\\(\\)]', '')
ds_significant$day1 <-str_extract(ds_significant$daytemp,'^[-]?\\d{1}')
ds_significant$daytemp2 <-str_replace(ds_significant$daytemp,'^[-]?\\d{1}', '')
ds_significant$day2 <-str_replace(ds_significant$daytemp2,'[-]{1}', '')
ds_significant$day2 <- str_trim(ds_significant$day2)
ds_significant$daytemp2 <- NULL
ds_significant$daytemp <- NULL
ds_significant_baseline <- ds_significant%>%filter(day1 == -7 & day2 !=7)%>%
  merge(ds_sick, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
  mutate(y = emmean + SE.y + 0.25)%>%mutate(x = as.numeric(day2) + 6)

ds_significant_prev <- ds_significant%>%mutate(dif = as.numeric(day2) -as.numeric(day1))%>%filter(dif == 1)%>%
  merge(ds_sick, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
  mutate(y = emmean + SE.y + 0.8)%>%mutate(x = as.numeric(day2) + 6)

p <- ggplot(ds_sick, aes(x = rdd_pneu_all, y = emmean)) + geom_point(size = 4) + 
  geom_errorbar(aes(ymin=emmean - SE, ymax = emmean + SE)) + 
  theme_classic() +ylim(c(460, 510)) + 
  geom_hline(yintercept= ds_healthy$emmean, size =1) +
  geom_hline(yintercept= ds_healthy$emmean - ds_healthy$SE, linetype = 'dashed') +
  geom_hline(yintercept= ds_healthy$emmean + ds_healthy$SE, linetype = 'dashed') +
  xlab('Relative day to BRD treatment')+ ylab('Drinking speed ml/min')+
  annotate("text", x = ds_significant_baseline$x+0.1 , y = ds_significant_baseline$y, label = '*', size = 10) +
  annotate("text", x = ds_significant_prev$x-0.1 , y = ds_significant_prev$y, label = 't', size = 6) +
  theme(text = element_text(size = 35))

pdf(paste(out_dir, 'ds_plot.pdf', sep = '/'), width = 12, height = 10)
  print(p)
dev.off()


# total milk ------
tm_contrasts_df <- data.frame(tm_contrasts$emmeans)
tm_healthy <- tm_contrasts_df%>%filter(rdd_pneu_all == - 7) 
tm_sick <- tm_contrasts_df%>%filter(rdd_pneu_all != -7 & rdd_pneu_all != 7)


tm_significant <-data.frame(tm_contrasts$contrasts)%>%
  filter(p.value <=0.05)
tm_significant$daytemp <-str_replace_all(tm_significant$contrast,'[\\(\\)]', '')
tm_significant$day1 <-str_extract(tm_significant$daytemp,'^[-]?\\d{1}')
tm_significant$daytemp2 <-str_replace(tm_significant$daytemp,'^[-]?\\d{1}', '')
tm_significant$day2 <-str_replace(tm_significant$daytemp2,'[-]{1}', '')
tm_significant$day2 <- str_trim(tm_significant$day2)
tm_significant$daytemp2 <- NULL
tm_significant$daytemp <- NULL
tm_significant_baseline <- tm_significant%>%filter(day1 == -7 & day2 !=7)%>%
  merge(tm_sick, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
  mutate(y = emmean + SE.y + 0.01)%>%mutate(x = as.numeric(day2) + 6)

tm_significant_prev <- tm_significant%>%mutate(dif = as.numeric(day2) -as.numeric(day1))%>%filter(dif == 1)%>%
  merge(tm_sick, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
  mutate(y = emmean + SE.y + 0.03)%>%mutate(x = as.numeric(day2) + 6)

p <- ggplot(tm_sick, aes(x = rdd_pneu_all, y = emmean)) + geom_point(size = 4) + 
  geom_errorbar(aes(ymin=emmean - SE, ymax = emmean + SE)) + 
  theme_classic()+ylim(c(8.1, 9.3)) + scale_y_continuous(breaks = c(8.2, 8.4, 8.6, 8.8, 9.0, 9.2)) +
  geom_hline(yintercept= tm_healthy$emmean) +
  geom_hline(yintercept= tm_healthy$emmean - tm_healthy$SE, linetype = 'dashed') +
  geom_hline(yintercept= tm_healthy$emmean + tm_healthy$SE, linetype = 'dashed') +
  xlab('Relative day to BRD treatment')+ ylab('Daily milk consumption L') + 
  annotate("text", x = tm_significant_baseline$x +0.1 , y = tm_significant_baseline$y, label = '*', size = 10) +
  annotate("text", x = tm_significant_prev$x-0.1 , y = tm_significant_prev$y, label = 't', size = 6) +
  theme(text = element_text(size = 35))

pdf(paste(out_dir, 'milk_plot.pdf', sep = '/'), width = 12, height = 10)
  print(p)
dev.off()

# lactation & drinking speed ----
ds_lact_contrast <- emmeans(fit_ds, pairwise ~ parity)

ds_lact_contrast_df <- data.frame(ds_lact_contrast$emmeans)
ds_lact_significant_df <-data.frame(ds_lact_contrast$contrasts)%>%
  filter(p.value <=0.05)

p <- ggplot(ds_lact_contrast_df, aes( x= parity, y = emmean)) + geom_point(size = 5) +  
  geom_errorbar(aes(ymin=emmean - SE, ymax = emmean + SE), width = 0.75) + 
  theme_classic() +  
  ylab('Drinking speed ml/min') + 
  xlab('Parity') + 
  scale_x_discrete(labels=c("1","2","3+")) +
  annotate("text", x = ds_lact_contrast_df$parity, 
           y = (ds_lact_contrast_df$emmean + ds_lact_contrast_df$SE +2) , label = c('A', 'B', 'C'), size = 8) +
  theme(text = element_text(size = 35))

pdf(paste(out_dir, 'ds_lact_plot.pdf', sep = '/'), width = 12, height = 10)
  print(p)
dev.off()

# lactation & milk ----
tm_lact_contrast <- emmeans(fit_tm, pairwise ~ parity)

tm_lact_contrast_df <- data.frame(tm_lact_contrast$emmeans)
tm_lact_significant_df <-data.frame(tm_lact_contrast$contrasts)%>%
  filter(p.value <=0.05)

p <- ggplot(tm_lact_contrast_df, aes( x= parity, y = emmean)) + geom_point(size =5) +  
  geom_errorbar(aes(ymin=emmean - SE, ymax = emmean + SE), width = 0.75) + 
  theme_classic() +  
  ylab('Daily milk consumption L') + 
  xlab('Parity') + 
  scale_x_discrete(labels=c("1","2","3+")) +
  annotate("text", x = tm_lact_contrast_df$parity, 
           y = (tm_lact_contrast_df$emmean + tm_lact_contrast_df$SE +0.02) , label = c('A', 'B', 'B'), size = 8) +
  theme(text = element_text(size = 35))

pdf(paste(out_dir, 'milk_lact_plot.pdf', sep = '/'), width = 12, height = 10)
  print(p)
dev.off()


# costum contrast -----
A1 = c(0, 0, 1)
B2 = c(0, 1, 0)
C1 = c(1, 0, 0)
Boverall = (A1 + B2)/2

contrast(ds_lact, method = list("C - Boverall" = C1 - Boverall))

summary(fit_tm)

tm_lact = emmeans(fit_tm, 'LACT', mode = "satterth")
tm_lact = add_grouping(tm_lact, 'parity', 'LACT', c('prim', 'mult', 'mult'))
emmeans(tm_lact, ~parity)

contrast(tm_lact, method = list("C - Boverall" = C1 - Boverall))
