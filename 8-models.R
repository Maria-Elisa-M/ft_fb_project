# Maria Elisa Montes
# July 2022
#
# New model 

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lemon)
library(lubridate)
library(emmeans)
library(lmerTest)

emm_options(rg.limit = 30000)
set.seed(1)

# get data ----
out_dir <- "outfiles"


# FT data -----
data <- read.csv(paste(out_dir, 'integrated_file_sample32.csv', sep = '/'))

## date format ----
data$event_date <- as_date(data$event_date)
data$initial_date <- as_date(data$initial_date)

# grouped data -----
data32D <- data%>%group_by(case_no)%>%mutate(obs = n())%>%ungroup()

data32D_g <- data32D%>%
  mutate(extra_visit = ifelse(visits_wo_milk > 0, 1, 0))%>%
  filter(!is.na(parity))%>%
  filter(obs>= 32)%>%
  group_by(case_no, weight_kg, section, parity)%>%
  summarise(
    milk =mean(milk_day, na.rm = TRUE), 
            thi = round(mean(thi,  na.rm = TRUE), 1), 
            speed  = mean(d_speed, na.rm = TRUE),
            med_speed =median(d_speed),
            sum_milk = sum(milk_day),
            feeder = max(feeder),
            milk32 = milk*32,
            extra_visits = sum(extra_visit, na.rm = TRUE),
            temp = mean(maximum_temperature, na.rm = TRUE),
            obs = n(),
            day_ent = sum(day_ent)/obs,
            num_pneu = max(inc_pneu))%>%ungroup()

# 32 days models -------
data32D_g <- data32D_g%>%mutate(quartile_weight = ntile(weight_kg, 4))
data32D_g$quartile_weight <- as.factor(data32D_g$quartile_weight)
data32D_g <- data32D_g%>%mutate(quartile_thi = ntile(thi, 4))
data32D_g$quartile_thi <- as.factor(data32D_g$quartile_thi)
data32D_g$parity <- as.factor(data32D_g$parity)
data32D_g$feeder <- as.factor(data32D_g$feeder)
data32D_g <- data32D_g%>%mutate(num_pneu2 = ifelse(num_pneu >= 2, 2, num_pneu))
data32D_g$num_pneu2 <- as.factor(data32D_g$num_pneu2)

### visits mdel ----
fit32_visits <- glm(extra_visits ~ num_pneu2 + quartile_weight +  milk +parity + thi + offset((obs)),
                    data = data32D_g, family = 'quasipoisson')

summ_visits <- summary(fit32_visits)
with(summary(fit32_visits), 1 - deviance/null.deviance)

# corretaltion tests
cor.test(data32D_g$weight_kg,data32D_g$milk32 )
fit32_milk1 <- lm(milk32 ~ num_pneu2 + quartile_weight +  parity + quartile_thi,
                    data = data32D_g)

summary(fit32_milk1)

fit32_speed1 <- lm(speed ~ num_pneu2 + quartile_weight +  parity + quartile_thi,
                  data = data32D_g)

summary(fit32_speed1)

### residuals ------
# layout(matrix(c(1,2,3,4),2,2)) 
# plot(fit32_visits)

# tested for overdispersion in poisson, because it was overdispersed then fitted a quiasipoisson model
# AER::dispersiontest(fit32_visits, alternative = c('less'))




# Daily moldels -----
data32D_thi <-data32D%>%select(event_date, thi)%>%unique()
data32D_thi <- data32D_thi%>%mutate(quartile_thi = ntile(thi, 8))

data32D_weight <-data32D%>%select(case_no, weight_kg)%>%unique()
data32D_weight <- data32D_weight%>%mutate(quartile_weight = ntile(weight_kg, 4))

quantiles_thi <- data.frame(from = round(quantile(data32D_thi$thi, c(0,1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 7/8, 1), na.rm = TRUE),0))%>%
mutate(to = lead(from),range = paste(from, to, sep = '-'))

quantiles_weight <- data.frame(from = round(quantile(data32D_weight$weight_kg, c(0,1/4, 2/4,3/4, 1)),0))%>%
  mutate(to = lead(from),range = paste(from, to, sep = '-'))

data32D <- data32D%>%merge(select(data32D_weight, c(quartile_weight, case_no)), by = 'case_no', all.x = TRUE)
data32D <- data32D%>%merge(data32D_thi, by = c('event_date', 'thi'), all.x = TRUE)

data32D$parity <- as.factor(data32D$parity)
data32D$quartile_weight <- as.factor(data32D$quartile_weight)
data32D$quartile_thi <- as.factor(data32D$quartile_thi)
data32D$rdd_pneu_all <- as.factor(data32D$rdd_pneu_all)


## milk and speed model -----

fit32_milk <- lmer(total_milk ~  thi + rdd_pneu_all+ feeding_day + parity + 
                     quartile_weight + (1|feeder/case_no), data = data32D)

summ_milk <- summary(fit32_milk)
MuMIn::r.squaredGLMM(fit32_milk)

fit32_speed <- lmer(speed_clean ~  thi + rdd_pneu_all + parity + feeding_day +
                      quartile_weight + (1|feeder/case_no), data = data32D, 
                    control = lmerControl(optimizer ="Nelder_Mead"))

summ_speed <- summary(fit32_speed)
MuMIn::r.squaredGLMM(fit32_speed)

### residuals----
# residuals_milk32 <- resid(fit32_milk)
# plot(residuals_milk32)
# qqnorm(residuals_milk32)
# qqline(residuals_milk32)
# plot(fit32_milk)
# 
# residuals_speed32 <- resid(fit32_speed) 
# plot(residuals_speed32)
# qqnorm(residuals_speed32)
# qqline(residuals_speed32)
# plot(fit32_speed)


keep <- c('data32D', 'quantiles_weight', 'data32D_g', 'fit32_visits', 
          'fit32_speed', 'fit32_milk', 'summ_speed', 'summ_milk')

rm(list = setdiff(ls(), keep))

# plotting  ----

## plot functions ---------
plot_brd <- function(model, sh, dh, ylabel){
  brd_means <- as.data.frame(emmeans(model, 'rdd_pneu_all'))
  brd_pairs <- as.data.frame(pairs(emmeans(model, 'rdd_pneu_all')))
  
  brd_pairs$daytemp <- str_replace_all(brd_pairs$contrast,'[\\(\\)]', '')
  brd_pairs$day1 <-str_extract(brd_pairs$daytemp,'^[-]?\\d{1}')
  brd_pairs$daytemp2 <-str_replace(brd_pairs$daytemp,'^[-]?\\d{1}', '')
  brd_pairs$day2 <-str_replace(brd_pairs$daytemp2,'[-]{1}', '')
  brd_pairs$day2 <- str_trim(brd_pairs$day2)
  brd_pairs$daytemp2 <- NULL
  brd_pairs$daytemp <- NULL
  
  healthy_df <- brd_means%>%filter(rdd_pneu_all == -7)
  sick_df <- brd_means%>%filter(rdd_pneu_all != -7 & rdd_pneu_all != 7)
  
  diff_baseline <- brd_pairs%>%
    filter(p.value <=0.01)%>%
    filter(day1 == -7 & day2 !=7)%>%
    merge(sick_df, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
    mutate(y = emmean + SE.y + sh)%>%mutate(x = as.numeric(day2) + 6)
  
  diff_prev <- brd_pairs%>%
    filter(p.value <=0.01)%>%
    mutate(dif = as.numeric(day2) -as.numeric(day1))%>%filter(dif == 1)%>%
    merge(sick_df, by.x = 'day2', by.y = 'rdd_pneu_all', all.x = TRUE)%>%
    mutate(y = emmean + SE.y + dh )%>%mutate(x = as.numeric(day2) + 6)
  
  
  base_plot <- ggplot(sick_df, aes(x = rdd_pneu_all, y = emmean)) + geom_point(size = 4) + 
    geom_errorbar(aes(ymin=emmean - SE, ymax = emmean + SE)) + 
    theme_classic() + xlab('Relative day to BRD treatment') + ylab(ylabel)+
    geom_hline(yintercept= healthy_df$emmean) +
    geom_hline(yintercept= healthy_df$emmean - healthy_df$SE, linetype = 'dashed') +
    geom_hline(yintercept= healthy_df$emmean + healthy_df$SE, linetype = 'dashed') +
    annotate("text", x = diff_baseline$x +0.1 , y = diff_baseline$y, label = '*', size = 10) +
    annotate("text", x = diff_prev$x-0.1 , y = diff_prev$y, label = "\206", size = 8) +
    theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), 
          text = element_text(family = "serif")
    )
}

cat_var_plot <- function(model, var_name, ylabel, xlabel, label, group_vector, h){
  var_means <- as.data.frame(emmeans(model, var_name))
  var_means[,1] <- as.factor(var_means[,1])
  plot <- ggplot(var_means, aes(x = var_means[, var_name], y = emmean)) + 
    ylab(ylabel) + xlab(xlabel)+
    geom_point(size = 4) + geom_errorbar(aes(ymin = emmean - SE , ymax = emmean + SE)) +
    theme_classic() + 
    theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), 
          text = element_text(family = "serif"))
  #   )
  
  if(label == TRUE){
    plot <- plot+annotate("text", y = var_means$emmean + var_means$SE + h, 
                          x = var_means[[var_name]], label =group_vector, size = 10, family="serif")
    # x = var_means[[var_name]], label =group_vector, size = 6)
  }
  
  return(plot)
}

cont_var_plot <- function(fit_name, var_name, y_lab, x_lab){
  var_name <- var_name
  model <- fit_name
  model_data <- data.frame(model@frame)%>%select(c(eval(var_name)))
  
  colnames(model_data)[1] <- 'var'
  model_data$pred <-predict(model)
  #model_data$pred <- model@frame[,1]
  min_var <- round(min(model_data[,1]))
  max_var <- round(max(model_data[,1]))
  
  list_var <- list(var = seq(min_var,max_var, by = 0.5), feeder = 0, case_no = 0)
  names(list_var)[1] <- c(eval(var_name))
  
  
  model_predict <- emmeans(model, specs = c(eval(var_name)), 
                           at = list_var, 
                           lmer.df = "satterthwaite")%>%data.frame()
  data1 <- model_data%>%group_by(var)%>%
    summarise(pred = mean(pred),size = n())
  
  plot <- ggplot(model_predict, aes(x =model_predict[,var_name])) +
    geom_ribbon(aes(ymin = emmean - SE, ymax= emmean + SE),  linetype = c('dashed'), alpha = 0.2, fill = 'gray20') +
    geom_line(aes(y = emmean), size = 1.5)  + 
    theme_classic() +
    ylab(y_lab)+ xlab(x_lab) +
    theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), 
          text = element_text(family = "serif"))
  
  if(nrow(model_predict) > nrow(data1)){
    res <- TRUE
    data2 <- sample_n(data1, nrow(model_predict), replace = res)
    plot <- plot + geom_jitter(aes(y =data2$pred, x = data2$var),alpha = 0.2)
  }else{
    res <- FALSE
    data2 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data2)
    data3 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data3)
    data4 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data4)
    data5 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data5)
    data6 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data6)
    data7 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data7)
    data8 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data8)
    data9 <- sample_n(data1, nrow(model_predict), replace = res)
    data1 <- anti_join(data1, data9)
    data10 <- sample_n(data1, nrow(model_predict), replace = res)
    
    
    plot <- plot + geom_jitter(aes(y =data2$pred, x = data2$var), color = 'gray60') +
      geom_jitter(aes(y =data3$pred, x = data3$var), color = 'gray60') +
      geom_jitter(aes(y =data4$pred, x = data4$var), color = 'gray60') +
      geom_jitter(aes(y =data5$pred, x = data5$var), color = 'gray60') +
      geom_jitter(aes(y =data6$pred, x = data6$var), color = 'gray60') +
      geom_jitter(aes(y =data7$pred, x = data7$var), color = 'gray60') +
      geom_jitter(aes(y =data8$pred, x = data8$var), color = 'gray60') +
      geom_jitter(aes(y =data9$pred, x = data9$var), color = 'gray60') +
      geom_jitter(aes(y =data10$pred, x = data10$var), color = 'gray60')
  }
  return(plot)
}

cat_var_plot_visit <- function(model, var_name, ylabel, xlabel, label, group_vector, h){
  
  var_means <- as.data.frame(emmeans(model, var_name, offset = log(32)))%>%
    mutate(emmean2 = exp(emmean), 
           ll = exp(emmean - SE), 
           ul = exp(emmean + SE))
  
  var_means[,1] <- as.factor(var_means[,1])
  
  plot <- ggplot(var_means, aes(x = var_means[, var_name], y = emmean2)) + 
    ylab(ylabel) + xlab(xlabel)+
    geom_point(size = 4) + geom_errorbar(aes(ymin = ll , ymax = ul)) +
    theme_classic() + 
    theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), 
          text = element_text(family = "serif"))
  #   )
  
  if(label == TRUE){
    plot <- plot+annotate("text", y = var_means$ul+ h, 
                          x = var_means[, var_name], label =group_vector, size = 10, family="serif")
    # x = var_means[[var_name]], label =group_vector, size = 6)
  }
  
  return(plot)
}

cont_var_plot_visits <- function(fit_name, var_name, y_lab, x_lab){
  var_name <- var_name
  model <- fit_name
  model_data <- as.data.frame(model[['data']])%>%select(c(eval(var_name), 'extra_visits'))
  colnames(model_data) <- c('var', 'obs')
  model_data$pred <-predict(model, type= 'response')
  
  
  min_var <- round(min(model_data[,1]))
  max_var <- round(max(model_data[,1]))
  
  list_var <- list(var = seq(min_var,max_var))
  names(list_var)[1] <- c(eval(var_name))
  
  
  model_predict <- emmeans(model, specs = c(eval(var_name)), 
                           at = list_var, 
                           lmer.df = "satterthwaite", type= 'response')%>%
    data.frame()
  model_data <- sample_n(model_data, 1000)
  model_predict <- model_predict%>%merge(model_data, all = TRUE)
  
  plot <- ggplot(model_predict, aes(x =model_predict[,var_name])) +
    geom_point(aes(x = var, y = obs),color = 'gray60') +
    geom_ribbon(aes(ymin = rate - SE, ymax= rate + SE),  linetype = c('dashed'), alpha = 0.2, fill = 'gray20') +
    geom_line(aes(y = rate), size = 1.5)  + 
    theme_classic() +
    ylab(y_lab)+ xlab(x_lab) +
    theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), 
          text = element_text(family = "serif"))
  return(plot)
}

## milk ------
milk_ylab <- 'Daily milk consumption, L'
parity_milk_pairs <- as.data.frame(pairs(emmeans(fit32_milk, 'parity')))
weight_milk_pairs <- as.data.frame(pairs(emmeans(fit32_milk, 'quartile_weight')))

parity_milk_plot <- cat_var_plot(fit32_milk, 'parity', milk_ylab, 'Parity', label= TRUE, c('C', 'B', 'A'), 0.05) +
  scale_y_continuous(limits = c(8.1,9.75),breaks = seq(8.25,9.75, by = 0.25))  +
  scale_x_discrete(labels = c('1', '2', '3+'))  +
  coord_capped_cart(left = 'both')

thi_milk_plot <- cont_var_plot(fit32_milk, 'thi', 
                          milk_ylab, 'Temperature-humidity index')+
  scale_y_continuous(limits = c(6,12),breaks = seq(6,12, by = 0.5)) +
  scale_x_continuous(limits = c(0,80), breaks = seq(0,80,5)) +
  annotate('text', y = 11.5, x =10, label = paste(paste('\U03B2', round(fit32_milk@beta[2], 3), sep='='), 
                                                'P<0.01', sep= ', '),
           size=10, family = 'serif') +
  coord_capped_cart(bottom = 'both', left = 'both')

weight_milk_plot <- cat_var_plot(fit32_milk, 'quartile_weight', milk_ylab, 'Birth weight kg',  label= TRUE, c('D', 'C', 'B', 'A'), 0.05) +
  scale_y_continuous(limits = c(8.25,10),breaks = seq(8.25,10, by = 0.25)) +
  scale_x_discrete(labels = quantiles_weight$range[1:4]) +
  coord_capped_cart(left = 'both')
  

plot_brd_milk <- plot_brd(fit32_milk, 0.02, 0.04, milk_ylab) + 
  scale_y_continuous(limits = c(8.25,10),breaks = seq(8.25,10, by = 0.25))+
  coord_capped_cart(left = 'both')


## speed ----
speed_ylab <- 'Drinking speed, ml/min'
weight_speed_pairs <- as.data.frame(pairs(emmeans(fit32_speed, 'quartile_weight')))
parity_speed_pairs <- as.data.frame(pairs(emmeans(fit32_speed, 'parity')))

parity_speed_plot <- cat_var_plot(fit32_speed, 'parity', speed_ylab, 'Parity', label= TRUE, c('A', 'B', 'C'), 3.5) +
  scale_y_continuous(limits = c(440,580), breaks = seq(440,580, by =20)) +
  scale_x_discrete(labels = c('1', '2', '3+')) +
  coord_capped_cart(left = 'both')


thi_speed_plot <- cont_var_plot(fit32_speed, 'thi', 
                               speed_ylab, 'Temperature-humidity index') +
  scale_y_continuous(limits = c(400,600), breaks = seq(400,600, by =20)) +
  scale_x_continuous(limits = c(0,80), breaks = seq(0,80,5)) +
  annotate('text', y = 580, x =10, label = paste(paste('\U03B2', round(fit32_speed@beta[2], 3), sep='='), 
                                                 'P<0.01', sep= ', '),
           size=10, family = 'serif') +
  coord_capped_cart(left = 'both', bottom  = 'both')

weight_speed_plot <- cat_var_plot(fit32_speed, 'quartile_weight', speed_ylab, 'Birth weight, kg', 
                              label= TRUE, c('C', 'B', 'B', 'A'), 3.5) + 
  scale_y_continuous(limits = c(440,560), breaks = seq(440,560, by =20)) +
  scale_x_discrete(labels = quantiles_weight$range[1:4]) +
  coord_capped_cart(left = 'both')


plot_brd_speed <- plot_brd(model = fit32_speed, sh = 0.5, dh = 1.8, ylabel =speed_ylab) +
  scale_y_continuous(limits = c(440,560), breaks = seq(440,560, by =20)) +
  coord_capped_cart(left = 'both')


## visits ----
visits_ylab <- 'Days with unrewarded visits, count'
pairs(emmeans(fit32_visits, 'parity'))
pairs(emmeans(fit32_visits, 'num_pneu2'))
pairs(emmeans(fit32_visits, 'quartile_weight'))

milk_visits_plot <- cont_var_plot_visits(fit32_visits, 'milk', visits_ylab, 'Average milk consumption, L') +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by =5)) +
  scale_x_continuous(limits = c(4,16), breaks = seq(4,16, by =2)) +
  coord_capped_cart(left = 'both', bottom = 'both')

thi_visits_plot <- cont_var_plot_visits(fit32_visits, 'thi', visits_ylab, 'Temperature-humidity index') +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by =5)) +
  scale_x_continuous(limits = c(23,75), breaks = seq(20,75,5))+
  coord_capped_cart(left = 'both', bottom = 'both')


parity_visits_plot <- cat_var_plot_visit(fit32_visits, 'parity', visits_ylab, 'Parity', FALSE) +
  scale_y_continuous(limits = c(5,10), breaks = seq(5,10, by =1)) +
  scale_x_discrete(labels = c('1', '2', '3+')) +
  coord_capped_cart(left = 'both')

weight_visits_plot <- cat_var_plot_visit(fit32_visits, 'quartile_weight', visits_ylab, 'Birth weight, kg', 
                                         label= TRUE, c('A', 'AB', 'B', 'AB'), 0.2) +
  scale_y_continuous(limits = c(5,10), breaks = seq(5,10, by =1)) +
  scale_x_discrete(labels = quantiles_weight$range[1:4]) +
  coord_capped_cart(left = 'both')

plot_brd_visits <- cat_var_plot_visit(fit32_visits, 'num_pneu2', visits_ylab, 'BRD incidences, count', 
                                      label= TRUE, c('A', 'B', 'AB'), 0.2) +
  scale_x_discrete(labels = c('0', '1', '2+')) +
  scale_y_continuous(limits = c(5,10), breaks = seq(5,10, by =1)) +
  coord_capped_cart(left = 'both')


## save plots------
pdf('plots/parity_milk.pdf', width = 12, height = 8)
  print(parity_milk_plot)
dev.off()
pdf('plots/parity_speed.pdf', width = 12, height = 8)
  print(parity_speed_plot)
dev.off()
pdf('plots/parity_visits.pdf', width = 12, height = 8)
print(parity_visits_plot)
dev.off()


pdf('plots/thi_milk.pdf', width = 12, height = 8)
  print(thi_milk_plot)
dev.off()
pdf('plots/thi_speed.pdf', width = 12, height = 8)
  print(thi_speed_plot)
dev.off()
pdf('plots/thi_visits.pdf', width = 12, height = 8)
print(thi_visits_plot)
dev.off()

pdf('plots/weight_milk.pdf', width = 12, height = 8)
  print(weight_milk_plot)
dev.off()
pdf('plots/weight_speed.pdf', width = 12, height = 8)
  print(weight_speed_plot)
dev.off()
pdf('plots/weight_visits.pdf', width = 12, height = 8)
print(weight_visits_plot)
dev.off()

pdf('plots/brd_speed.pdf', width = 12, height = 8)
  print(plot_brd_speed)
dev.off()
pdf('plots/brd_milk.pdf', width = 12, height = 8)
  print(plot_brd_milk)
dev.off()
pdf('plots/brd_visits.pdf', width = 12, height = 8)
print(plot_brd_visits)
dev.off()

pdf('plots/milk_visits.pdf', width = 12, height = 8)
print(milk_visits_plot)
dev.off()



# save model output -----
## function to write output ---
table <- function(model_df, col_names, file_name, l){
  sink(file_name)
  model_df <- data.frame(variable = row.names(model_df))%>%cbind(model_df)
  model_df2 <- model_df[,c(1,2,3,l)]
  cat(col_names)
  for(i in 1:nrow(model_df2)){
    row <- model_df2[i,]
    for(c in 1:length(row)){
      cell <- str_replace_all(row[[c]], '_', ' ')
      cat(cell)
      if(c < length(row)){
        cat(' & ')
      }else{
        cat('\\')
      }
    }
    cat('\n')
  }
  sink()
}


## milk coefficients ----
coef_names <- 'variable & estimate & SE & P value \\ \n'
coef_milk <- data.frame(coef(summ_milk))%>%round(3)
table(coef_milk, coef_names, 'tables/milk_coef.txt', 6)

## speed coefficients----
coef_speed <- data.frame(coef(summ_speed))%>%round(3)
table(coef_speed, coef_names, 'tables/speed_coef.txt', 6)




## visits coefficients-----
coef_visits <- data.frame(coef(summ_visits))
# coef_visits$ul <- exp(2*coef_visits$Std..Error + coef_visits$Estimate)
# coef_visits$ll <- exp(-2*coef_visits$Std..Error + coef_visits$Estimate)
# coef_visits$Estimate <- exp(coef_visits$Estimate)
# coef_visits$Std..Error <- coef_visits$Std..Error - coef_visits$Estimate
coef_visits <- coef_visits%>%round(3)
table(coef_visits, coef_names, 'tables/visits_coef.txt', 5)


conf_int <- data.frame(coef(summ_visits))
conf_int$Estimate <- exp(conf_int$Estimate)
conf_int <-conf_int%>%cbind(exp(confint(fit32_visits)))
conf_int <- conf_int%>%round(3)
conf_int <- data.frame(var = row.names(conf_int))%>%cbind(conf_int)
write.csv(conf_int, 'tables/conf_int.csv', row.names = FALSE)

## descriptive stats attempt -----
data32_long <- data32D_g%>%
  select(c("weight_kg", "thi", "speed", "milk","extra_visits", "case_no"))%>%
  pivot_longer(cols = c ("speed", "milk","extra_visits"), 
               names_to = "variable", values_to = 'values')

data32_longs <- data32_long%>%
  group_by(variable)%>%
  summarize(n = n(), 
            mean = mean(values),
            med = quantile(values, c(1/2)),
            sd = sd(values), 
            iqr = IQR(values), 
            min = min(values), 
            max = max(values))

data32_long2 <- data32D%>%
  group_by(case_no, weight_kg, section, LACT, BDAT)%>%
  summarise(
    feeder = max(feeder),
    num_pneu = max(inc_pneu))%>%ungroup()

data32_long2 <- data32_long2%>%mutate(year = year(as_date(BDAT)))
data32_long2 <- data32_long2%>%mutate(LACT = ifelse(LACT >= 3, 3, LACT))
data32_long2 <- data32_long2%>%mutate(num_pneu = ifelse(num_pneu>= 2, 2, num_pneu))


data32_long2s <- data32_long2%>%                                
  select(c("LACT", "num_pneu", "feeder", "section", "year"))%>%
  pivot_longer(cols = c("LACT", "num_pneu", "feeder"), 
               names_to = "variable", values_to = 'values')%>%
  group_by(variable, values, year)%>%
  summarize(n = n())

data32_long2s<- data32_long2s%>%pivot_wider(names_from = c(year), values_from = c(n))
  