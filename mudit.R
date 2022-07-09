ft_data$initial_date <- as.Date(ft_data$initial_date)
ft_data2 <- ft_data%>%mutate(day = str_pad(day(initial_date), 2, pad= 0))%>%
                               mutate(md = as.numeric(paste(month(initial_date), day, sep ='')))%>%
                               mutate(b_season = ifelse(md <= 320 | md >=  1221, 'winter', 
                                                      ifelse(md >= 321 & md <= 620, 'spring', 
                                                             ifelse(md >= 621 & md <= 920, 'summer', 
                                                                    ifelse(md >= 921 & md <= 1220, 'fall', NA)))))
ggplot(ft_data2, aes(y= total_milk, x = thi)) +geom_point() + facet_wrap(vars(b_season))
