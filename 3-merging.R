# Maria Elisa Montes
# May 2022
#
# Part 3

library(dplyr)

# get data ----

in_dir <- "outfiles"
out_dir <- "outfiles"
sections<- c('calfcloud', 'historic')

# demographic files -----

source <- 'dc_demo'
# info
for(i in 1:length(sections)){
  temp <- read.csv(paste(in_dir, paste(source, sections[i], 'info.csv', sep = '_'), sep = '/'))
  if(i == 1){
    demo_info_data <- temp
  }else{
    demo_info_data <- rbind(demo_info_data, temp)
  }
}
rm(temp)
write.csv(demo_info_data, paste(out_dir, paste(source, 'info.csv', sep = '_'), sep = '/'), row.names = FALSE)

# master
for(i in 1:length(sections)){
  temp <- read.csv(paste(in_dir, paste(source, sections[i], 'master.csv', sep = '_'), sep = '/'))
  if(i == 1){
    demo_master_data <- temp
  }else{
    demo_master_data <- rbind(demo_master_data, temp)
  }
}
rm(temp)
write.csv(demo_master_data, paste(out_dir, paste(source, 'master.csv', sep = '_'), sep = '/'), row.names = FALSE)

# foerster technik files -----
source <- 'foerster_technik'
# summary 
for(i in 1:length(sections)){
  temp <- read.csv(paste(in_dir, paste(source, sections[i], 'summary.csv', sep = '_'), sep = '/'))
  if(i == 1){
    sum_data <- temp
  }else{
    sum_data <- rbind(sum_data, temp)
  }
}
rm(temp)
write.csv(sum_data, paste(out_dir, paste(source, 'summary.csv', sep = '_'), sep = '/'), row.names = FALSE)

# daily
for(i in 1:length(sections)){
  temp <- read.csv(paste(in_dir, paste(source, sections[i], 'daily.csv', sep = '_'), sep = '/'))
  if(i == 1){
    daily_data <- temp
  }else{
    daily_data <- rbind(daily_data, temp)
  }
}
rm(temp)
write.csv(daily_data, paste(out_dir, paste(source, 'daily.csv', sep = '_'), sep = '/'), row.names = FALSE)










