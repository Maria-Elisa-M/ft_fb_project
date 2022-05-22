# Maria Elisa Montes
# January 2022
#
# Part 2. Add demographic
# 
# Foerster technik + DC demographic cleaning process
# Get birthdates, breed,sex, and dam related info
#
# date range 2015- 2019
#
# files in: ft_new_summary.csv, demographic/*
# files out: demo_master2.csv, demo_info2.csv

# Settings ----
source <- 'dairy_comp/demographic'
section <- 'historic'

in_dir <-  paste(paste('infiles', source, sep = '/'), section, sep ='/')
out_dir <- 'outfiles'

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)


'%nin%' <- Negate('%in%')
# Reading data-----
ft_file <- str_replace('foerster_technik_sec_summary.csv', 'sec', section)
all_summary <- read.csv(paste(out_dir, ft_file, sep = '/' ))
file_list <- list.files(in_dir)
fileNames <- paste(in_dir, file_list, sep = '/')

dc_demo_df <- read.csv(fileNames[1], colClasses=c(rep("character", 33)))
dc_demo_df <- dc_demo_df[1:nrow(dc_demo_df)-1,1:33]

demo_cols <- c("ID", "REG", "EID", "BDAT", "GNDR", "CBRD", "PEN", "SREG", "DREG", "DID", "MGSID", "RPRO", "TODAY")

for(i in 2:length(fileNames)){
  temp <- read.csv(fileNames[i], colClasses=c(rep("character", 33)))
  temp <- temp[1:nrow(temp)-1,1:33]
  colnames(temp)[5] <- "GNDR"
  dc_demo_df <- rbind(dc_demo_df, temp)
}

rm(fileNames, temp, file_list) # clean

# DC data preparation -----

# Data types and formats
dc_demo_data <-select(dc_demo_df, all_of(demo_cols)) 
dc_demo_data$BDAT <- as.Date(dc_demo_data$BDAT, "%m/%d/%Y")
dc_demo_data$TODAY <- as.Date(dc_demo_data$TODAY, "%m/%d/%Y")

dc_demo_data$REG <- str_trim(dc_demo_data$REG)
dc_demo_data$RPRO <- str_trim(dc_demo_data$RPRO)
dc_demo_data$GNDR <- str_trim(dc_demo_data$GNDR)
dc_demo_data$CBRD <- str_trim(dc_demo_data$CBRD)
dc_demo_data$SREG <- str_trim(dc_demo_data$SREG)
dc_demo_data$DREG <- str_trim(dc_demo_data$DREG)
dc_demo_data$PEN <- as.numeric(dc_demo_data$PEN)

all_summary$initial_date <- as.Date(all_summary$initial_date)

# filter all record for cows born between  2015 and 2019
dc_demo_data <- dc_demo_data%>%filter(BDAT >= as.Date("2015-01-01") &
                                        BDAT <= as.Date("2019-03-01"))

# FT DC merging -----
# Last digits in EID after 00 are the same as transmitter number
dc_demo_data <- dc_demo_data%>%mutate(transmitter_number = as.numeric(str_extract(EID,'(?<=0{2})\\d*' )))

# merge first set of calves
merged1 <- merge(dc_demo_data, all_summary, 
                 by.x = c('transmitter_number', 'ID'), by.y =  c('transmitter_number', 'farm_animal_id'))

merged_count <- merged1%>%
  group_by(case_no)%>%
  mutate(datediff = as.numeric(initial_date - BDAT))%>%
  mutate(max_datediff= max(datediff))%>%
  mutate(min_datediff = min(datediff))%>%
  ungroup()

small_diff <- merged_count%>%
  filter(datediff < 10 & datediff >=-1)%>%
  group_by(ID)%>%
  arrange(desc(datediff), .by_group = TRUE)

small_diff2 <- small_diff%>%
  group_by(case_no)%>%
  filter(datediff == min(datediff))%>%
  mutate(max_datediff= max(datediff))%>%
  mutate(min_datediff = min(datediff))

# Select a breed and gender
small_diff2 <- small_diff2%>%
  group_by(case_no)%>% 
  mutate(count = n())%>%
  mutate(f_count = sum(GNDR == "F"))%>%
  mutate(m_count = sum(GNDR == "M"))%>%
  mutate(h_count = sum(CBRD == "H"))%>%
  mutate(a_count = sum(CBRD == "A"))%>%
  mutate(gender = ifelse(any(RPRO == "BULLCAF"), "M", 
                         ifelse(any(RPRO == "BRED"|
                                      RPRO == "PREG" |
                                      RPRO == "FRESH"|
                                      RPRO == "OPEN"), "F", 
                                ifelse(f_count > m_count, "F","M"))))%>%
  mutate(breed = ifelse(any(RPRO == "BRED"|
                              RPRO == "PREG" |
                              RPRO == "FRESH"|
                              RPRO == "OPEN"), "H", 
                        ifelse(a_count > h_count, "A", "H")))

small_diff2 <- small_diff2%>%
  group_by(case_no)%>%
  mutate(DREG = DREG[DREG != "-"][1L])

# counts 
length(unique(small_diff2$ID))
length(unique(small_diff2$case_no))
length(unique(small_diff2$transmitter_number))


columns_demo <- c("ID", "REG","EID", "BDAT", "case_no",  "gender", "breed", "DREG", "DID")

# select the most recent report
demo_info <- small_diff2%>%
  group_by(case_no)%>%
  arrange(TODAY)%>%
  mutate(report_no = row_number())%>%
  filter(report_no == 1)%>%
  select(all_of(columns_demo))

colnames(demo_info)[2] <- "reg_calf" 
colnames(demo_info)[3] <- "eid_calf"


# dam data ---

# Dam lactation number----

# select REG, FDAT, LACT from DC
dc_demo_df$FDAT <- as.Date(dc_demo_df$FDAT, "%m/%d/%Y")
dc_demo_df$PCDAT <- as.Date(dc_demo_df$PCDAT, "%m/%d/%Y")
dc_demo_df$REG <- str_trim(dc_demo_df$REG)

dams_data <- dc_demo_df%>%
  select(c("ID", "USDA", "EID", "REG", "FDAT", "LACT", "PCDAT", "CSEX"))%>%
  unique()%>%
  filter(LACT >=1)%>%
  mutate(DCC = FDAT- PCDAT)

colnames(dams_data)[1] <- 'ID_dam'
calf_data <- demo_info%>% select(c("ID", "case_no", "BDAT", "DREG","DID"))

dam_calf <- merge(dams_data, calf_data, by.x = c("REG", "FDAT"), by.y = c("DREG", "BDAT"))

# check for repeated records
dam_calf <- dam_calf%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC >0)%>%
  filter(count_no == min(count_no))

dam_calf$count_no <- NULL
dam_calf$DID <- NULL

# Using DID
orphans <- subset(calf_data, case_no %nin% dam_calf$case_no)
dam_calf2 <- merge(dams_data, orphans, by.x = c("ID_dam", "FDAT"), by.y = c("DID", "BDAT"))
dam_calf2 <- dam_calf2%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))

dam_calf2$DREG <- NULL
dam_calf2$count_no <- NULL

dam_calf_all <- rbind(dam_calf, dam_calf2)


# Now using DREG - EID
orphans <- subset(calf_data, case_no %nin% dam_calf_all$case_no)
dam_calf3 <- merge(dams_data, orphans, by.x = c("USDA", "FDAT"), by.y = c("DREG", "BDAT"))
dam_calf3 <- dam_calf3%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))

dam_calf3$DID <- NULL
dam_calf3$count_no <- NULL

dam_calf_all <- rbind(dam_calf_all, dam_calf3)
orphans2 <- subset(orphans, case_no %nin% dam_calf_all$case_no)
rm(list = c("orphans", "dam_calf3"))

## for calves missing a match give a chance of +-1 day
orphans2 <-orphans2%>%mutate(bdat1 = BDAT + 1)%>%mutate(bdat2 = BDAT - 1)
## With DREG - REG
dam_calf4 <- merge(dams_data, orphans2, c("REG", "FDAT"), by.y = c("DREG", "bdat1"))
dam_calf4 <- dam_calf4%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))
dam_calf4$DID <- NULL
dam_calf4$count_no <- NULL
dam_calf4$bdat2 <- NULL
dam_calf4$bdat1 <- NULL
dam_calf4$BDAT <- NULL

dam_calf_all <- rbind(dam_calf_all, dam_calf4)
orphans3 <- subset(orphans2, case_no %nin% dam_calf_all$case_no)
rm(list = c("orphans2", "dam_calf4"))

dam_calf42 <- merge(dams_data, orphans3, c("REG", "FDAT"), by.y = c("DREG", "bdat2"))
dam_calf42 <- dam_calf42%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))
dam_calf42$DID <- NULL
dam_calf42$count_no <- NULL
dam_calf42$bdat2 <- NULL
dam_calf42$bdat1 <- NULL
dam_calf42$BDAT <- NULL

dam_calf_all <- rbind(dam_calf_all, dam_calf42)
orphans4 <- subset(orphans3, case_no %nin% dam_calf_all$case_no)
rm(list = c("orphans3", "dam_calf42"))

dam_calf5 <- merge(dams_data, orphans4, c("ID_dam", "FDAT"), by.y = c("DID", "bdat2"))
dam_calf5 <- dam_calf5%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))
dam_calf5$DREG <- NULL
dam_calf5$count_no <- NULL
dam_calf5$bdat2 <- NULL
dam_calf5$bdat1 <- NULL
dam_calf5$BDAT <- NULL

dam_calf_all <- rbind(dam_calf_all, dam_calf5)
orphans5 <- subset(orphans4, case_no %nin% dam_calf_all$case_no)
rm(list = c("orphans4", "dam_calf5"))

dam_calf52 <- merge(dams_data, orphans5, c("ID_dam", "FDAT"), by.y = c("DID", "bdat1"))
dam_calf52 <- dam_calf52%>% group_by(case_no)%>%
  mutate(count_no = row_number())%>%
  filter(DCC > 0)%>%
  filter(count_no == min(count_no))
dam_calf52$DREG <- NULL
dam_calf52$count_no <- NULL
dam_calf52$bdat2 <- NULL
dam_calf52$bdat1 <- NULL
dam_calf52$BDAT <- NULL

dam_calf_all <- rbind(dam_calf_all, dam_calf52)
orphans6 <- subset(orphans5, case_no %nin% dam_calf_all$case_no) # last orphan list
rm(list = c("orphans5", "dam_calf52"))

dam_calf_all$DCC <- as.numeric(dam_calf_all$DCC, units = 'days')


# Merge all demo info ----
dam_calf_s <- dam_calf_all%>%select(c("LACT","DCC","case_no"))
demo_master <- merge(demo_info, dam_calf_s, by = "case_no", all.x = TRUE)

file_out <- paste(out_dir, paste('dc_demo', section, sep= '_'), sep = '/')
write.csv(demo_master,paste(file_out, 'master.csv', sep  ='_'), row.names = FALSE, na = "")
write.csv(demo_info, paste(file_out, 'info.csv', sep  ='_'), row.names = FALSE, na = "")
