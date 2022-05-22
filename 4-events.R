# Maria Elisa Montes
# May 2022
#
# Part 3

library(dplyr)
library(stringr)

# get data ----
out_dir <- "outfiles"

# events data -----
source <- "dairy_comp/events"
in_dir <-  paste('infiles', source, sep = '/')

file_list <- list.files(in_dir)
fileNames <- paste(in_dir, file_list, sep = "/")

events_df <- read.csv(fileNames[1])

for(i in 2:length(fileNames)){
  temp <- read.csv(fileNames[i])
  events_df <- rbind(events_df, temp)
}

# demographic data -----
demo_file <- "dc_demo_info.csv"
demo_data <-  read.csv(paste(out_dir, demo_file, sep = '/'))

demo_data <- demo_data%>%select('ID', 'BDAT', 'case_no')

# formating and cleaning procedures ----
# clean extra spaces
events_df$Event <- str_trim(events_df$Event)
events_df$Remark <- str_trim(events_df$Remark)
events_df$DIM <- str_trim(events_df$DIM)

# make DIM be a number
events_df$DIM <- as.numeric(events_df$DIM)
# event date as date
events_df$Date <- as.Date(events_df$Date, "%m/%d/%Y")

events_df2 <- events_df%>%
  unique()%>%
  mutate(BDAT = Date - abs(DIM))


events_demo <- merge(events_df2, demo_data, by = c("ID", "BDAT"))

# Make data frames for each event----

# measurement events ----
weight_all <- events_demo%>%filter(Event == "MEASURE")
# make remark be a number
weight_all$Remark <- as.numeric(str_extract(weight_all$Remark, "\\d*"))

weight_daily <- weight_all%>%
  select(c("case_no", "Date", "Remark", "DIM", "BDAT"))

weight_count <- weight_all%>%group_by(case_no)%>%summarise(count = n())

weight_initial <- weight_daily%>%
  filter(DIM <= 1 & Remark !=0)%>%
  group_by(case_no)%>%
  arrange(Date, .by_group = TRUE)%>%
  mutate(weight_no = row_number())%>%
  filter(weight_no == 1)

weight_initial <- select(weight_initial, c("case_no", "Remark"))%>%
  rename(weight = Remark)

weights <- select(weight_daily, c("case_no", "Remark", "DIM"))%>%
  rename(weight = Remark)%>%
  rename(age_weight = DIM)

write.csv(weight_initial, "outfiles/dc_events_bweight.csv", row.names = FALSE, na ="")
write.csv(weights, "outfiles/weight_all.csv", row.names = FALSE, na ="")

# Pneumonia events ----
pneu_all <- events_demo%>%filter(Event == "PNEU")

pneu_daily <- pneu_all%>%
  select(c("case_no", "Date", "Remark", "DIM", "BDAT", "Protocols"))

# give 5 days between events, otherwise considered to be the same
pneu_daily2 <- pneu_daily%>%
  group_by(case_no)%>%
  arrange(Date, .by_group = TRUE)%>%
  mutate(incidence = row_number())%>%
  mutate(daysfrom = Date - lag(Date, 1))%>%
  mutate(new = ifelse(is.na(daysfrom) | daysfrom > 5, 1, 0))%>%
  mutate(inc_pneu = cumsum(new))%>%
  ungroup()

pneu_daily2 <- pneu_daily2%>%
  filter(new == 1)

pneu_daily2_2 <- pneu_daily2%>%
  filter(DIM <= 60)

pneu_60 <- pneu_daily2_2%>%
  group_by(case_no)%>%
  filter(inc_pneu == max(inc_pneu))%>%
  select(c('case_no', 'inc_pneu'))%>%
  unique()

colnames(pneu_60) <- c('case_no', 'pneu60')

pneu_daily2_2 <- pneu_daily2_2%>%
  select(c("case_no", "Date", "inc_pneu"))

write.csv(pneu_daily2_2, "outfiles/dc_events_brd_daily.csv", row.names = FALSE, na ="")
