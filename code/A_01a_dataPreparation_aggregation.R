# Aggreagte Discrete Data
# Load packages and functions
source("code/A_00_packages.R")

# Load datafiles
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)

dat <- 
  paste("discreteData_20210901_", 
             c("P1", "P2", "P3", "P4"), 
             ".txt", 
             sep = "") %>% 
  map_df(~read_delim(., delim = " "))

# Reduce to ages observed 

dat1 <- 
  dat %>% 
  filter(!is.na(AgeGr)) %>% 
  mutate(Start = if_else(is.na(Start), End, Start)) %>%
  group_by(id) %>% 
  mutate(maxAge = max(AgeGr)) %>% 
  ungroup() %>% 
  filter(!AgeGr == maxAge)

# Calculate year variable 
dat1 <- 
  dat1 %>% 
  mutate(YearSt = Coh+AgeGr, 
         YearEnd = Coh+AgeGr+1)

# How many double transitions do we have
doubTrans <- 
  dat1 %>% 
  filter(dTrans == 1) %>% 
  dplyr::select(id, dTrans) %>% 
  distinct()

# datRaw %>% filter(lopnr == 63)
# 6.9% have transitions that happen within a quarter of a year
length(unique(doubTrans$id))/length(unique(dat1$id))

# Aggregate data
aggDat <- 
  dat1 %>%
  group_by(sex, AgeGr, Start, End, Coh, exclude) %>% 
  count() %>% 
  group_by(sex, AgeGr, Start, Coh, exclude) %>% 
  mutate(TotalPop = sum(n))

# Select Age Range 70 to 105 
unique(aggDat$AgeGr)

aggDat1 <- 
  aggDat %>% 
  filter(AgeGr >= 70 & AgeGr < 105) %>% 
  arrange(Coh) %>% 
  mutate(sY = Coh+AgeGr, 
         eY = Coh+AgeGr+1.25) %>%
  mutate(YearGr = case_when(
    eY <= 2017 & sY >= 2015 ~ 15,
    eY <= 2018 & sY >= 2016 ~ 16,
    eY <= 2019 & sY >= 2017 ~ 17,
    eY <= 2020 & sY >= 2018 ~ 18,
    eY <= 2021 & sY >= 2019 ~ 19)) %>% 
  filter(YearGr == 18)

setwd("C:/careLifeExp/care_LifeExp_paper")
getwd()

write.table(aggDat1, file = "data_inter/aggreagted_data_transitions.txt", row.names =FALSE)