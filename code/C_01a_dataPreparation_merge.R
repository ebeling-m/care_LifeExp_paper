# Preparation of inputs for matrix estimations
# Load packages and functions
source("code/C_00_packages.R")

# Data to be merged
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)
load("data_inter/data_merge_longCare.RData")
dat.name <- paste("part_", 1:10, "_personYearsMonth.RData", sep = "")
dat_long <- list()
no.lop <- list()

for(i in 1:10){
  load(dat.name[i])
  no.lop[[i]] <- length(unique(dat.sel$lopnr))
  py.agg <- 
    dat.sel %>%
    left_join(dat_merge) %>% 
    group_by(year, month, age, t_p, care, male, exclude_lopnr) %>% 
    summarise(py_sum = sum(py))

    dx.agg <-
    dat.sel %>%
    left_join(dat_merge) %>%
    filter(!Event == 50 & deathMonth == month & deathYear == year) %>%
    mutate(death = 1) %>%
    group_by(year, month, age, t_p, care, male, exclude_lopnr) %>%
    summarise(Dx = sum(death)) %>%
    right_join(py.agg)

    dat_long[[i]] <- dx.agg
}

dat_long <- 
  dat_long %>% 
  bind_rows() %>% 
  mutate(Dx = ifelse(is.na(Dx), 0, Dx)) %>% 
  group_by(year, month, age, t_p, care, male, exclude_lopnr) %>% 
  summarise(Dx = sum(Dx), 
            Nx = sum(py_sum))

# Compare number of unique LopNr
sum(unlist(no.lop))
unique(length(dat_merge$lopnr))

# Compare to HMD Data 
setwd(wd.dir)

deaths <- read_table("Deaths_lexis.txt", skip = 2)
expo <- read_table("Exposures_lexis.txt", skip = 2)

deaths <- 
  deaths %>% 
  mutate(Age = as.numeric(Age), 
         Age = ifelse(is.na(Age), 110, Age), 
         Cohort = as.numeric(Cohort)) %>% 
  filter(Year %in% c(2015:2020) & Age >= 69 & !is.na(Cohort)) %>% 
  mutate(highAge = Year - Cohort) %>% 
  filter(highAge >= 70) %>% 
  select(Year, highAge, Female, Male) %>% 
  pivot_longer(cols = c(Female, Male)) %>% 
  mutate(male = as.numeric(mapvalues(name, from =c("Female", "Male"), to = c(0,1)))) %>% 
  group_by(Year, male, highAge) %>% 
  summarise(Dx_hmd = sum(value)) %>% 
  rename(year = Year, age = highAge)

expo <- 
  expo %>% 
  mutate(Age = as.numeric(Age), 
         Age = ifelse(is.na(Age), 110, Age), 
         Cohort = as.numeric(Cohort)) %>% 
  filter(Year %in% c(2015:2020) & Age >= 69 & !is.na(Cohort)) %>% 
  mutate(highAge = Year - Cohort) %>% 
  filter(highAge >= 70) %>% 
  select(Year, highAge, Female, Male) %>% 
  pivot_longer(cols = c(Female, Male)) %>% 
  mutate(male = as.numeric(mapvalues(name, from =c("Female", "Male"), to = c(0,1)))) %>% 
  group_by(Year, male, highAge) %>% 
  summarise(Nx_hmd = sum(value)) %>% 
  rename(year = Year, age = highAge)

# Data check

datPlot <- 
  dat_long %>%
  group_by(year, male, age) %>% 
  summarise(Dx = sum(Dx), 
            Nx = sum(Nx)) %>% 
  left_join(deaths) %>% 
  left_join(expo)


datPlot <- 
  datPlot %>% 
  mutate(Dx_diff = Dx - Dx_hmd, 
         Nx_diff = Nx - Nx_hmd,
         Nx_diffR = Nx/Nx_hmd,
         Dx_diffR = Dx/Dx_hmd)

test <- 
  datPlot %>%
  filter(male == 0 & year == 2020)
plot(test$age, test$Dx_diffR)  

# Check for anomalies in the death counts by care and age 
datTest <-  
  dat_long %>% 
  filter(age >= 70 & year < 2021) %>% 
  count(year, care, age, male)

test <- datTest %>% filter(male == 1 & care == 1 & age == 99)
length(test$t_p)
table(test$n)

# Check randomly for death counts
datTest <-  
  dat_long %>% 
  filter(age >= 70 & year < 2021)

test <- datTest %>% filter(male == 0 & care == 2 & age == 93 & exclude_lopnr == 1)

plot(test$t_p, test$Dx)
abline(v = 72)
abline(v = 12)
abline(v = 24)
abline(v = 36)
abline(v = 48)
abline(v = 60)

 
# Take out datafile for excess fits
dat_long_txt <- 
  dat_long %>% 
  bind_rows() %>% 
  mutate(Dx = ifelse(is.na(Dx), 0, Dx)) %>% 
  group_by(year, month, age, t_p, care, male, exclude_lopnr) %>% 
  summarise(Dx = sum(Dx), 
            Nx = sum(py_sum)) %>% 
  filter(age >= 70 & year < 2021)

write.table(dat_long_txt, file = "data_inter/aggregated_care_20211003.txt", row.names = FALSE)


