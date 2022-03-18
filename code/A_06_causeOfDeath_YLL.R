# Prepare death data
# Load packages and functions
source("code/A_00_packages.R")

# Load datafiles
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)

var_types <- c("n", "n", "n", "n", "c", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "c")

dat <- read_delim("oneYearPD_data_20210914.txt", delim = " ", 
                  col_types = as.list(var_types))

setwd("C:/careLifeExp/care_LifeExp")

# Reduce to variables needed
dat <- 
  dat %>% 
  dplyr::select(LopNr, CoD, care0, DeathYear, Age, sex)

# Code Ischeamic Heart disease (I20:I25), Stroke(I61:I64), 
# Cancer ("C", "D0", "D1", "D2", "D3", "D4"), Covid ("U"), Infections (A,B), Resp (J), 
# External ("V", "W", "X", "Y")

codCode <- cbind(c("U", "A", "B", "C18", "C19", "C20", "C21", "I20", "I21", "I22", "I23",
                   "I24", "I25", "I60", "I61", "I62", "I63", "I64", "J", "V", "W", "X", "Y"),
                 c(1, 2, 2, rep(3, 4), rep(4, 6), rep(5, 5), 6, rep(7, 4)))

dat <- 
  dat %>% 
  mutate(chapter = case_when(substr(CoD, 1, 1) == "C" ~ substr(CoD, 1, 3),
                             substr(CoD, 1, 1) == "I" ~ substr(CoD, 1, 3), 
                             !substr(CoD, 1, 1) %in% c("D", "I") ~ substr(CoD, 1, 1)),
         CodGr = as.numeric(mapvalues(chapter, from = codCode[,1], to = codCode[,2])),
         CodGr = ifelse(is.na(CodGr), 8, CodGr),
         Sex=ifelse(sex == 2, 0, 1),
         Care = mapvalues(care0, from = 1:3, to = c("NC", "HC", "CH")),
         AgeGr = ifelse(as.integer(Age) >= 100, 100,as.integer(Age))) %>% 
  filter(Age >= 70)

# 3678 don't have a care status
# missCare <- dat %>% filter(is.na(Care))
# dat <- 
#   dat %>% 
#   mutate(Care = ifelse(is.na(Care), "NC", Care))
# 
# update care status 

setwd(dat.dir)
miss_care <- read_delim("missinglopnr_status.csv", delim = ",")
setwd("C:/careLifeExp/care_LifeExp")

miss_care <- 
  miss_care %>% 
  mutate(Care = mapvalues(status, 
                          from = c("nocare", "homecare", "carehome"), 
                          to = c("NC", "HC", "CH"))) %>% 
  dplyr::select(lopnr, Care) %>% 
  rename(LopNr = lopnr, Care_n = Care)

dat <- 
  dat %>%
  left_join(miss_care) %>% 
  mutate(Care = ifelse(!is.na(Care_n), Care_n, Care))

# Aggregate counts
dat_agg <- 
  dat %>%
  count(DeathYear, Sex, Care, AgeGr, CodGr) %>% 
  rename(Dx = n, Year = DeathYear, Age = AgeGr, care = Care, sex = Sex)

# Link ref life expectancy
load("data_output/final_refLE_YLL.RData")
ref_le <- 
  ref_le %>% 
  dplyr::select(Age, sex, ex, care)

cod_yll <- 
  dat_agg %>% 
  left_join(ref_le) %>%
  mutate(yll = Dx*ex) %>% 
  group_by(Year, sex, care, CodGr) %>% 
  summarise(sum_yll = sum(yll), 
            sum_Dx = sum(Dx)) %>% 
  filter(Year > 2018)

# Calculate YLL using total life expectancy
le_total <- 
  ref_le %>% 
  filter(care == "total") %>% 
  dplyr::select(Age, sex, ex)

cod_yll_total <- 
  dat_agg %>%
  group_by(Year, sex, Age, CodGr) %>% 
  summarise(Dx = sum(Dx)) %>%
  left_join(le_total) %>%
  mutate(yll = Dx*ex) %>% 
  group_by(Year, sex, CodGr) %>% 
  summarise(sum_yll = sum(yll), 
            sum_Dx = sum(Dx)) %>% 
  filter(Year > 2018)

save(cod_yll, file = "data_output/yll_cause_of_death.RData")
save(cod_yll_total, file = "data_output/yll_cause_of_death_totalLE.RData")


