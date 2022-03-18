library(tidyverse)
library(ungroup)
load("data_inter/deaths_2015_to_2020.RData")


# ungrouping C19 confirmed deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c19 <- 
  death_dat %>% 
  as_tibble() %>% 
  filter(CodGr == 1,
         Year == 2020)

c19 %>% 
  group_by(Sex) %>% 
  summarise(sum(Dx))

# visual inspection
c19 %>% 
  ggplot()+
  geom_point(aes(AgeGr, Dx))+
  facet_grid(Care ~ Sex)+
  theme_bw()

c19 %>% 
  filter(AgeGr >= 70) %>% 
  ggplot()+
  geom_point(aes(AgeGr, Dx))+
  facet_grid(Care ~ Sex)+
  theme_bw()

# Aggregating deaths in integer years of age over 70
c19_int <- 
  c19 %>% 
  mutate(AgeGr = floor(AgeGr)) %>% 
  group_by(AgeGr, Care, Sex) %>% 
  summarise(Dx = sum(Dx)) %>% 
  ungroup() %>% 
  rename(age = AgeGr,
         care = Care,
         sex = Sex,
         dts = Dx) %>% 
  mutate(sex = recode(sex,
                      "0" = "f",
                      "1" = "m"))

# building "all" care category
c19_69less <- 
  c19_int %>% 
  filter(age < 70) %>% 
  mutate(care = "all")

c19_int2 <- 
  c19_int %>% 
  filter(age >= 70) %>% 
  group_by(age, sex) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(care = "all") %>% 
  bind_rows(c19_int %>% 
              filter(age >= 70),
            c19_69less) %>% 
  arrange(care, sex, age) %>% 
  complete(care, sex, age = 70:100, fill = list(dts = 0))
  

# visual inspection
c19_int2 %>% 
  ggplot()+
  geom_point(aes(age, dts))+
  facet_grid(care ~ sex)+
  theme_bw()


# adding 1 death and multiplying times 100 to avoid errors wit PCLM
# this will be adjusted again after ungrouping
c19_int_ung <- 
  c19_int2 %>% 
  mutate(dts = 100 * (dts + 1)) %>% 
  arrange(care, sex, age)

# ungrouping C19 in single-year of age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_ungr <- NULL
for(s in unique(c19_int_ung$sex)){
  for(c in unique(c19_int_ung$care)){
    
    chunk <- 
      c19_int_ung %>% 
      filter(care == c,
             sex == s)
    
    a <- chunk %>% dplyr::pull(age)
    dts <- chunk %>% dplyr::pull(dts) 
    
    # fitting pclm without exposures
    res <- pclm(x = a, y = dts, nlast = 6)
    
    db_ungr <- 
      db_ungr %>% 
      bind_rows(tibble(care = c,
                       sex = s,
                       age = min(a):105, 
                       dts = res$fitted))
    
  }
}

db_ungr2 <- 
  db_ungr %>% 
  mutate(dts = dts/100 - 1, # adjusting again 
         dts = ifelse(dts > 0, dts, 0), # not allowing negative values
         age = ifelse(age >= 100, 100, age)) %>% # aggregating after age 100
  group_by(care, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

# re-scaling <70 to totals and adding original 70+ confirmed deaths
tot_care_sex <-
  c19_int2 %>% 
  filter(age < 70) %>% 
  group_by(care, sex) %>% 
  summarise(tot_care_sex = sum(dts))

c19_all <- 
  db_ungr2 %>% 
  filter(age < 70) %>% 
  left_join(tot_care_sex) %>% 
  group_by(care, sex) %>% 
  mutate(dts = tot_care_sex * dts/sum(dts)) %>% 
  select(-tot_care_sex) %>% 
  bind_rows(c19_int2 %>% 
              filter(age >= 70)) %>% 
  arrange(care, sex, age) %>% 
  mutate(source = "C19") 

# visual inspection
c19_all %>% 
  ggplot()+
  geom_point(aes(age, dts))+
  facet_grid(care ~ sex)+
  theme_bw()

c19_all %>% 
  group_by(care, sex) %>% 
  summarise(sum(dts))
# write_rds(c19_all, "output/ungrouped_covid_deaths_0_100_care_status.rds")


# ungrouping excess mortality estimates for ages 70+ =====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# monthly excess in 5-year age groups (ages 70+)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn5_mth <-
  read_rds(paste0("data_inter/baselines_age5_100plus_exc_0_NAknots.rds"))

# monthly excess in single-year age groups (ages 70+)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn1_mth <-
  read_rds(paste0("data_inter/baselines_age1_100plus_exc_0_NAknots.rds"))

  
# STMF monthly excess in 5-year age groups (all ages)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_stmf <- 
  read_rds("data_inter/baseline_stmf.rds")



# excess estimation and merging data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

exc1_mth <- 
  bsn1_mth %>% 
  filter(year == 2020,
         month >= 3,
         care != "all") %>% 
  rename(age = age_gr) %>% 
  group_by(year, care, sex, age) %>% 
  summarise(Nx = sum(Nx),
            Dx = sum(Dx), 
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(excess = Dx - bsn,
         # negative excess to 0
         excess = ifelse(excess >= 0, excess, 0), 
         care = recode(care,
                       "care home" = "CH",
                       "home care" = "HC",
                       "no care" = "NC")) %>% 
  select(care, sex, age, excess) %>% 
  rename(dts = excess) 

unique(exc1_mth$care)
unique(exc1_mth$age)


# 5-year
exc5_mth <- 
  bsn5_mth %>% 
  filter(year == 2020,
         month >= 3,
         care != "all") %>% 
  rename(age = age_gr) %>% 
  group_by(year, care, sex, age) %>% 
  summarise(Nx = sum(Nx),
            Dx = sum(Dx), 
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(excess = Dx - bsn,
         excess = ifelse(excess >= 0, excess, 0),
         care = recode(care,
                       "care home" = "CH",
                       "home care" = "HC",
                       "no care" = "NC")) %>% 
  select(care, sex, age, excess) %>% 
  rename(dts = excess) 

exc_stmf <- 
  bsn_stmf %>% 
  filter(year == 2020,
         month >= 3) %>% 
  group_by(sex, age) %>% 
  summarise(dts = sum(Dx),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(excess = dts - bsn,
         excess = ifelse(excess >= 0, excess, 0),
         care = "all") %>% 
  select(care, sex, age, excess) %>% 
  filter(age < 70) %>% 
  rename(dts = excess)

# 5-year series to ungroup
# ~~~~~~~~~~~~~~~~~~~~~~~
exc_all_5 <- 
  # adding "all" care category  
  exc5_mth %>% 
  group_by(sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(care = "all") %>% 
  bind_rows(exc5_mth, 
            exc_stmf) %>% 
  arrange(care, sex, age) %>% 
  mutate(source = "excess 5y")


# 1- and 5-year series to ungroup
# ~~~~~~~~~~~~~~~~~~~~~~~
exc_all_1 <- 
  # adding "all" care category
  exc1_mth %>% 
  group_by(sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(care = "all") %>% 
  bind_rows(exc1_mth, 
            exc_stmf) %>% 
  arrange(care, sex, age) %>% 
  mutate(source = "excess 1y")

exc_all <- 
  bind_rows(exc_all_1,
            exc_all_5) %>% 
  mutate(dts = 100 * (dts + 1))

unique(exc_all$care)

# ungrouping excess in single-year of age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_ungr_exc <- NULL
for(sr in unique(exc_all$source)){
  for(sx in unique(exc_all$sex)){
    for(cr in unique(exc_all$care)){
      
      chunk <- 
        exc_all %>% 
        filter(source == sr,
               care == cr,
               sex == sx)
      
      a <- chunk %>% dplyr::pull(age)
      dts <- chunk %>% dplyr::pull(dts) 
      
      # fitting pclm without exposures
      res <- pclm(x = a, y = dts, nlast = 6)
      
      db_ungr_exc <- 
        db_ungr_exc %>% 
        bind_rows(tibble(source = sr,
                         care = cr,
                         sex = sx,
                         age = min(a):105, 
                         dts = res$fitted))
      
    }
  }
}

all_ungr <- 
  db_ungr_exc %>% 
  mutate(dts = dts/100 - 1, # adjusting death counts 
         dts = ifelse(dts > 0, dts, 0),
         age = ifelse(age > 100, 100, age)) %>%
  group_by(source, care, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() 


# regrouping deaths by care status in ages above 70 for the "all" category 
exc_ungr_69 <- 
  all_ungr %>% 
  filter(age < 70)

exc_ungr_70 <- 
  all_ungr %>% 
  filter(age >= 70,
         care != "all")

exc_ungr_all <- 
  bind_rows(exc_ungr_69,
            exc_ungr_70) %>% 
  arrange(source, care, sex, age)
  

# re-scaling to totals
tots <- 
  exc_all %>% 
  filter(age < 70 | care != "all") %>% 
  mutate(dts = dts/100 - 1) %>% 
  group_by(source, care, sex) %>% 
  summarise(tot = sum(dts))

exc_ungr_all2 <- 
  exc_ungr_all %>% 
  left_join(tots) %>% 
  group_by(source, care, sex) %>% 
  mutate(dts = tot*(dts/sum(dts))) %>% 
  select(-tot)

# regrouping deaths by care status in ages above 70 for the "all" category 
exc_ungr_all3 <- 
  exc_ungr_all2 %>% 
  filter(age >= 70) %>% 
  group_by(source, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(care = "all") %>% 
  bind_rows(exc_ungr_all2) %>% 
  arrange(source, care, sex, age)

unique(exc_ungr_all3$source)
table(exc_ungr_all3$care)

exc_ungr_all3 %>% 
  ggplot()+
  geom_point(aes(age, dts))+
  geom_line(aes(age, dts))+
  facet_grid(care~sex~source)+
  theme_bw()

exc_ungr_all3 %>% 
  filter(care == "NC") %>% 
  ggplot()+
  geom_point(aes(age, dts))+
  geom_line(aes(age, dts))+
  facet_grid(sex~source)+
  theme_bw()

write_rds(exc_ungr_all3, "data_inter/ungrouped_excess_deaths_0_100_care_status.rds")

dts_all <- 
  bind_rows(exc_ungr_all3,
            c19_all)

dts_all %>% 
  ggplot()+
  geom_line(aes(age, dts, col = source))+
  geom_point(aes(age, dts, col = source), 
             size = 0.5,
             alpha = 0.6)+
  facet_grid(care~sex)+
  theme_bw()

write_rds(dts_all, "data_inter/ungrouped_all_c19_excess_0_100_care_status.rds")

test <- 
  dts_all %>% 
  spread(care, dts) %>% 
  mutate(test = CH + HC + NC,
         diff = all - test)

dts_all %>% 
  bind_rows(dts_all %>% 
              filter(age < 70) %>% 
              mutate(care = "NC")) %>% 
  mutate(status = ifelse(care == "all", "unadjusted", "adjusted")) %>% 
  # filter(age>=70) %>% 
  group_by(source, status, sex) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  spread(status, dts)

