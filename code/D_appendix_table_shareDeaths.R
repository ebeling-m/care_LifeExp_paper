# Appendix table share of deaths
# Care adjusted estimates 
source("code/A_00_packages.R")

yll <- read_rds("data_output/yll_estimates_sex_age_care.rds")
load("data_output/yll_cause_of_death.RData")
load("data_output/yll_cause_of_death_totalLE.RData")

# Total number of excess deaths
excess_total <- 
  yll %>% 
  filter(status == "adjusted" & age >= 70) %>% 
  group_by(source, sex) %>% 
  summarise(excess = sum(dts)) %>% 
  filter(source %in% c("excess 5y")) %>% 
  mutate(Year = 2020, 
         sex = ifelse(sex == "f", 0, 1)) %>% 
  ungroup() %>% 
  dplyr::select(Year, sex, excess)

# Get total death counts
cod_p <- 
  cod_yll %>% 
  group_by(Year, sex) %>% 
  mutate(tot_Dx = sum(sum_Dx)) %>% 
  group_by(Year, sex, CodGr, tot_Dx) %>%
  summarise(sum_Dx = sum(sum_Dx)) %>% 
  ungroup() %>% 
  left_join(excess_total) %>% 
  mutate(ex_Dx = tot_Dx-excess,
         p_Dx = round(sum_Dx/tot_Dx*100,2), 
         p_Dx_ex = round(sum_Dx/(tot_Dx-excess)*100,2)) %>% 
  arrange(Year, sex, CodGr)


# Women
cod19_w <- cod_p %>% filter(sex == 0 & Year == 2019)
cod19_w$p_Dx

cod20_w <- cod_p %>% filter(sex == 0 & Year == 2020)
cod20_w$p_Dx
cod20_w$p_Dx_ex

# Men
cod19_m <- cod_p %>% filter(sex == 1 & Year == 2019)
cod19_m$p_Dx

cod20_m <- cod_p %>% filter(sex == 1 & Year == 2020)
cod20_m$p_Dx
cod20_m$p_Dx_ex


# Care-specific estimates 
# Total number of excess deaths
excess_care <- 
  yll %>% 
  filter(status == "adjusted" & age >= 70) %>% 
  group_by(source, sex, care) %>% 
  summarise(excess = sum(dts)) %>% 
  filter(source %in% c("excess 5y")) %>% 
  mutate(Year = 2020, 
         sex = ifelse(sex == "f", 0, 1)) %>% 
  ungroup() %>% 
  dplyr::select(Year, sex, care, excess)

# Get total death counts
cod_care_p <- 
  cod_yll %>% 
  group_by(Year, sex, care) %>% 
  mutate(tot_Dx = sum(sum_Dx)) %>% 
  group_by(Year, sex, CodGr, care, tot_Dx) %>%
  summarise(sum_Dx = sum(sum_Dx)) %>% 
  ungroup() %>% 
  left_join(excess_care) %>% 
  mutate(ex_Dx = tot_Dx-excess,
         p_Dx = round(sum_Dx/tot_Dx*100,2), 
         p_Dx_ex = round(sum_Dx/(tot_Dx-excess)*100,2)) %>% 
  arrange(Year, sex, care, CodGr)

# Women

care19_w_NC <- cod_care_p %>% filter(sex == 0 & Year == 2019 & care == "NC")
care19_w_HC <- cod_care_p %>% filter(sex == 0 & Year == 2019 & care == "HC")
care19_w_CH <- cod_care_p %>% filter(sex == 0 & Year == 2019 & care == "CH")

care20_w_NC <- cod_care_p %>% filter(sex == 0 & Year == 2020 & care == "NC")
care20_w_HC <- cod_care_p %>% filter(sex == 0 & Year == 2020 & care == "HC")
care20_w_CH <- cod_care_p %>% filter(sex == 0 & Year == 2020 & care == "CH")

library(xtable)
xtable(cbind(
  c(care19_w_NC$tot_Dx[1], NA, care19_w_NC$p_Dx),
  c(care20_w_NC$tot_Dx[1], care20_w_NC$p_Dx),
  c(care20_w_NC$ex_Dx[1], care20_w_NC$p_Dx_ex),
  c(care19_w_HC$tot_Dx[1], NA, care19_w_HC$p_Dx),
  c(care20_w_HC$tot_Dx[1], care20_w_HC$p_Dx),
  c(care20_w_HC$ex_Dx[1], care20_w_HC$p_Dx_ex), 
  c(care19_w_CH$tot_Dx[1], NA, care19_w_CH$p_Dx),
  c(care20_w_CH$tot_Dx[1], care20_w_CH$p_Dx),
  c(care20_w_CH$ex_Dx[1], care20_w_CH$p_Dx_ex) 
))

  

# Men

care19_m_NC <- cod_care_p %>% filter(sex == 1 & Year == 2019 & care == "NC")
care19_m_HC <- cod_care_p %>% filter(sex == 1 & Year == 2019 & care == "HC")
care19_m_CH <- cod_care_p %>% filter(sex == 1 & Year == 2019 & care == "CH")

care20_m_NC <- cod_care_p %>% filter(sex == 1 & Year == 2020 & care == "NC")
care20_m_HC <- cod_care_p %>% filter(sex == 1 & Year == 2020 & care == "HC")
care20_m_CH <- cod_care_p %>% filter(sex == 1 & Year == 2020 & care == "CH")

xtable(cbind(
  c(care19_m_NC$tot_Dx[1], NA, care19_m_NC$p_Dx),
  c(care20_m_NC$tot_Dx[1], care20_m_NC$p_Dx),
  c(care20_m_NC$ex_Dx[1], care20_m_NC$p_Dx_ex),
  c(care19_m_HC$tot_Dx[1], NA, care19_m_HC$p_Dx),
  c(care20_m_HC$tot_Dx[1], care20_m_HC$p_Dx),
  c(care20_m_HC$ex_Dx[1], care20_m_HC$p_Dx_ex), 
  c(care19_m_CH$tot_Dx[1], NA, care19_m_CH$p_Dx),
  c(care20_m_CH$tot_Dx[1], care20_m_CH$p_Dx),
  c(care20_m_CH$ex_Dx[1], care20_m_CH$p_Dx_ex) 
))







