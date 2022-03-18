source("code/B_00_functions.r")

dts_all <- read_rds("data_inter/ungrouped_all_c19_excess_0_100_care_status.rds")
age_exs <- read_rds("data_inter/life_expectancy_adjusted_for_yll_0_100_care_status.rds")

unique(dts_all$source)
unique(dts_all$care)

# dts_exs <- 
#   dts_all %>% 
#   left_join(age_exs) %>% 
#   mutate(dts = round(dts))

table(dts_all$source)

dts_all_exs <- 
  dts_all %>% 
  arrange(source, care, sex, age) %>% 
  select(source, care, sex, age, dts) %>% 
  left_join(age_exs) %>% 
  mutate(dts = round(dts))

# adding NC under 70 (with ex of "all") to include them in both the adjusted
# and the unadjusted estimations
dts_all_exs2 <- 
  dts_all_exs %>% 
  bind_rows(dts_all_exs %>% 
              filter(age < 70) %>% 
              mutate(care = "NC"))

yll <- 
  dts_all_exs2 %>% 
  mutate(yll = dts *ex_yll,
         status = ifelse(care == "all", "unadjusted", "adjusted"),
         status = factor(status, levels = c("unadjusted", "adjusted"))) %>% 
  ungroup()


write_rds(yll, "data_output/yll_estimates_sex_age_care.rds")

