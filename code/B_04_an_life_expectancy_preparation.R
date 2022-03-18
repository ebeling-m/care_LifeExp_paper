source("code/B_00_functions.r")

ages_care <- 
  read_rds("data_inter/ungrouped_all_c19_excess_0_100_care_status.rds") %>% 
  select(care, sex, age) %>% 
  unique() %>% 
  arrange(care, sex, age)
  
unique(ages_care$care)

# data loading
# ============

# life expectancy
# ~~~~~~~~~~~~~~~~~

# loading life expectancy values by care status
# load("data/life_exp_ref_values.RData")
load("data_output/final_refLE_YLL.RData")

# some values repeated in ages > 100y already corrected
yll_ref_values2 <- 
  ref_le %>% 
  group_by(care, Age, sex) %>% 
  mutate(n = n()) %>% 
  arrange(care, Age, sex) %>% 
  unique()


# extracting the ax values
# loading life tables for all population combined from the HMD
lt_m <- 
  read_table("data/mltper_1x1.txt", skip = 1) %>% 
  select(year = Year, age = Age, ex, ax) %>% 
  mutate(sex = "m")

lt_f <- 
  read_table("data/fltper_1x1.txt", skip = 1) %>% 
  select(year = Year, age = Age, ex, ax) %>% 
  mutate(sex = "f")


# life expectancy preparation
# ============================

# taking the 2018-2019 ex average by age/sex for all care combined
exs_all_care <-
  bind_rows(lt_m,
            lt_f) %>%
  mutate(age = ifelse(age == "110+", 110, as.double(age))) %>%
  filter(year %in% c(2018, 2019)) %>%
  group_by(age, sex) %>%
  summarise(ex = mean(ex),
            ax = mean(ax)) %>%
  ungroup() %>%
  mutate(care = "all",
         age_ex = age) %>%
  select(-age)

exs_by_care <- 
  ref_le %>% 
  select(care, sex, ex, age_ex = Age) %>% 
  # filter(age_ex >= 70) %>% 
  mutate(sex = recode(sex,
                      "0" = "f",
                      "1" = "m"),
         care = recode(care,
                       "total" = "all")) %>% 
  unique() 


# decimal age interpolation of life expectancy 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interex <- function(db)
{
  
  amax <- max(db$age_ex)
  amin <- min(db$age_ex)
  xs <- db %>% drop_na() %>% pull(age_ex)
  ys <- db %>% drop_na() %>% pull(ex)
  # smoothing using cubic splines
  ts <- seq(amin, amax, 0.01)
  
  db2 <- 
    tibble(age_ex = ts,
           ex = spline(xs, ys, xout = ts)$y)
  return(db2)
  
}

exs_interp <- 
  exs_by_care %>% 
  arrange(care, sex, age_ex) %>% 
  group_by(sex, care) %>% 
  do(interex(db = .data)) %>% 
  ungroup() %>% 
  mutate(age_ex = round(age_ex / 0.01) * 0.01) %>% 
  arrange(care, sex, age_ex)


# assigning ex
axs <- 
  exs_all_care %>% 
  select(age = age_ex, sex, ax)

# ax for the closing age, 100+ (life expectancy at age 100)
exs_100 <- 
  exs_interp %>% 
  filter(age_ex == 100) %>% 
  rename(e100 = ex,
         age = age_ex) %>% 
  select(care, sex, age, e100)


out_exs <- 
  ages_care %>% 
  # matching each age with age + ax
  left_join(axs) %>% 
  # using e100 as a100
  left_join(exs_100) %>% 
  mutate(age_ex = ifelse(age == 100, age + e100, age + ax)) %>% 
  select(-e100, -ax) %>% 
  # rounding age ex to two decimals to be able to merge with ex values
  mutate(age_ex = round(age_ex / 0.01) * 0.01) %>% 
  left_join(exs_interp) %>% 
  select(care, sex, age, ex_yll = ex)

write_rds(out_exs, "data_inter/life_expectancy_adjusted_for_yll_0_100_care_status.rds")

