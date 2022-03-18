source("code/B_00_functions.R")

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 15 March 2021
download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", 
              "data_input/STMFinput.zip")

# list of country codes in STMF
zipdf <- unzip("data_input/STMFinput.zip", list = TRUE)

stmf <- 
  read_csv(unz("data_input/STMFinput.zip", "SWEstmf.csv")) %>% 
  select(year = Year,
         week = Week,
         sex = Sex,
         age = Age, 
         dts = Deaths)

# distribute unknown weeks 
tot_year <- 
  stmf %>% 
  group_by(year, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  rename(tot_year = dts) %>% 
  filter(!age %in% c("UNK"))

stmf2 <- 
  stmf %>% 
  filter(!is.na(week),
         !age %in% c("UNK")) %>% 
  left_join(tot_year) %>% 
  group_by(year, age, sex) %>% 
  mutate(dts = tot_year * (dts/sum(dts))) %>% 
  ungroup() %>% 
  select(-tot_year) %>% 
  replace_na(list(dts = 0))

# distribute unknown age
tot_age <- 
  stmf2 %>% 
  filter(age == "TOT") %>% 
  select(-age) %>% 
  rename(tot_age = dts)

stmf3 <- 
  stmf2 %>% 
  filter(age != "TOT") %>% 
  left_join(tot_age) %>% 
  group_by(year, week, sex) %>% 
  mutate(dts = tot_age * (dts/sum(dts))) %>% 
  ungroup() %>% 
  select(-tot_age) %>% 
  replace_na(list(dts = 0))

# distribute unknown sex
tot_sex <- 
  stmf3 %>% 
  filter(sex == "b") %>% 
  select(-sex) %>% 
  rename(tot_sex = dts)

stmf4 <- 
  stmf3 %>% 
  filter(sex != "b") %>% 
  left_join(tot_sex) %>% 
  group_by(year, week, age) %>% 
  mutate(dts = tot_sex * (dts/sum(dts))) %>% 
  ungroup() %>% 
  select(-tot_sex) %>% 
  replace_na(list(dts = 0)) %>% 
  filter(year >= 2015)

# ====



# from weekly to monthly mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmf5 <- 
  stmf4 %>% 
  # filter(year == 2015, week <= 2) %>% 
  mutate(age = age %>% as.double(),
         isoweek_ini = paste0(year, "-W", sprintf("%02d", week), "-1"),
         isoweek_end = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date_ini = ISOweek2date(isoweek_ini),
         date_end = ISOweek2date(isoweek_end),
         month_ini = month(date_ini),
         month_end = month(date_end),
         share = ifelse(month_ini == month_end, 1, day(date_end)/7),
         # deaths during previous month
         dts_ini_month = dts * (1 - share),
         # deaths during current month
         dts_end_month = dts * share) %>% 
  gather(dts_ini_month, dts_end_month, key = month_dts, value = dts_m) %>% 
  mutate(month = case_when(month_dts == "dts_ini_month" ~ month_ini,
                           month_dts == "dts_end_month" ~ month_end),
         # identify calendar year
         year_c = ifelse(month_ini == 12 & 
                           month_end == 1 & 
                           month_dts == "dts_ini_month",
                         year(date_ini),
                         year(date_end))) %>% 
  group_by(year_c, month, sex, age) %>% 
  summarise(dts = sum(dts_m)) %>% 
  ungroup() %>% 
  rename(year = year_c)
# ====




# loading exposures ====
# ~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_delim("data_input/EndMonthPopulation_Sweden_singleAges.csv", 
             delim = ";",
             skip = 2)

pop2 <- 
  pop %>% 
  select(-region) %>% 
  gather(-age, -sex, key = t, value = pop) %>% 
  separate(t, c("year", "month"), sep = "M") %>% 
  mutate(age = str_replace(age, " years| year", ""),
         age = ifelse(age == "100+", 100, as.double(age)),
         age = age - age %% 5,
         age = ifelse(age > 90, 90, age),
         month = month %>% as.double(),
         year = year %>% as.double(),
         sex = recode(sex,
                      "men" = "m",
                      "women" = "f")) %>% 
  group_by(age, sex, year, month) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()




# mortality baseline ====
# ~~~~~~~~~~~~~~~~~~~~~~~

to_fit <- 
  stmf5 %>% 
  arrange(sex, age, year, month) %>% 
  group_by(sex, age) %>% 
  mutate(t_p = 1:n(),
         w = ifelse(!(year == 2020 & month >= 3), 1, 0)) %>% 
  ungroup() %>% 
  left_join(pop2) %>% 
  filter(year >= 2015) %>% 
  mutate(Nx = pop / 12,
         date = make_date(d = 15, m = month, y = year)) %>% 
  rename(Dx = dts) %>% 
  drop_na()

bsn <- 
  to_fit %>% 
  group_by(sex, age) %>% 
  do(est_baseline(db = .data)) %>% 
  ungroup()


write_rds(bsn, "data_inter/baseline_stmf.rds")
bsn <- read_rds("data_inter/baseline_stmf.rds")

# ====
bsn %>%
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), fill = "#0077b6", alpha = 0.3)+
  geom_line(aes(date, ll_r), col = "#0077b6", size = 0.02, alpha = 0.5)+
  geom_line(aes(date, ul_r), col = "#0077b6", size = 0.02, alpha = 0.5)+
  geom_line(aes(date, dts_r), col = "grey10", size = 0.05, alpha = 0.7)+
  geom_line(aes(date, bsn_r), col = "#0077b6", size = 0.2)+
  facet_wrap(age ~ sex, ncol = 2, scales = "free")+
  # labs(title = plot_title, y = "death rates", x = "date")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 5),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.5, t = 0.5),
                              size = 7),
    legend.position = "bottom", 
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
# saving the plot in PNG (bitmap) and PDF (vectorized) formats
ggsave("figures/baseline_stmf.pdf",
         width = 7, 
         height = 50,
       limitsize = FALSE)
  

excess <- 
  bsn %>% 
  filter(year == 2020,
         month >= 3) %>% 
  group_by(sex, age) %>% 
  summarise(dts = sum(Dx),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(excess = dts - bsn)



# example fitting
# age 90, males, for each care status

bsn %>%
  filter(age == 90,
         sex == "m") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), fill = "#0077b6", alpha = 0.3)+
  geom_line(aes(date, ll_r), col = "#0077b6", size = 0.02, alpha = 0.5)+
  geom_line(aes(date, ul_r), col = "#0077b6", size = 0.02, alpha = 0.5)+
  geom_line(aes(date, dts_r), col = "grey10", size = 0.05, alpha = 0.7)+
  geom_line(aes(date, bsn_r), col = "#0077b6", size = 0.2)+
  facet_wrap(age ~ sex, ncol = 2, scales = "free")+
  # labs(title = plot_title, y = "death rates", x = "date")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 5),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.5, t = 0.5),
                              size = 7),
    legend.position = "bottom", 
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
# saving the plot in PNG (bitmap) and PDF (vectorized) formats
ggsave("figures/baseline_stmf.pdf",
       width = 7, 
       height = 50,
       limitsize = FALSE)

