# sourcing functions
# ~~~~~~~~~~~~~~~~~~
source("code/B_00_functions.R")


# loading and adjusting data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
db <- 
  read_delim("data_input/aggregated_care_20211003.txt", delim = " ")

db2 <- 
  db %>%
  mutate(date = make_date(d = 15, m = month, y = year),
         sex = recode(male,
                      "1" = "m",
                      "0" = "f"),
         care = recode(care,
                       "1" = "no care",
                       "2" = "home care",
                       "3" = "care home"),
         care = factor(care, levels = c("no care", "home care", "care home"))) %>% 
  select(care, sex, age, year, month, date, t_p, Dx, Nx)

db3 <- 
  db2 %>% 
  group_by(year, month, age, date, t_p, sex) %>% 
  summarise(Dx = sum(Dx),
            Nx = sum(Nx)) %>% 
  ungroup() %>% 
  mutate(care = "all") %>% 
  bind_rows(db2)

unique(db3$care)

# fitting age- and sex-specific baselines accounting for care status
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1-year age groups, closing at 100+
db_bsn_age1_100plus <- 
  give_me_baseline(db3,
                   age_gr = 1,
                   age_cl = 100,
                   last_date = "2020-02-15",
                   mths_exc = c(0),
                   knots = NA)

# # 2-year age groups, closing at 100+
# db_bsn_age2_100plus <- 
#   give_me_baseline(db3,
#                    age_gr = 2,
#                    age_cl = 100,
#                    last_date = "2020-02-15",
#                    mths_exc = c(0),
#                    knots = NA)

# 5-year age groups, closing at 100+
db_bsn_age5_100plus <- 
  give_me_baseline(db3,
                   age_gr = 5,
                   age_cl = 100,
                   last_date = "2020-02-15",
                   mths_exc = c(0),
                   knots = NA)




# example baseline fitting
bsn5_mth <-
  read_rds(paste0("data_inter/baselines_age5_100plus_exc_0_NAknots.rds"))

bsn5_mth %>% 
  filter(age_gr == 90,
         sex == "m") %>% 
  mutate(care = factor(care, 
                       levels = c("all", "no care", "home care", "care home"))) %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.2, fill = "#118ab2")+
  geom_line(aes(date, ll_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
  geom_line(aes(date, ul_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
  geom_line(aes(date, dts_r), size = 0.2, alpha = 0.2)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.8)+
  geom_point(aes(date, dts_r, col = excess), size = 0.3, alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-02-15"), linetype = "dashed", 
             col = "#06d6a0",
             alpha = 0.5,
             size = 0.3)+
  scale_color_manual(values = c("#073b4c", "#ef476f"))+
  # facet_wrap( ~ care, scales = "free", ncol = 6)+
  facet_wrap( ~ care, ncol = 4, scales = "free")+
  labs(y = "death rates")+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 7),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7),
    strip.background = element_blank()
  )
ggsave("figures/example_baseline_care_status_free_scale.png",
       width = 6, 
       height = 2.5,
       dpi = 1000)



# # fitting age- and sex-specific baselines ignoring care status
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # 1-year age groups, closing at 100+
# db_bsn_age1_100plus <- 
#   give_me_baseline_no_care(age_gr = 1,
#                    age_cl = 100,
#                    last_date = "2020-02-15",
#                    mths_exc = c(0),
#                    knots = NA)
# 
# # 2-year age groups, closing at 100+
# db_bsn_age2_100plus <- 
#   give_me_baseline_no_care(age_gr = 2,
#                    age_cl = 100,
#                    last_date = "2020-02-15",
#                    mths_exc = c(0),
#                    knots = NA)
# 
# # 5-year age groups, closing at 100+
# db_bsn_age5_100plus <- 
#   give_me_baseline_no_care(age_gr = 5,
#                    age_cl = 100,
#                    last_date = "2020-02-15",
#                    mths_exc = c(0),
#                    knots = NA)

