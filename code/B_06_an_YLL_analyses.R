source("code/B_00_functions.r")
# library(tidyverse)

yll <- read_rds("data_output/yll_estimates_sex_age_care.rds")


# figure 1 ====
# ~~~~~~~~~~~~~
tx <- 8
cols <- c("all" = "black",
          "under_70" = "grey",
          "NC" = "#1b9e77",
          "HC" = "#d95f02",
          "CH" = "#7570b3")

yll %>% 
  filter(source != "excess 1y",
         status == "adjusted") %>% 
  ggplot()+
  geom_bar(aes(age, dts, fill = care), stat = "identity")+
  facet_grid(sex ~ source)+
  scale_fill_manual(values = cols)+
  theme_bw()+
  theme(
    axis.text = element_text(size = tx),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = tx),
    legend.text = element_text(size = tx),
    legend.title = element_text(size = tx),
    strip.text.y = element_text(margin = margin(b = 0, t = 0)),
    strip.text.x = element_text(margin = margin(b = 0, t = 0)),
    legend.key.size = unit(3,"mm"),
    legend.position = "bottom"
  )

ggsave("figures/01_deaths_age_source.png",
       dpi = 2000,
       width = 6,
       height = 4)

# comparing adjusted and unadjusted
yll %>% 
  filter(source != "excess 1y") %>% 
  ggplot()+
  geom_bar(aes(age, dts, fill = care), stat = "identity")+
  facet_grid(source ~ sex ~ status)+
  theme_bw()+
  theme(
    axis.text = element_text(size = tx),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = tx),
    legend.text = element_text(size = tx),
    legend.title = element_text(size = tx),
    strip.text.y = element_text(margin = margin(b = 0, t = 0)),
    strip.text.x = element_text(margin = margin(b = 0, t = 0)),
    legend.key.size = unit(3,"mm"),
    legend.position = "bottom"
  )



# =====




# table 3 ====
# ~~~~~~~~~~~~

# deaths
# ~~~~~~
summ_dts <- 
  yll %>% 
  group_by(source, status, sex) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  spread(status, dts)

summ_dts
copy_to_excel(summ_dts)

# total YLL
# ~~~~~~~~~
sum <- 
  yll %>% 
  group_by(source, status, sex) %>% 
  summarise(yll = sum(yll)) %>% 
  ungroup() %>% 
  spread(status, yll)
sum
copy_to_excel(sum)

# average YLL
# ~~~~~~~~~~~
av_yll <- 
  yll %>% 
  group_by(source, status, sex) %>% 
  summarise(av_yll = sum(ex_yll * dts)/sum(dts)) %>% 
  ungroup() %>% 
  spread(status, av_yll)

av_yll
copy_to_excel(av_yll)

# ====



# figure 3 ====
# ~~~~~~~~~~~~~

cols2 <- c("#b7094c","#0091ad")

yll %>% 
  filter(source != "excess 1y") %>% 
  select(status, source, sex, ex_yll, dts) %>% 
  uncount(dts) %>% 
  ggplot(aes(ex_yll, 
             sex, 
             fill = sex,
             col = sex)) +
  geom_violin(alpha = 0.7,
              trim = FALSE)+
  scale_fill_manual(values = cols2)+
  scale_color_manual(values = cols2)+
  scale_x_continuous(limits = c(0, 20))+
  # geom_boxplot(width=0.1)+
  stat_summary(fun = median, geom = "point", col = "black", 
               size = 3)+
  stat_summary(fun = mean, geom = "point", col = "black", 
               shape = 1, size = 3)+
  facet_grid(source~status)+ 
  theme_bw()+
  theme(strip.background = element_rect(fill = "transparent"))

ggsave("figures/03_violins.png",
       w = 8,
       h = 5,
       dpi = 600)

# including dots for each person
yll %>% 
  filter(source != "excess 1y") %>% 
  select(status, source, sex, ex_yll, dts) %>% 
  mutate(dts = round(dts)) %>% 
  uncount(dts) %>% 
  ggplot(aes(ex_yll, sex)) +
  geom_jitter(height = 0.3, 
              width = 0, 
              alpha = 0.05,
              size = 0.1)+
  geom_violin(aes(fill = sex), 
              alpha = 0.4,
              trim = FALSE)+
  facet_grid(status~source)+ 
  scale_x_continuous(limits = c(0, 20))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(strip.background = element_rect(fill = "transparent"))

# ggsave("figures/violins_dots.pdf",
#        w = 15,
#        h = 15)
# 
# ggsave("figures/violins_dots.png",
#        w = 8,
#        h = 8,
#        dpi = 600)

# ====


# figure 4 ====
# ~~~~~~~~~~~~~
tx <- 8
yll %>% 
  filter(source != "excess 5y") %>% 
  ggplot()+
  geom_bar(aes(age, yll, fill = care), stat = "identity")+
  facet_grid(source ~ sex ~ status)+
  scale_fill_manual(values = cols)+
  theme_bw()+
  theme(
    axis.text = element_text(size = tx),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = tx),
    legend.text = element_text(size = tx),
    legend.title = element_text(size = tx),
    strip.text.y = element_text(margin = margin(b = 0, t = 0)),
    strip.text.x = element_text(margin = margin(b = 0, t = 0)),
    legend.key.size = unit(3,"mm"),
    legend.position = "bottom"
  )


ggsave("figures/04_yll_age_source.png",
       dpi = 2000,
       width = 8,
       height = 4)




