# Plot and analyse LE 
# Load packages and functions
source("code/A_00_packages.R")
load("data_inter/leEstimates.RData")
# Load HMD LE

expo <- read_table("data_input/Exposures_lexis.txt", skip = 2)
death <- read_table("data_input/Deaths_lexis.txt", skip = 2)

expo <- 
  expo %>% 
  mutate(Age = as.numeric(Age), 
         Age = ifelse(is.na(Age), 110, Age)) %>% 
  filter(Year %in% c(2015:2020) & Age <= 105) %>% 
  mutate(Cohort = as.numeric(Cohort)) %>% 
  pivot_longer(cols = c(Male, Female), names_to = "sex", 
               values_to = "Nx") %>% 
  mutate(sex = as.numeric(mapvalues(sex, 
                                    from = c("Male", "Female"), 
                                    to = c(1, 0)))) %>% 
  group_by(Cohort, Age, sex) %>% 
  summarise(Nx = sum(Nx)) %>%
  ungroup() %>% 
  mutate(Year = Cohort + Age) %>% 
  arrange(desc(Cohort), desc(Age))
  
hmd <- 
  death %>% 
  mutate(Age = as.numeric(Age), 
         Age = ifelse(is.na(Age), 110, Age)) %>% 
  filter(Year %in% c(2015:2020) & Age <= 105) %>% 
  mutate(Cohort = as.numeric(Cohort)) %>% 
  pivot_longer(cols = c(Male, Female), names_to = "sex", 
              values_to = "Dx") %>% 
  mutate(sex = as.numeric(mapvalues(sex, 
                                    from = c("Male", "Female"), 
                                    to = c(1, 0)))) %>% 
  group_by(Cohort, Age, sex) %>% 
  summarise(Dx = sum(Dx)) %>%
  ungroup() %>% 
  mutate(Year = Cohort + Age) %>% 
  arrange(desc(Cohort), desc(Age)) %>% 
  left_join(expo) %>% 
  filter(2014 < Year & Year < 2020)

# Calculate life expectancy 
le_HMD <- 
  hmd %>% 
  mutate(mx = Dx/Nx) %>% 
  group_by(Year, sex) %>% 
  group_map(~lifetable_HMD(x=.), .keep = TRUE) %>% 
  bind_rows()

# Create plot
remain_LE <- 
  remain_LE %>% 
  mutate(Age = as.integer(AgeGr))

# Create data set with ref values for YLL calculation
le_HMD_ref <- 
  le_HMD %>% 
  dplyr::select(Age, sex, Year, ex) %>% 
  mutate(care = "total") %>% 
  filter(Year == 2018)

le_care_ref <- 
  remain_LE %>% 
  ungroup %>% 
  mutate(Year = mapvalues(YearGr, 
                          from = 15:19, 
                          to = 2015:2019)) %>%
  dplyr::select(care, AgeGr, sex, Year, le_med_r) %>% 
  rename(Age = AgeGr, ex = le_med_r) %>% 
  filter(Year == 2018)

ref_le <- bind_rows(le_HMD_ref, le_care_ref)
save(ref_le, file = "data_output/final_refLE_YLL.RData")  


# # Create data set for LE comparison in total LE
# comp_LE <- 
#   remain_LE %>%
#   ungroup() %>% 
#   filter(AgeGr %in% seq(70, 105, by = 1)) %>% 
#   distinct(AgeGr, sex, YearGr, leT_obs, leT_med, leT_obs_r, leT_med_r) %>% 
#   mutate(Year = mapvalues(YearGr, from = c(15:19), to = 2015:2019)) %>% 
#   rename(Age = AgeGr) %>% 
#   left_join(le_HMD) %>%
#   dplyr::select(Age, sex, Year, leT_obs, leT_med, leT_obs_r, leT_med_r, ex)
 
# # Create plot of difference in LE
# pdf("figures/comparisons_total_LE.pdf", width = 14, pointsize = 16)
# par(mfrow = c(1, 2))
# plot(x=comp_LE$leT_obs, y=comp_LE$ex, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(-1, 1),
#      xlim=c(70,105), ylab = "Difference Total LE from MSM - HMD data (in years)", 
#      xlab = "Age", main = "Females")
# lines(x=c(70,105), y=c(0, 0), lwd = 2, col = "gray")
# 
# comp_LE %>% 
#   filter(sex == 0) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$Age, y=.$leT_obs-.$ex, lwd = 3, col = .$color.y))
# 
# 
# comp_LE %>% 
#   filter(sex == 0) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$Age, y=.$leT_obs_r-.$ex, lwd = 3, col = .$color.y, lty = 2))
# legend("topright", legend = 2015:2019, col = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'),
#        lwd = 3, bty = "n")
# 
# legend("bottomleft", legend = c("MSM LE based on fundamental matrix", 
#                               "MSM LE based on rewards"), col = "black",
#        lwd = 3, lty = c(1, 2), bty = "n")
# 
# 
# # Males
# plot(x=comp_LE$leT_obs, y=comp_LE$ex, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(-1, 1),
#      xlim=c(70,105), ylab = "Difference Total LE from MSM - HMD data (in years)", 
#      xlab = "Age", main = "Males")
# lines(x=c(70,105), y=c(0, 0), lwd = 2, col = "gray")
# 
# comp_LE %>% 
#   filter(sex == 1) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$Age, y=.$leT_obs-.$ex, lwd = 3, col = .$color.y))
# 
# 
# comp_LE %>% 
#   filter(sex == 0) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$Age, y=.$leT_obs_r-.$ex, lwd = 3, col = .$color.y, lty = 2))
# dev.off()
# 
# 
# # Plot for difference between observed and smoothed care-specific LE
# 
# trend_LE <- 
#   remain_LE %>% 
#   ungroup() %>% 
#   mutate(diff_LE_med_r = le_obs_r-le_med_r,
#          color.y = mapvalues(YearGr, 
#                              from = 15:19, 
#                              to = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'))
#   )
# 
# names.care <- c("No care", "Home care", "Care home")
# names(names.care) <- c("NC", "HC", "CH")
# 
# pdf("figures/diff_obs_fit_careLE.pdf", family = "Times", 
#     width = 14, height = 10, pointsize = 15)
# par(mfrow = c(2,3))
# for(i in c("NC", "HC", "CH")){
#   plot(x=trend_LE$leT_obs, y=trend_LE$leT_obs, typ = "n", 
#        xaxs = "i", yaxs = "i", bty = "n", ylim = c(-1, 1),
#        xlim=c(70,105), ylab = "Difference observed-smoothed LE estimates (in years)", 
#        xlab = "Age", main = paste("Women, ", names.care[i], sep = ""))
#   lines(x=c(70,105), y=c(0, 0), lwd = 2, col = "gray")
#   
#   trend_LE %>% 
#     filter(sex == 0 & care == i) %>% 
#     group_by(YearGr) %>% 
#     group_map(~lines(x=.$AgeGr, y=.$diff_LE_med_r, col = .$color.y, lwd = 3))
#   
#   legend("topleft", legend = 2015:2019, col = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'),
#          lwd = 3, bty = "n")
# }
# 
# for(i in c("NC", "HC", "CH")){
#   plot(x=trend_LE$leT_obs, y=trend_LE$leT_obs, typ = "n", 
#        xaxs = "i", yaxs = "i", bty = "n", ylim = c(-1, 1),
#        xlim=c(70,105), ylab = "Difference observed-smoothed LE estimates (in years)", 
#        xlab = "Age", main = paste("Men, ", names.care[i], sep = ""))
#   lines(x=c(70,105), y=c(0, 0), lwd = 2, col = "gray")
#   
#   trend_LE %>% 
#     filter(sex == 1 & care == i) %>% 
#     group_by(YearGr) %>% 
#     group_map(~lines(x=.$AgeGr, y=.$diff_LE_med_r, col = .$color.y, lwd = 3))
#   
#   legend("topleft", legend = 2015:2019, col = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'),
#          lwd = 3, bty = "n")
# }
# dev.off()

# Plot for LE differences overtime 
# Plot at ages 70, 80, 90

# le_trend <- 
#   remain_LE %>% 
#   ungroup() %>% 
#   filter(AgeGr %in% c(70, 80, 90)) %>% 
#   mutate(color.y = mapvalues(YearGr, 
#                              from = 15:19, 
#                              to = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')),
#          y.values = as.numeric(mapvalues(YearGr, 
#                              from = 15:19, 
#                              to = seq(0.1, 0.9, length.out = 5))),
#          pch.age = mapvalues(AgeGr, 
#                              from = c(70, 80, 90),
#                              to = c(16, 15, 17))) %>% 
#   mutate(y.values = case_when(care == "NC" ~ y.values + 0,
#                               care == "HC" ~ y.values + 1,
#                               care == "CH" ~ y.values + 2))
# 
# pdf("figures/le_values.pdf", family = "Times", width = 14, 
#     pointsize = 12)
# par(mar=c(5,3,2,1))
# layout(rbind(c(1,2), c(3,3)), height = c(0.9, 0.1))
# 
# plot(x=trend_LE$leT_obs, y=trend_LE$leT_obs, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
#      xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# segments(x0=seq(0, 20, by = 2), y0=0, x1=seq(0, 20, by = 2), y1=4, 
#          col = "lightgray", lwd = 2.5)
# segments(x0=seq(1, 19, by = 2), y0=0, x1=seq(1, 19, by = 2), y1=4, 
#          col = "lightgray", lwd = 0.5)
# 
# axis(1, at = seq(0, 20, by = 2), lwd = 3, cex.axis = 1.3)
# axis(1, at = seq(1, 19, by = 2), lwd = 1, labels = FALSE)
# 
# axis(2, at = c(0.5, 1.5, 2.5, 3.5), 
#      labels = c("No care", "Home care", "Care home", "Total"), 
#      lwd = 0, cex.axis = 1.3)
# 
# le_trend %>% 
#   filter(sex == 0) %>%
#   dplyr::select(YearGr, AgeGr, care, color.y, y.values,le_med_r, pch.age) %>% 
#   group_by(YearGr, care, AgeGr) %>% 
#   group_map(~lines(x=.$le_med_r,y=.$y.values, col=.$color.y, 
#                    pch = .$pch.age, cex = 1.7, typ = "p"), .keep = TRUE)
# 
# le_trend %>% 
#   filter(sex == 0 & AgeGr == 70) %>%
#   dplyr::select(YearGr, care, color.y, y.values,le_med_r) %>% 
#   group_by(YearGr, care) %>% 
#   group_map(~lines(x=c(0,.$le_med_r), y=rep(.$y.values,2), col=.$color.y, 
#                    lwd = 1, typ = "l"), .keep = TRUE)
# 
# le_HMD %>% 
#   filter(sex == 0 & Age %in% c(70, 80, 90)) %>% 
#   mutate(pch.age = mapvalues(Age, 
#                              from = c(70, 80, 90),
#                              to = c(16, 15, 17)),
#         color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')),
#          y.values = as.numeric(mapvalues(Year, 
#                                          from = 2015:2019, 
#                                          to = seq(0.1, 0.9, length.out = 5)+3))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$ex,y=.$y.values, col=.$color.y, 
#                    pch = .$pch.age, cex = 1.7, typ = "p"), .keep = TRUE)
# 
# 
# le_HMD %>% 
#   filter(sex == 0 & Age == 70) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')),
#          y.values = as.numeric(mapvalues(Year, 
#                                          from = 2015:2019, 
#                                          to = seq(0.1, 0.9, length.out = 5)+3))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=c(0,.$ex), y=rep(.$y.values,2), col=.$color.y, 
#                    lwd = 1, typ = "l"), .keep = TRUE)
# 
# mtext("Women", 3, line = 0.3, cex = 1.3)
# mtext("Remaining life expectancy", 1, line = 2.5, cex = 1.2)
# 
# ####################################################
# # Males
# plot(x=trend_LE$leT_obs, y=trend_LE$leT_obs, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
#      xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# segments(x0=seq(0, 20, by = 2), y0=0, x1=seq(0, 20, by = 2), y1=4, 
#          col = "lightgray", lwd = 2.5)
# segments(x0=seq(1, 19, by = 2), y0=0, x1=seq(1, 19, by = 2), y1=4, 
#          col = "lightgray", lwd = 0.5)
# 
# axis(1, at = seq(0, 20, by = 2), lwd = 3, cex.axis = 1.3)
# axis(1, at = seq(1, 19, by = 2), lwd = 1, labels = FALSE)
# 
# # axis(2, at = c(0.5, 1.5, 2.5, 3.5), 
#      # labels = c("No care", "Home care", "Care home", "Total"), 
#      # lwd = 0)
# 
# le_trend %>% 
#   filter(sex == 1) %>%
#   dplyr::select(YearGr, AgeGr, care, color.y, y.values,le_med_r, pch.age) %>% 
#   group_by(YearGr, care, AgeGr) %>% 
#   group_map(~lines(x=.$le_med_r,y=.$y.values, col=.$color.y, 
#                    pch = .$pch.age, cex = 1.7, typ = "p"), .keep = TRUE)
# 
# le_trend %>% 
#   filter(sex == 1 & AgeGr == 70) %>%
#   dplyr::select(YearGr, care, color.y, y.values,le_med_r) %>% 
#   group_by(YearGr, care) %>% 
#   group_map(~lines(x=c(0,.$le_med_r), y=rep(.$y.values,2), col=.$color.y, 
#                    lwd = 1, typ = "l"), .keep = TRUE)
# 
# le_HMD %>% 
#   filter(sex == 1 & Age %in% c(70, 80, 90)) %>% 
#   mutate(pch.age = mapvalues(Age, 
#                              from = c(70, 80, 90),
#                              to = c(16, 15, 17)),
#          color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')),
#          y.values = as.numeric(mapvalues(Year, 
#                                          from = 2015:2019, 
#                                          to = seq(0.1, 0.9, length.out = 5)+3))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=.$ex,y=.$y.values, col=.$color.y, 
#                    pch = .$pch.age, cex = 1.7, typ = "p"), .keep = TRUE)
# 
# 
# le_HMD %>% 
#   filter(sex == 1 & Age == 70) %>% 
#   mutate(color.y = mapvalues(Year, 
#                              from = 2015:2019, 
#                              to = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')),
#          y.values = as.numeric(mapvalues(Year, 
#                                          from = 2015:2019, 
#                                          to = seq(0.1, 0.9, length.out = 5)+3))) %>%
#   group_by(Year) %>% 
#   group_map(~lines(x=c(0,.$ex), y=rep(.$y.values,2), col=.$color.y, 
#                    lwd = 1, typ = "l"), .keep = TRUE)
# 
# mtext("Men", 3, line = 0.3, cex = 1.3)
# mtext("Remaining life expectancy", 1, line = 2.5, cex = 1.2)
# 
# # Legend
# par(mar = c(0,0,0,0))
# plot(x=trend_LE$leT_obs, y=trend_LE$leT_obs, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
#      xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# legend("top", legend = paste("Year ", 2015:2019), 
#        col = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494', rep ("black", 3)),
#        lwd = rep(2, 5), horiz = TRUE, bty = "n", cex = 1.2)
# legend("bottom", legend = paste("Age ", c(70, 80, 90)), 
#        col = rep ("black", 3), pch =c(16, 15, 17), bty = "n", horiz = TRUE, cex = 1.2)
# 
# dev.off()
