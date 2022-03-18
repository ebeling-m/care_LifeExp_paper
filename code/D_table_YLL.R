# Main table 1
# Table for life years Lost 
source("code/A_00_packages.R")


yll <- read_rds("data_output/yll_estimates_sex_age_care.rds")
 
# Calculate unadjusted yll
yll_sum_all <-   
  yll %>%
  filter(status == "unadjusted") %>% 
  group_by(source, sex, care) %>% 
  summarise(yll_sum = sum(yll), 
            dts_sum = sum(dts)) %>% 
  mutate(average_yll = yll_sum/dts_sum)

# Calculate adjusted
yll_sum_adj <-   
  yll %>%
  mutate(care = ifelse(age < 70, "total", care)) %>% 
  filter(status == "adjusted") %>% 
  group_by(source, sex) %>% 
  summarise(yll_sum = sum(yll), 
            dts_sum = sum(dts)) %>% 
  mutate(average_yll = yll_sum/dts_sum)

care_yll_c19 <- yll_sum_adj %>% filter(source == "C19")
round(care_yll_c19$yll_sum, 1)
round(care_yll_c19$average_yll, 2)

care_yll_ex <- yll_sum_adj %>% filter(source == "excess 5y")
round(care_yll_ex$yll_sum, 1)
round(care_yll_ex$average_yll, 2)

yll_sum_care <-   
  yll %>%
  mutate(care = ifelse(age < 70, "total", care)) %>% 
  filter(status == "adjusted") %>% 
  group_by(source, sex, care) %>% 
  summarise(yll_sum = sum(yll), 
            dts_sum = sum(dts)) %>% 
  mutate(average_yll = yll_sum/dts_sum)



# pdf("figures/yll_total.pdf", family = "Times", width = 14, 
#     pointsize = 12)
# 
# # layout(rbind(cbind(1,2), c(3,3)), height = c(0.9, 0.1))
# plot(x=0:101, y= 0:101, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 60000),
#      xlim=c(0,2), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# 
# segments(x0=0, y0=seq(0, 60000, by = 10000), 
#          x1=2, y1=seq(0, 60000, by = 10000), col = "lightgray", 
#          lwd = 2)
# 
# segments(x0=0, y0=seq(5000, 55000, by = 10000), 
#          x1=2, y1=seq(5000, 55000, by = 10000), col = "lightgray", 
#          lwd = 1)
# 
# segments(x0=1, 0, 
#          x1=1, y1=60000, col = "lightgray", 
#          lwd = 2)
# 
# par(las = 1)
# axis(2, at = seq(0, 60000, by = 10000), labels = TRUE, lwd = 3, cex.axis = 1.3)
# axis(2, at = seq(5000, 55000, by = 10000), labels = FALSE, lwd = 1)
# 
# axis(1, at = c(0.25, 0.75), labels = 
#        c("Covid-19\nCause-of-death", "Excess\nMortality"), lwd = 0, 
#      cex.axis = 1.3, pos = -600)
# 
# axis(1, at = c(1.25, 1.75), labels = 
#        c("Covid-19\nCause-of-death", "Excess\nMortality"), lwd = 0, 
#      cex.axis = 1.3, pos = -600)
# 
# mtext("Women", 3, adj = 0.01, cex = 1.4, line = -1.5)
# mtext("Men", 3, adj = 0.55, cex = 1.4, line = -1.5)
# mtext("Total years of life lost", 3, cex = 1.6, line = 1)
# 
# # Plot total unadjusted
# fem_all_c19 <- 
#   yll_sum_all %>% 
#   filter(source == "C19" & sex == "f")
# 
# polygon(x=c(0.05,0.05, 0.05+0.175, 0.05+0.175), 
#           y = c(0, rep(fem_all_c19$yll_sum,2), 0), 
#           col = "darkgray", 
#           border = "white")
# 
# text(x = 0.05+0.125/2, y =fem_all_c19$yll_sum+750, "Unadjusted", 
#      cex = 1.4, srt = 90, pos = 4)
# 
# fem_all_ex <- 
#   yll_sum_all %>% 
#   filter(source == "excess 5y" & sex == "f")
# 
# polygon(x=c(0.05,0.05, 0.05+0.175, 0.05+0.175)+0.5, 
#         y = c(0, rep(fem_all_ex$yll_sum,2), 0), 
#         col = "darkgray", 
#         border = "white")
# 
# male_all_c19 <- 
#   yll_sum_all %>% 
#   filter(source == "C19" & sex == "m")
# 
# polygon(x=c(0.05,0.05, 0.05+0.175, 0.05+0.175)+1, 
#         y = c(0, rep(male_all_c19$yll_sum,2), 0), 
#         col = "darkgray", 
#         border = "white")
# 
# male_all_ex <- 
#   yll_sum_all %>% 
#   filter(source == "excess 5y" & sex == "m")
# 
# polygon(x=c(0.05,0.05, 0.05+0.175, 0.05+0.175)+0.5+1, 
#         y = c(0, rep(male_all_ex$yll_sum,2), 0), 
#         col = "darkgray", 
#         border = "white")
# 
# # Plot total adjusted
# fem_adj_c19 <- 
#   yll_sum_adj %>% 
#   filter(source == "C19" & sex == "f")
# 
# col_bar <- c("darkgray", '#1b9e77','#d95f02','#7570b3')
# xx_p <- cumsum(rev(fem_adj_c19$yll_sum))
# 
# text(x = 0.275+0.125/2, y = xx_p[4]+750, "Adjusted", 
#      cex = 1.4, srt = 90, pos = 4)
# 
# for(i in 1:4){
#   if(i == 1){
#   polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175), 
#           y = c(0, rep(xx_p[i],2), 0), 
#           col = col_bar[i], 
#           border = "white")
#   }else{
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175), 
#             y = c(xx_p[i-1], rep(xx_p[i],2), xx_p[i-1]), 
#             col = col_bar[i], 
#             border = "white")
#   }
# }
# 
# fem_adj_ex <- 
#   yll_sum_adj %>% 
#   filter(source == "excess 5y" & sex == "f")
# 
# col_bar <- c("darkgray", '#1b9e77','#d95f02','#7570b3')
# xx_p <- cumsum(rev(fem_adj_ex$yll_sum))
# for(i in 1:4){
#   if(i == 1){
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+0.5, 
#             y = c(0, rep(xx_p[i],2), 0), 
#             col = col_bar[i], 
#             border = "white")
#   }else{
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+0.5, 
#             y = c(xx_p[i-1], rep(xx_p[i],2), xx_p[i-1]), 
#             col = col_bar[i], 
#             border = "white")
#   }
# }
# 
# #################
# # Male
# 
# male_adj_c19 <- 
#   yll_sum_adj %>% 
#   filter(source == "C19" & sex == "m")
# 
# col_bar <- c("darkgray", '#1b9e77','#d95f02','#7570b3')
# xx_p <- cumsum(rev(male_adj_c19$yll_sum))
# for(i in 1:4){
#   if(i == 1){
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+1, 
#             y = c(0, rep(xx_p[i],2), 0), 
#             col = col_bar[i], 
#             border = "white")
#   }else{
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+1, 
#             y = c(xx_p[i-1], rep(xx_p[i],2), xx_p[i-1]), 
#             col = col_bar[i], 
#             border = "white")
#   }
# }
# 
# male_adj_ex <- 
#   yll_sum_adj %>% 
#   filter(source == "excess 5y" & sex == "m")
# 
# col_bar <- c("darkgray", '#1b9e77','#d95f02','#7570b3')
# xx_p <- cumsum(rev(male_adj_ex$yll_sum))
# for(i in 1:4){
#   if(i == 1){
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+0.5+1, 
#             y = c(0, rep(xx_p[i],2), 0), 
#             col = col_bar[i], 
#             border = "white")
#   }else{
#     polygon(x=c(0.275,0.275, 0.275+0.175, 0.275+0.175)+0.5+1, 
#             y = c(xx_p[i-1], rep(xx_p[i],2), xx_p[i-1]), 
#             col = col_bar[i], 
#             border = "white")
#   }
# }
# 
# ################################
# # Average YLL
# 
# par(new = TRUE)
# plot(x=0, y= 0, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 12),
#      xlim=c(0,2), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# 
# # segments(x0=0, y0=seq(0, 12, by = 2), 
# #          x1=2, y1=seq(0, 12, by = 2), col = "lightgray", 
# #          lwd = 2)
# # 
# # segments(x0=0, y0=seq(1, 11, by = 2), 
# #          x1=2, y1=seq(1, 11, by = 2), col = "lightgray", 
# #          lwd = 1)
# # 
# # segments(x0=1, 0, 
# #          x1=1, y1=12, col = "lightgray", 
# #          lwd = 2)
# axis(4, at = seq(0, 12, by = 2), labels = TRUE, lwd = 3, cex.axis = 1.3)
# axis(4, at = seq(1, 11, by = 2), labels = FALSE, lwd = 1)
# 
# axis(1, at = c(0.25, 0.75), labels = 
#        c("Covid-19\nCause-of-death", "Excess\nMortality"), lwd = 0, 
#      cex.axis = 1.3, pos = -0.5)
# 
# axis(1, at = c(1.25, 1.75), labels = 
#        c("Covid-19\nCause-of-death", "Excess\nMortality"), lwd = 0, 
#      cex.axis = 1.3, pos = -0.5)
# 
# mtext("Women", 3, adj = 0.01, cex = 1.4, line = -1.5)
# mtext("Men", 3, adj = 0.55, cex = 1.4, line = -1.5)
# mtext("Average years of life lost (per death)", 3, cex = 1.6, line = 1)
# 
# ##########################
# # Females
# fem_all <- 
#   yll_sum_all %>% 
#   filter(sex == "f" & source %in% c("C19", "excess 5y"))
# 
# lines(x = 0.25, y= fem_all$average_yll[1], pch = 16, 
#       cex = 1.5, col = "black", typ = "p")
# lines(x = 0.75, y= fem_all$average_yll[2], pch = 16, 
#       cex = 1.5, col = "black", typ = "p")
# 
# # Total adjusted 
# fem_adj_tot <- 
#   yll_sum_adj %>%
#   group_by(source, sex) %>% 
#   summarize(yll_sum = sum(yll_sum),
#             dts_sum = sum(dts_sum)) %>% 
#   mutate(mean=yll_sum/dts_sum) %>% 
#   filter(sex == "f" & source %in% c("C19", "excess 5y"))
# 
# lines(x = 0.25, y= fem_adj_tot$mean[1], pch = 15, 
#       cex = 1.5, col = "black", typ = "p")
# lines(x = 0.75, y= fem_adj_tot$mean[2], pch = 15, 
#       cex = 1.5, col = "black", typ = "p")
# 
# # Care adjusted
# yll_sum_adj %>%
#   filter(sex == "f" & source %in% c("C19") & !care =="total") %>%
#   mutate(col.p = mapvalues(care, from = c("NC", "HC", "CH"),
#                            to = col_bar[2:4])) %>% 
#   group_by(care) %>% 
#   group_map(~lines(x=0.25, y=.$average_yll, typ = "p", pch = 15, col =.$col.p, 
#                    cex = 1.5))
# 
# yll_sum_adj %>%
#   filter(sex == "f" & source %in% c("excess 5y") & !care =="total") %>%
#   mutate(col.p = mapvalues(care, from = c("NC", "HC", "CH"),
#                            to = col_bar[2:4])) %>% 
#   group_by(care) %>% 
#   group_map(~lines(x=0.75, y=.$average_yll, typ = "p", pch = 15, col =.$col.p, 
#                    cex = 1.5))
# 
# # Males
# male_all <- 
#   yll_sum_all %>% 
#   filter(sex == "m" & source %in% c("C19", "excess 5y"))
# 
# lines(x = 0.25+1, y= male_all$average_yll[1], pch = 16, 
#       cex = 1.5, col = "black", typ = "p")
# lines(x = 0.75+1, y= male_all$average_yll[2], pch = 16, 
#       cex = 1.5, col = "black", typ = "p")
# 
# # Total adjusted 
# male_adj_tot <- 
#   yll_sum_adj %>%
#   group_by(source, sex) %>% 
#   summarize(yll_sum = sum(yll_sum),
#             dts_sum = sum(dts_sum)) %>% 
#   mutate(mean=yll_sum/dts_sum) %>% 
#   filter(sex == "m" & source %in% c("C19", "excess 5y"))
# 
# lines(x = 0.25+1, y= male_adj_tot$mean[1], pch = 15, 
#       cex = 1.5, col = "black", typ = "p")
# lines(x = 0.75+1, y= male_adj_tot$mean[2], pch = 15, 
#       cex = 1.5, col = "black", typ = "p")
# 
# # Care adjusted
# yll_sum_adj %>%
#   filter(sex == "m" & source %in% c("C19") & !care =="total") %>%
#   mutate(col.p = mapvalues(care, from = c("NC", "HC", "CH"),
#                            to = col_bar[2:4])) %>% 
#   group_by(care) %>% 
#   group_map(~lines(x=0.25+1, y=.$average_yll, typ = "p", pch = 15, col =.$col.p, 
#                    cex = 1.5))
# 
# yll_sum_adj %>%
#   filter(sex == "m" & source %in% c("excess 5y") & !care =="total") %>%
#   mutate(col.p = mapvalues(care, from = c("NC", "HC", "CH"),
#                            to = col_bar[2:4])) %>% 
#   group_by(care) %>% 
#   group_map(~lines(x=0.75+1, y=.$average_yll, typ = "p", pch = 15, col =.$col.p, 
#                    cex = 1.5))
# 
# dev.off()