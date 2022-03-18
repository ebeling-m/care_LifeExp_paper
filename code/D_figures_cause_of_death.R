# Main Fig cause of death comparison
# Table for life years Lost 
source("code/A_00_packages.R")

yll <- read_rds("data_output/yll_estimates_sex_age_care.rds")
load("data_output/yll_cause_of_death.RData")
load("data_output/yll_cause_of_death_totalLE.RData")

# Care adjusted estimates 
yll_sel <- 
  yll %>% 
  filter(status == "adjusted" & age >= 70) %>% 
  group_by(source, care, sex) %>% 
  summarise(sum_yll = sum(yll), 
            sum_Dx = sum(dts)) %>% 
  filter(source %in% c("C19", "excess 5y"))

cod_yll <-
  cod_yll %>% 
  filter(!CodGr %in% c(1,8)) %>% 
  arrange(sum_yll) %>% 
  mutate(mean_yll = sum_yll/sum_Dx)

# Unadjusted estimates 
yll_sel_total <- 
  yll %>% 
  filter(status == "unadjusted" & age >= 70) %>% 
  group_by(source, care, sex) %>% 
  summarise(sum_yll = sum(yll), 
            sum_Dx = sum(dts)) %>% 
  filter(source %in% c("C19", "excess 5y"))

cod_yll_total <-
  cod_yll_total %>% 
  filter(!CodGr %in% c(1,8)) %>% 
  arrange(sum_yll) %>% 
  mutate(mean_yll = sum_yll/sum_Dx)

# range(cod_yll_total$sum_yll)
# plot: 
# 2-infectious
# 4-IHD
# 5-stroke
# 6-resp
# 7-external

dis.label <- 
  c("Excess\n2020", "Covid-19", 
    "Infectious\nparasitic dis.", "Colorectal\ncancer", "Ischeamic\nheart dis.", 
    "Stroke", "Respiratory\ndiseases", "External\ncauses")
yy.label <- seq(15.5, 1.5, by = -2)

care.col <- adjustcolor(c('#1b9e77','#d95f02','#7570b3'), 
            alpha.f = 1)
names(care.col) <- c("NC", "HC", "CH")

cod_gr <- c(2, 3, 4, 5, 6, 7)
col_tot <- gray(level = 0.35)

pdf("figures/cause_of_death_YLL.pdf", family = "Times", width = 10, height = 10,
    pointsize = 14)
layout(rbind(1,2))
par(mar = c(2, 4, 1, 2))

# Women
plot(x=0, y=0, typ = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
     xlab = NA, ylab = NA, bty = "n", ylim = c(0, 38000), xlim = c(0, 16))
for(i in 1:8){
text(x = seq(1, 15, by = 2)[i], y = 0, labels = dis.label[i], pos = 1, xpd = TRUE)
}  

segments(y0=seq(0, 36000, by = 4000), x0=0, y1=seq(0, 36000, by = 4000), x1=16,
         lwd = 1.5, col = "lightgray")

segments(y0=seq(2000, 34000, by = 4000), x0=0, y1=seq(2000, 34000, by = 4000), x1=16,
         lwd = 0.5, col = "lightgray")

par(las = 1)
axis(2, at = c(seq(0, 36000, by = 4000), 40000), labels = TRUE, lwd = 3, pos = -0.1)
axis(2, at = seq(2000, 34000, by = 4000), labels = FALSE, lwd = 1, pos = -0.1)
axis(1, at = seq(0, 16, by = 2), labels = FALSE, lwd = 3)
mtext("Women", 3, line = -0.5, cex = 1.4, adj = 0.99)
mtext("YLL", 3, line = -0.5, cex = 1.3, adj = -0.1)

# Excess
exm <- 
  yll_sel_total %>% 
  filter(source == "excess 5y" & sex == "f")
yy_val <- exm$sum_yll

polygon(x=c(0.1, 0.1, 0.9, 0.9), y=c(0, rep(yy_val,2), 0), 
        col = col_tot, border = "white")

# Excess care
exm_c <- 
  yll_sel %>% 
  filter(source == "excess 5y" & sex == "f")

yy_val_c <- exm_c$sum_yll
names(yy_val_c) <- exm_c$care

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(0, rep(yy_val_c["NC"],2), 0), 
        col = care.col["NC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(yy_val_c["NC"], rep(sum(yy_val_c[c("NC", "HC")]),2), yy_val_c["NC"]), 
        col = care.col["HC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(sum(yy_val_c[c("NC", "HC")]), rep(sum(yy_val_c),2), sum(yy_val_c[c("NC", "HC")])), 
        col = care.col["CH"], border = "white")

# C-19
c19 <- 
  yll_sel_total %>% 
  filter(source == "C19" & sex == "f")

yy_val <- c19$sum_yll
polygon(x=c(0.1, 0.1, 0.9, 0.9)+2, y=c(0, rep(yy_val,2), 0), 
        col = col_tot, border = "white")
# C-19 Care
c19_c <- 
  yll_sel %>% 
  filter(source == "C19" & sex == "f")
yy_val_c <- c19_c$sum_yll
names(yy_val_c) <- c19_c$care

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(0, rep(yy_val_c["NC"],2), 0), 
        col = care.col["NC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(yy_val_c["NC"], rep(sum(yy_val_c[c("NC", "HC")]),2), yy_val_c["NC"]), 
        col = care.col["HC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(sum(yy_val_c[c("NC", "HC")]), rep(sum(yy_val_c),2), sum(yy_val_c[c("NC", "HC")])), 
        col = care.col["CH"], border = "white")

# Other causes of death 
fact_add <- seq(4, 14, by = 2)

for(i in 1:length(cod_gr)){
  # Total 2019
  cod_dat_tot <- 
    cod_yll_total %>% 
    filter(Year == 2019 & CodGr == cod_gr[i] & sex == 0)

  yy_val_tot <- cod_dat_tot$sum_yll
  
  polygon(x=c(0.1, 0.1, 0.5, 0.5)+fact_add[i], y=c(0, rep(yy_val_tot,2), 0), 
          col = col_tot, border = "white")
  
  # Total 2020
  cod_dat_tot <- 
    cod_yll_total %>% 
    filter(Year == 2020 & CodGr == cod_gr[i] & sex == 0)
  
  yy_val_tot <- cod_dat_tot$sum_yll
  
  polygon(x=c(0.1, 0.1, 0.5, 0.5)+fact_add[i]+1, y=c(0, rep(yy_val_tot,2), 0), 
          col = col_tot, border = "white")
  
  # Care 2019
  cod_dat <- 
    cod_yll %>% 
    filter(Year == 2019 & CodGr == cod_gr[i] & sex == 0)
  
  yy_val <- cod_dat$sum_yll
  names(yy_val) <- cod_dat$care
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(0, rep(yy_val["NC"],2), 0), 
          col = care.col["NC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(yy_val["NC"], rep(sum(yy_val[c("NC", "HC")]),2), yy_val["NC"]), 
          col = care.col["HC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(sum(yy_val[c("NC", "HC")]), rep(sum(yy_val),2), sum(yy_val[c("NC", "HC")])), 
          col = care.col["CH"], border = "white")
  
  # Care 2020
  cod_dat <- 
    cod_yll %>% 
    filter(Year == 2020 & CodGr == cod_gr[i] & sex == 0)
  
  yy_val <- cod_dat$sum_yll
  names(yy_val) <- cod_dat$care
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(0, rep(yy_val["NC"],2), 0), 
          col = care.col["NC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(yy_val["NC"], rep(sum(yy_val[c("NC", "HC")]),2), yy_val["NC"]), 
          col = care.col["HC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(sum(yy_val[c("NC", "HC")]), rep(sum(yy_val),2), sum(yy_val[c("NC", "HC")])), 
          col = care.col["CH"], border = "white")
}

text(x=4.25, y=8000, "2019", srt = 45, cex = 1.2, pos = 4)
text(x=5.25, y=8000, "2020", srt = 45, cex = 1.2, pos = 4)

legend("topright", legend = c("Total - unadjusted", "No care", "Home care", "Care home"), 
       col = c(col_tot, care.col), pch = 15, bty = "n", cex = 1)

#########################################
# Men
plot(x=0, y=0, typ = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
     xlab = NA, ylab = NA, bty = "n", ylim = c(0, 38000), xlim = c(0, 16))
for(i in 1:8){
  text(x = seq(1, 15, by = 2)[i], y = 0, labels = dis.label[i], pos = 1, xpd = TRUE)
}  

segments(y0=seq(0, 36000, by = 4000), x0=0, y1=seq(0, 36000, by = 4000), x1=16,
         lwd = 1.5, col = "lightgray")

segments(y0=seq(2000, 34000, by = 4000), x0=0, y1=seq(2000, 34000, by = 4000), x1=16,
         lwd = 0.5, col = "lightgray")

par(las = 1)
axis(2, at = c(seq(0, 36000, by = 4000), 40000), labels = TRUE, lwd = 3, pos = -0.1)
axis(2, at = seq(2000, 34000, by = 4000), labels = FALSE, lwd = 1, pos = -0.1)
axis(1, at = seq(0, 16, by = 2), labels = FALSE, lwd = 3)
mtext("Men", 3, line = -0.5, cex = 1.4, adj = 0.99)
mtext("YLL", 3, line = -0.5, cex = 1.3, adj = -0.1)

# Excess
exm <- 
  yll_sel_total %>% 
  filter(source == "excess 5y" & sex == "m")
yy_val <- exm$sum_yll

polygon(x=c(0.1, 0.1, 0.9, 0.9), y=c(0, rep(yy_val,2), 0), 
        col = col_tot, border = "white")

# Excess care
exm_c <- 
  yll_sel %>% 
  filter(source == "excess 5y" & sex == "m")

yy_val_c <- exm_c$sum_yll
names(yy_val_c) <- exm_c$care

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(0, rep(yy_val_c["NC"],2), 0), 
        col = care.col["NC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(yy_val_c["NC"], rep(sum(yy_val_c[c("NC", "HC")]),2), yy_val_c["NC"]), 
        col = care.col["HC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+1, 
        y=c(sum(yy_val_c[c("NC", "HC")]), rep(sum(yy_val_c),2), sum(yy_val_c[c("NC", "HC")])), 
        col = care.col["CH"], border = "white")

# C-19
c19 <- 
  yll_sel_total %>% 
  filter(source == "C19" & sex == "m")

yy_val <- c19$sum_yll
polygon(x=c(0.1, 0.1, 0.9, 0.9)+2, y=c(0, rep(yy_val,2), 0), 
        col = col_tot, border = "white")
# C-19 Care
c19_c <- 
  yll_sel %>% 
  filter(source == "C19" & sex == "m")
yy_val_c <- c19_c$sum_yll
names(yy_val_c) <- c19_c$care

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(0, rep(yy_val_c["NC"],2), 0), 
        col = care.col["NC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(yy_val_c["NC"], rep(sum(yy_val_c[c("NC", "HC")]),2), yy_val_c["NC"]), 
        col = care.col["HC"], border = "white")

polygon(x=c(0.1, 0.1, 0.9, 0.9)+3, 
        y=c(sum(yy_val_c[c("NC", "HC")]), rep(sum(yy_val_c),2), sum(yy_val_c[c("NC", "HC")])), 
        col = care.col["CH"], border = "white")

# Other causes of death 
fact_add <- seq(4, 14, by = 2)

for(i in 1:length(cod_gr)){
  # Total 2019
  cod_dat_tot <- 
    cod_yll_total %>% 
    filter(Year == 2019 & CodGr == cod_gr[i] & sex == 1)
  
  yy_val_tot <- cod_dat_tot$sum_yll
  
  polygon(x=c(0.1, 0.1, 0.5, 0.5)+fact_add[i], y=c(0, rep(yy_val_tot,2), 0), 
          col = col_tot, border = "white")
  
  # Total 2020
  cod_dat_tot <- 
    cod_yll_total %>% 
    filter(Year == 2020 & CodGr == cod_gr[i] & sex == 1)
  
  yy_val_tot <- cod_dat_tot$sum_yll
  
  polygon(x=c(0.1, 0.1, 0.5, 0.5)+fact_add[i]+1, y=c(0, rep(yy_val_tot,2), 0), 
          col = col_tot, border = "white")
  
  # Care 2019
  cod_dat <- 
    cod_yll %>% 
    filter(Year == 2019 & CodGr == cod_gr[i] & sex == 1)
  
  yy_val <- cod_dat$sum_yll
  names(yy_val) <- cod_dat$care
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(0, rep(yy_val["NC"],2), 0), 
          col = care.col["NC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(yy_val["NC"], rep(sum(yy_val[c("NC", "HC")]),2), yy_val["NC"]), 
          col = care.col["HC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i], 
          y=c(sum(yy_val[c("NC", "HC")]), rep(sum(yy_val),2), sum(yy_val[c("NC", "HC")])), 
          col = care.col["CH"], border = "white")
  
  # Care 2020
  cod_dat <- 
    cod_yll %>% 
    filter(Year == 2020 & CodGr == cod_gr[i] & sex == 1)
  
  yy_val <- cod_dat$sum_yll
  names(yy_val) <- cod_dat$care
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(0, rep(yy_val["NC"],2), 0), 
          col = care.col["NC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(yy_val["NC"], rep(sum(yy_val[c("NC", "HC")]),2), yy_val["NC"]), 
          col = care.col["HC"], border = "white")
  
  polygon(x=c(0.5, 0.5, 0.9, 0.9)+fact_add[i]+1, 
          y=c(sum(yy_val[c("NC", "HC")]), rep(sum(yy_val),2), sum(yy_val[c("NC", "HC")])), 
          col = care.col["CH"], border = "white")
}
dev.off()


# pdf("figures/cause_of_death_YLL.pdf", family = "Times", width = 10, height = 10,
#     pointsize = 14)
# layout(rbind(1,2))
# par(mar = c(4, 5, 3, 2))
# 
# # Women 
# plot(x=0, y=0, typ = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
#      xlab = NA, ylab = NA, bty = "n", ylim = c(0, 38000), xlim = c(0, 16))
# 
# segments(y0=seq(0, 36000, by = 4000), x0=0, y1=seq(0, 36000, by = 4000), x1=16,
#          lwd = 2, col = "lightgray")
# 
# segments(x0=seq(2000, 38000, by =4000), y0=0, x1=seq(2000, 38000, by = 4000), y1=16,
#          lwd = 1, col = "lightgray")
# par(las = 1)
# axis(2, at = yy.label, labels = dis.label,
#      lwd = 0, cex.axis = 0.8)
# 
# 
# # axis(2, at = yy.label[seq(1, 8, by = 2)], labels = dis.label[seq(1, 8, by = 2)],
# #      lwd = 0, cex.axis = 0.8)
# # axis(2, at = yy.label[seq(2, 8, by = 2)], labels = dis.label[seq(2, 8, by = 2)],
# #      lwd = 0, cex.axis = 0.8, pos = -1000)
# dev.off()
# 
# for(i in 1:length(dis.label)){
#   text(x=0, y=yy.label[i], dis.label[i], pos = 4, font = 1, col = "black", cex = 0.8)
# }
# 
# axis(1, at = c(seq(0, 36000, by = 4000), 40000), labels = TRUE, lwd = 3, pos = -0.1)
# axis(1, at = seq(2000, 38000, by = 4000), labels = FALSE, lwd = 1, pos = -0.1)
# 
# yy.total <- c(0.5,0.5, 1, 1)
# yy.care <- c(0, 0, 0.5, 0.5)
# 
# # C-19
# c19 <- 
#   yll_sel_total %>% 
#   filter(source == "C19" & sex == "f")
# 
# xx_val <- c19$sum_yll
# polygon(y=yy.total+12, x=c(0, rep(xx_val,2), 0), 
#         col = col_tot, border = "white")
# # Care
# c19_c <- 
#   yll_sel %>% 
#   filter(source == "C19" & sex == "f")
# xx_val_c <- c19_c$sum_yll
# names(xx_val_c) <- c19_c$care
# 
# polygon(y=yy.care+12, 
#         x=c(0, rep(xx_val_c["NC"],2), 0), 
#         col = care.col["NC"], border = "white")
# 
# polygon(y=yy.care+12, 
#         x=c(xx_val_c["NC"], rep(sum(xx_val_c[c("NC", "HC")]),2), xx_val_c["NC"]), 
#         col = care.col["HC"], border = "white")
# 
# polygon(y=yy.care+12, 
#         x=c(sum(xx_val_c[c("NC", "HC")]), rep(sum(xx_val_c),2), sum(xx_val_c[c("NC", "HC")])), 
#         col = care.col["CH"], border = "white")
# 
# # Excess
# exm <- 
#   yll_sel_total %>% 
#   filter(source == "excess 5y" & sex == "f")
# xx_val <- exm$sum_yll
# 
# polygon(y=yy.total+14, x=c(0, rep(xx_val,2), 0), 
#         col = col_tot, border = "white")
# 
# # care
# exm_c <- 
#   yll_sel %>% 
#   filter(source == "excess 5y" & sex == "f")
# xx_val_c <- exm_c$sum_yll
# names(xx_val_c) <- exm_c$care
# 
# polygon(y=yy.care+14, 
#         x=c(0, rep(xx_val_c["NC"],2), 0), 
#         col = care.col["NC"], border = "white")
# 
# polygon(y=yy.care+14, 
#         x=c(xx_val_c["NC"], rep(sum(xx_val_c[c("NC", "HC")]),2), xx_val_c["NC"]), 
#         col = care.col["HC"], border = "white")
# 
# polygon(y=yy.care+14, 
#         x=c(sum(xx_val_c[c("NC", "HC")]), rep(sum(xx_val_c),2), sum(xx_val_c[c("NC", "HC")])), 
#         col = care.col["CH"], border = "white")
# 
# # All other causes of death
# fact_add <- seq(10, 0, by = -2)
# 
# for(i in 1:length(cod_gr)){
#   # Total
#   cod_dat_tot <- 
#     cod_yll_total %>% 
#     filter(Year == 2019 & CodGr == cod_gr[i] & sex == 0)
#   
#   xx_val_tot <- cod_dat_tot$sum_yll
#   polygon(y=yy.total+fact_add[i], x=c(0, rep(xx_val_tot,2), 0), 
#           col = col_tot, border = "white")
#   # Care
#   cod_dat <- 
#     cod_yll %>% 
#     filter(Year == 2019 & CodGr == cod_gr[i] & sex == 0)
#   
#   xx_val <- cod_dat$sum_yll
#   names(xx_val) <- cod_dat$care
#   
#   polygon(y=yy.care+fact_add[i], 
#           x=c(0, rep(xx_val["NC"],2), 0), 
#           col = care.col["NC"], border = "white")
# 
#   polygon(y=yy.care+fact_add[i], 
#           x=c(xx_val["NC"], rep(sum(xx_val[c("NC", "HC")]),2), xx_val["NC"]), 
#           col = care.col["HC"], border = "white")
#   
#   polygon(y=yy.care+fact_add[i], 
#           x=c(sum(xx_val[c("NC", "HC")]), rep(sum(xx_val),2), sum(xx_val[c("NC", "HC")])), 
#           col = care.col["CH"], border = "white")
# }
# mtext("Women", 3, line = 1.5, cex = 1.6, adj = 0.01)
# legend("bottomright", legend = c("Total - unadjusted", "No care", "Home care", "Care home"), 
#        col = c(col_tot, care.col), pch = 15, bty = "n", cex = 1.2)
# 
# # Males
# plot(x=0, y=0, typ = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
#      xlab = NA, ylab = NA, bty = "n", xlim = c(0, 38000), ylim = c(0, 16))
# 
# segments(x0=seq(0, 36000, by = 4000), y0=0, x1=seq(0, 36000, by = 4000), y1=16,
#          lwd = 2, col = "lightgray")
# segments(x0=seq(2000, 38000, by =4000), y0=0, x1=seq(2000, 38000, by = 4000), y1=16,
#          lwd = 1, col = "lightgray")
# 
# for(i in 1:length(dis.label)){
#   text(x=0, y=yy.label[i], dis.label[i], pos = 4, font = 1, col = "black", cex = 0.8)
# }
# 
# axis(1, at = c(seq(0, 36000, by = 4000), 40000), labels = TRUE, lwd = 3, pos = -0.1)
# axis(1, at = seq(2000, 38000, by = 4000), labels = FALSE, lwd = 1, pos = -0.1)
# 
# yy.total <- c(0.5,0.5, 1, 1)
# yy.care <- c(0, 0, 0.5, 0.5)
# 
# # C-19
# c19 <- 
#   yll_sel_total %>% 
#   filter(source == "C19" & sex == "m")
# 
# xx_val <- c19$sum_yll
# polygon(y=yy.total+12, x=c(0, rep(xx_val,2), 0), 
#         col = col_tot, border = "white")
# # Care
# c19_c <- 
#   yll_sel %>% 
#   filter(source == "C19" & sex == "m")
# xx_val_c <- c19_c$sum_yll
# names(xx_val_c) <- c19_c$care
# 
# polygon(y=yy.care+12, 
#         x=c(0, rep(xx_val_c["NC"],2), 0), 
#         col = care.col["NC"], border = "white")
# 
# polygon(y=yy.care+12, 
#         x=c(xx_val_c["NC"], rep(sum(xx_val_c[c("NC", "HC")]),2), xx_val_c["NC"]), 
#         col = care.col["HC"], border = "white")
# 
# polygon(y=yy.care+12, 
#         x=c(sum(xx_val_c[c("NC", "HC")]), rep(sum(xx_val_c),2), sum(xx_val_c[c("NC", "HC")])), 
#         col = care.col["CH"], border = "white")
# 
# # Excess
# exm <- 
#   yll_sel_total %>% 
#   filter(source == "excess 5y" & sex == "m")
# xx_val <- exm$sum_yll
# 
# polygon(y=yy.total+14, x=c(0, rep(xx_val,2), 0), 
#         col = col_tot, border = "white")
# 
# # care
# exm_c <- 
#   yll_sel %>% 
#   filter(source == "excess 5y" & sex == "m")
# xx_val_c <- exm_c$sum_yll
# names(xx_val_c) <- exm_c$care
# 
# polygon(y=yy.care+14, 
#         x=c(0, rep(xx_val_c["NC"],2), 0), 
#         col = care.col["NC"], border = "white")
# 
# polygon(y=yy.care+14, 
#         x=c(xx_val_c["NC"], rep(sum(xx_val_c[c("NC", "HC")]),2), xx_val_c["NC"]), 
#         col = care.col["HC"], border = "white")
# 
# polygon(y=yy.care+14, 
#         x=c(sum(xx_val_c[c("NC", "HC")]), rep(sum(xx_val_c),2), sum(xx_val_c[c("NC", "HC")])), 
#         col = care.col["CH"], border = "white")
# 
# # All other causes of death
# fact_add <- seq(10, 0, by = -2)
# 
# for(i in 1:length(cod_gr)){
#   # Total
#   cod_dat_tot <- 
#     cod_yll_total %>% 
#     filter(Year == 2019 & CodGr == cod_gr[i] & sex == 1)
#   
#   xx_val_tot <- cod_dat_tot$sum_yll
#   polygon(y=yy.total+fact_add[i], x=c(0, rep(xx_val_tot,2), 0), 
#           col = col_tot, border = "white")
#   # Care
#   cod_dat <- 
#     cod_yll %>% 
#     filter(Year == 2019 & CodGr == cod_gr[i] & sex == 1)
#   
#   xx_val <- cod_dat$sum_yll
#   names(xx_val) <- cod_dat$care
#   
#   polygon(y=yy.care+fact_add[i], 
#           x=c(0, rep(xx_val["NC"],2), 0), 
#           col = care.col["NC"], border = "white")
#   
#   polygon(y=yy.care+fact_add[i], 
#           x=c(xx_val["NC"], rep(sum(xx_val[c("NC", "HC")]),2), xx_val["NC"]), 
#           col = care.col["HC"], border = "white")
#   
#   polygon(y=yy.care+fact_add[i], 
#           x=c(sum(xx_val[c("NC", "HC")]), rep(sum(xx_val),2), sum(xx_val[c("NC", "HC")])), 
#           col = care.col["CH"], border = "white")
# }
# 
# mtext("Men", 3, line = 1.5, cex = 1.6, adj = 0.01)
# mtext("Years of life lost", 1, line = 2.5, cex = 1.4)
# dev.off()