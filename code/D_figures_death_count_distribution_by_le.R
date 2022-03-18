# Main Fig Deaths by life expectancy
# Table for life years Lost 
source("code/A_00_packages.R")

yll <- read_rds("data_output/yll_estimates_sex_age_care.rds")

# unadjusted deaths
yll <- 
  yll %>% 
  mutate(ex_gr = as.integer(ex_yll),
         ex_gr = ifelse(ex_gr >= 12, 12, ex_gr),
         care = ifelse(age < 70, "total", care)) %>% 
  group_by(source, care, sex, status, ex_gr) %>% 
  summarise(sum_dts = sum(dts))

# undadjusted
adj <- 
  yll %>% 
  filter(status == "adjusted")

# Total deaths
adj_tot <- 
  adj %>% 
  group_by(source, sex, status) %>% 
  summarise(dts_tot = sum(sum_dts)) %>% 
  right_join(adj) %>% 
  mutate(share = sum_dts/dts_tot)

# Create figure
# col.care <- adjustcolor(c('#1b9e77','#d95f02','#7570b3'), alpha.f = 0.7)
pdf("figures/deathCount_distribution_LE_sideChanged.pdf", height =10, width = 17,
    pointsize = 13)
par(mar=c(3,4,2.5,1))
layout(rbind(c(1,2), c(3,4), c(5,5)), height = c(0.47, 0.47, 0.06))
# Women Excess mortality

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.3),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.3, by = 0.05), x1=13, y1=seq(0, 0.3, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.3, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.3, by = 0.05), 
     labels = paste(round(seq(0, 0.3, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.275, by = 0.05), labels = FALSE, lwd = 1)

# Females Excess mortality
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "f" & source == "excess 5y") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

mtext("Excess mortality", 3, cex = 1.4, line = -3)

# text(x=101+5, y= 325, "Excess mortality", cex = 1.7, pos = 4)
# mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Women", 3, line = 0.5, cex = 1.7, adj = 0.01)

legend("topright", 
       legend = c("No care", 
                  "Home care", "Care home", "Care status unknown"),
       col = c('#1b9e77','#d95f02','#7570b3', "gray"), 
       bty = "n", pch = 15, cex = 1.7)

# Women C19 cause of death 


plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.3),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.3, by = 0.05), x1=13, y1=seq(0, 0.3, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.3, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.3, by = 0.05), 
     labels = paste(round(seq(0, 0.3, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.275, by = 0.05), labels = FALSE, lwd = 1)

# Females C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "f" & source == "C19") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

mtext("Covid-19 as underlying cause of death",3, cex = 1.4, line = -3)

#########################################################
# Men C 19


plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.3),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.3, by = 0.05), x1=13, y1=seq(0, 0.3, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.3, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.3, by = 0.05), 
     labels = paste(round(seq(0, 0.3, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.275, by = 0.05), labels = FALSE, lwd = 1)

# Females C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "m" & source == "excess 5y") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

mtext("Remaining life expectancy", 1, line =3.5, cex = 1.2)

# text(x=0.5, y= 0.375, "Covid-19 as underlying cause of death",cex = 1.7, pos = 4)
# text(x=101+5, y= 325, "Excess mortality", cex = 1.7, pos = 4)
# mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Men", 3, line = 0.5, cex = 1.7, adj = 0.01)

# Men Excess 

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.3),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.3, by = 0.05), x1=13, y1=seq(0, 0.3, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.3, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.3, by = 0.05), 
     labels = paste(round(seq(0, 0.3, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.275, by = 0.05), labels = FALSE, lwd = 1)

mtext("Remaining life expectancy", 1, line =3.5, cex = 1.2)

# males C 19 cause of  death
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "m" & source == "C19") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

# # legend
# par(mar = c(0,0,0,0))
# plot(x=0:1, y= 0:1, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n",
#      ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# legend("center", horiz = TRUE, legend = c("Deaths below 70", "No care", "Home care", "Care home"),
#        col = c("gray", '#1b9e77','#d95f02','#7570b3'), bty = "n", pch = 15, cex = 1.7)
dev.off()






#######################################################################
# Create figure
# col.care <- adjustcolor(c('#1b9e77','#d95f02','#7570b3'), alpha.f = 0.7)
pdf("figures/deathCount_distribution_LE.pdf", height =10, width = 17,
    pointsize = 13)
par(mar=c(3,4,2.5,1))
layout(rbind(c(1,2), c(3,4), c(5,5)), height = c(0.47, 0.47, 0.06))
# Women C 19

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.4),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.4, by = 0.05), x1=13, y1=seq(0, 0.4, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.4, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.4, by = 0.05), 
     labels = paste(round(seq(0, 0.4, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.375, by = 0.05), labels = FALSE, lwd = 1)

# Females C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "f" & source == "C19") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
  xx <- c(i, i, i+1, i+1)
  if(j == 1){
  yy <- c(0, rep(y_val[j], 2), 0)
  polygon(x=xx, y=yy, col = col_c[j], border = "white")
  }else{
  yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
  polygon(x=xx, y=yy, col = col_c[j], border = "white")
  }
  }
}

text(x=1, y= 0.375, "Covid-19 as underlying cause of death",cex = 1.7, pos = 4)
# text(x=101+5, y= 325, "Excess mortality", cex = 1.7, pos = 4)
# mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Women", 3, line = 0.5, cex = 1.7, adj = 0.01)

# Women Excess 

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.4),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.4, by = 0.05), x1=13, y1=seq(0, 0.4, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.4, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.4, by = 0.05), 
     labels = paste(round(seq(0, 0.4, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.375, by = 0.05), labels = FALSE, lwd = 1)

# Females C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "f" & source == "excess 5y") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

text(x=1, y= 0.375, "Excess mortality",cex = 1.7, pos = 4)


#########################################################
# Men C 19

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.4),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.4, by = 0.05), x1=13, y1=seq(0, 0.4, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.4, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.4, by = 0.05), 
     labels = paste(round(seq(0, 0.4, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.375, by = 0.05), labels = FALSE, lwd = 1)
mtext("Remaining life expectancy", 1, line =2.8, cex = 1.2)
# males C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "m" & source == "C19") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}

# text(x=0.5, y= 0.375, "Covid-19 as underlying cause of death",cex = 1.7, pos = 4)
# text(x=101+5, y= 325, "Excess mortality", cex = 1.7, pos = 4)
# mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Men", 3, line = 0.5, cex = 1.7, adj = 0.01)

# Men Excess 

plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 0.4),
     xlim=c(1,13), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=1, y0=seq(0, 0.4, by = 0.05), x1=13, y1=seq(0, 0.4, by = 0.05), 
         col = "gray", lwd = 2)

segments(x0=seq(1, 13, by = 1), y0=0, x1=seq(1, 13, by = 1), y1=0.4, 
         col = "gray")

axis(1, at = c(seq(1, 12, by = 1), 14), lwd = 3, 
     labels = c(seq(1, 11, by = 1), ">12", NA), cex.axis = 1.3, pos = -0.005)

par(las=1)
axis(2, at = seq(0, 0.4, by = 0.05), 
     labels = paste(round(seq(0, 0.4, by = 0.05)*100,0), "%", sep = ""), 
     lwd = 3, cex.axis = 1.3)
axis(2, at = seq(0.025, 0.375, by = 0.05), labels = FALSE, lwd = 1)
mtext("Remaining life expectancy", 1, line =2.8, cex = 1.2)

# Females C 19
plotDat <- 
  adj_tot %>%
  ungroup() %>% 
  filter(sex == "m" & source == "excess 5y") %>% 
  mutate(col.care = mapvalues(care, 
                              from = c("total", "NC", "HC", "CH"),
                              to = adjustcolor(c("gray", '#1b9e77','#d95f02','#7570b3'), 
                                               alpha.f = 0.7)))

for(i in unique(plotDat$ex_gr)){
  plotDat_1 <- plotDat %>% filter(ex_gr == i)
  
  # Death counts
  y_val <- plotDat_1$share
  names(y_val) <- plotDat_1$care
  y_val <- y_val[c("total", "NC", "HC", "CH")]
  y_val <- cumsum(y_val[!is.na(y_val)])
  
  # Color
  col_c <- plotDat_1$col.care
  names(col_c) <- plotDat_1$care
  col_c <- col_c[c("total", "NC", "HC", "CH")]
  col_c <- col_c[!is.na(col_c)]
  
  for(j in 1:length(y_val)){
    xx <- c(i, i, i+1, i+1)
    if(j == 1){
      yy <- c(0, rep(y_val[j], 2), 0)
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }else{
      yy <- c(y_val[j-1], rep(y_val[j], 2), y_val[j-1])
      polygon(x=xx, y=yy, col = col_c[j], border = "white")
    }
  }
}



# legend
par(mar = c(0,0,0,0))
plot(x=0:1, y= 0:1, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n",
     ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
legend("center", horiz = TRUE, legend = c("Deaths below 70", "No care", "Home care", "Care home"),
       col = c("gray", '#1b9e77','#d95f02','#7570b3'), bty = "n", pch = 15, cex = 1.7)
dev.off()
