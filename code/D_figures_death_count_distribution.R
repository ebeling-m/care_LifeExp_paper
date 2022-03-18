# Main article figures 1
# Plot one death distribution 
source("code/A_00_packages.R")

death <- read_rds("data_inter/ungrouped_all_c19_excess_0_100_care_status.rds")

# Take only single ages data
death <- 
  death %>% 
  filter(!source == "excess 1y")

# Total for deaths below 70
total <- 
  death %>% 
  filter(care %in% c("all") & age < 70)

# Care for deaths above 70
care <- 
  death %>% 
  filter(!care %in% c("all") & age >= 70)

col.care <- adjustcolor(c('#1b9e77','#d95f02','#7570b3'), alpha.f = 0.7)

############################################################################
pdf("figures/deathCount_distribution_sidesChanges.pdf", height =10, width = 14, 
    pointsize = 13)
par(mar=c(3,4,2.5,1))
layout(rbind(1,2,3), height = c(0.47, 0.47, 0.06))
# Women
plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 350),
     xlim=c(0,101+5+101), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=0, y0=seq(0, 350, by = 50), x1=101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=101+5, y0=seq(0, 350, by = 50), x1=101+5+101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10), y0=0, x1=seq(0.5, 100.5, by = 10), y1=350, 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10)+100.5+5, y0=0, 
         x1=seq(0.5, 100.5, by = 10)+100.5+5, y1=350, 
         col = "darkgray")

axis(1, at = seq(0.5, 100.5, by = 10), seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
axis(1, at = seq(0.5, 100.5, by = 10)+100.5+5, seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
# axis(1, at = seq(5, 95, by = 10), labels = FALSE, lwd = 1)
axis(2, at = seq(0, 350, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
axis(2, at = seq(25, 325, by = 50), labels = FALSE, lwd = 1)

legend(x=0, y=300, 
       legend = c("No care", 
                  "Home care", "Care home", "Care status unknown"),
       col = c('#1b9e77','#d95f02','#7570b3', "gray"), 
       bty = "n", pch = 15, cex = 1.5)

# Plot excess
# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "f" & age == i & source == "excess 5y")
  xx <- c(i, i, i+1, i+1)
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "f" & age == i & source == "C19")
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

text(x=0, y= 325, "Excess deaths",cex = 1.7, pos = 4)
text(x=101+5, y= 325, "Covid-19 as underlying cause of death", cex = 1.7, pos = 4)
mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Women", 3, line = 0.5, cex = 1.7, adj = 0.01)

#########################################################
# Men
plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 350),
     xlim=c(0,101+5+101), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=0, y0=seq(0, 350, by = 50), x1=101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=101+5, y0=seq(0, 350, by = 50), x1=101+5+101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10), y0=0, x1=seq(0.5, 100.5, by = 10), y1=350, 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10)+100.5+5, y0=0, 
         x1=seq(0.5, 100.5, by = 10)+100.5+5, y1=350, 
         col = "darkgray")


axis(1, at = seq(0.5, 100.5, by = 10), seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
axis(1, at = seq(0.5, 100.5, by = 10)+100.5+5, seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
# axis(1, at = seq(5, 95, by = 10), labels = FALSE, lwd = 1)
axis(2, at = seq(0, 350, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
axis(2, at = seq(25, 325, by = 50), labels = FALSE, lwd = 1)

# Plot excess
# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "m" & age == i & source == "excess 5y")
  xx <- c(i, i, i+1, i+1)
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}


# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "m" & age == i & source == "C19")
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

mtext("Age", 1, line = 4, cex = 1.5)
mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Men", 3, line = 0.5, cex = 1.7, adj = 0.01)

# legend
# par(mar = c(0,0,0,0))
# plot(x=0:1, y= 0:1, typ = "n", 
#      xaxs = "i", yaxs = "i", bty = "n",
#      ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
# legend("center", horiz = TRUE, legend = c("Deaths below 70", "No care", "Home care", "Care home"),
#        col = c("gray", '#1b9e77','#d95f02','#7570b3'), bty = "n", pch = 15, cex = 1.7)

dev.off()


###########################################################################
pdf("figures/deathCount_distribution.pdf", height =10, width = 14, 
    pointsize = 13)
par(mar=c(3,4,2.5,1))
layout(rbind(1,2,3), height = c(0.47, 0.47, 0.06))
# Women
plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 350),
     xlim=c(0,101+5+101), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=0, y0=seq(0, 350, by = 50), x1=101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=101+5, y0=seq(0, 350, by = 50), x1=101+5+101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10), y0=0, x1=seq(0.5, 100.5, by = 10), y1=350, 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10)+100.5+5, y0=0, 
         x1=seq(0.5, 100.5, by = 10)+100.5+5, y1=350, 
         col = "darkgray")

axis(1, at = seq(0.5, 100.5, by = 10), seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
axis(1, at = seq(0.5, 100.5, by = 10)+100.5+5, seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
# axis(1, at = seq(5, 95, by = 10), labels = FALSE, lwd = 1)
axis(2, at = seq(0, 350, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
axis(2, at = seq(25, 325, by = 50), labels = FALSE, lwd = 1)

# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "f" & age == i & source == "C19")
  xx <- c(i, i, i+1, i+1)
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "f" & age == i & source == "C19" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

# Plot excess
# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "f" & age == i & source == "excess 5y")
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "f" & age == i & source == "excess 5y" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

text(x=0, y= 325, "Covid-19 as underlying cause of death",cex = 1.7, pos = 4)
text(x=101+5, y= 325, "Excess mortality", cex = 1.7, pos = 4)
mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Women", 3, line = 0.5, cex = 1.7, adj = 0.01)

#########################################################
# Men
plot(x=0:101, y= 0:101, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 350),
     xlim=c(0,101+5+101), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")

segments(x0=0, y0=seq(0, 350, by = 50), x1=101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=101+5, y0=seq(0, 350, by = 50), x1=101+5+101, y1=seq(0, 350, by = 50), 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10), y0=0, x1=seq(0.5, 100.5, by = 10), y1=350, 
         col = "gray")

segments(x0=seq(0.5, 100.5, by = 10)+100.5+5, y0=0, 
         x1=seq(0.5, 100.5, by = 10)+100.5+5, y1=350, 
         col = "darkgray")


axis(1, at = seq(0.5, 100.5, by = 10), seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
axis(1, at = seq(0.5, 100.5, by = 10)+100.5+5, seq(0, 100, by = 10), 
     lwd = 3, pos =-2, cex.axis = 1.3)
# axis(1, at = seq(5, 95, by = 10), labels = FALSE, lwd = 1)
axis(2, at = seq(0, 350, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
axis(2, at = seq(25, 325, by = 50), labels = FALSE, lwd = 1)

# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "m" & age == i & source == "C19")
  xx <- c(i, i, i+1, i+1)
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "m" & age == i & source == "C19" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

# Plot excess
# Plot total until 69
for(i in 0:69){
  plotDat <- 
    total %>% 
    filter(sex == "m" & age == i & source == "excess 5y")
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy <- c(0, rep(plotDat$dts, 2), 0)
  polygon(x=xx, y=yy, col = adjustcolor("gray", alpha.f = 0.7), border = "white")
}

# Plot care
for(i in 70:100){
  nc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "NC")
  hc <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "HC")
  ch <- 
    care %>% 
    filter(sex == "m" & age == i & source == "excess 5y" & care == "CH")
  
  xx <- c(i, i, i+1, i+1)+100.5+5
  yy1 <- c(0, rep(nc$dts, 2), 0)
  yy2 <- c(nc$dts, rep(nc$dts+hc$dts, 2), nc$dts)
  yy3 <- c(nc$dts+hc$dts, 
           rep(nc$dts+hc$dts+ch$dts, 2), 
           nc$dts+hc$dts)
  polygon(x=xx, y=yy1, col = col.care[1], border = "white")
  polygon(x=xx, y=yy2, col = col.care[2], border = "white")
  polygon(x=xx, y=yy3, col = col.care[3], border = "white")
}

mtext("Age", 1, line = 3, cex = 1.5)
mtext("Deaths", 2, line = 2.3, cex = 1.5)
mtext("Men", 3, line = 0.5, cex = 1.7, adj = 0.01)

# legend
par(mar = c(0,0,0,0))
plot(x=0:1, y= 0:1, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n",
     ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
legend("center", horiz = TRUE, legend = c("Deaths below 70", "No care", "Home care", "Care home"),
       col = c("gray", '#1b9e77','#d95f02','#7570b3'), bty = "n", pch = 15, cex = 1.7)

dev.off()