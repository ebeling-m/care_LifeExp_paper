
pdf("figures/lexis_probability.pdf", family = "Times")
par(xpd = TRUE)
plot(x=1, y=1, typ = "n", xlab = NA, ylab = NA, xlim = c(0,3),
     ylim = c(0,3), bty = "n", xaxt = "n", yaxt = "n", xaxs = "i",
     yaxs = "i")

segments(x0=0:3, y0=0, x1=0:3, y1=3, col = "lightgray", lwd = 2)
segments(x0=0, y0=0:3, x1=3, y1=0:3, col = "lightgray", lwd = 2)

lines(x=c(0, 3), y =c(0,3), col = "lightgray", lwd = 2)
lines(x=c(1, 3), y =c(0,2), col = "lightgray", lwd = 2)
lines(x=c(0, 2), y =c(1,3), col = "lightgray", lwd = 2)

for(i in c(0, 0.25, 0.5, 0.75)){
polygon(x=c(0, 0.25, 1.25, 1)+i,
        y=c(1, 1.25, 1.25, 1)+i, 
        col = adjustcolor("tomato", alpha.f = 0.6),
        border = "white")
}

for(i in c(0, 0.25, 0.5, 0.75)){
  polygon(x=c(0, 0.25, 1.25, 1)+i+1,
          y=c(1, 1.25, 1.25, 1)+i, 
          col = adjustcolor("steelblue", alpha.f = 0.6),
          border = "white")
}

axis(1, at = c(0:3), lwd = 3, labels = FALSE)
axis(1, at = seq(0.5, 2.5, by = 1), lwd = 0, labels = 2018:2020)
par(las = 1)
axis(2, at = c(0:3), lwd = 3, labels = c(80, 81, 82, 83))
axis(2, at = c(1.25, 1.5, 1.75), lwd = 1, labels = c(81.25, 81.5, 81.75))

segments(x0=0,y0= c(1.25, 1.5, 1.75), x1=3, y1=c(1.25, 1.5, 1.75), lty = 2,
         col = "darkgray")
par(las = 0)
mtext("Year", 1, line = -1.1, cex = 1.3, adj = 0.99)
mtext("Age", 3, line = -1.1, cex = 1.3, adj = 0.01)

text(x = 2.45, y=2.05, "Birth cohort 1938", cex = 1, srt = 45, pos = 4)
text(x = 1.45, y=2.05, "Birth cohort 1937", cex = 1, srt = 45, pos = 4)
dev.off()
