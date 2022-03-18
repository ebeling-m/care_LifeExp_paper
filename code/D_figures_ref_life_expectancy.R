# Main article figures 2
# Plot ref LE 
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

# Plot for LE differences overtime 
# Plot at ages 70, 80, 90

le_trend <- 
  remain_LE %>%
  filter(YearGr == 18) %>% 
  ungroup() %>% 
  filter(AgeGr %in% c(70, 80, 90, 100)) %>% 
  mutate(color.y = mapvalues(care, 
                             from = c("NC", "HC", "CH"), 
                             to = c('#41b6c4','#2c7fb8','#253494')),
         y1= as.numeric(mapvalues(care, 
                                  from = c("NC", "HC", "CH"), 
                                  to = rev(c(0.1, 0.3, 0.5)))),
         y2= as.numeric(mapvalues(care, 
                                  from = c("NC", "HC", "CH"), 
                                  to = rev(c(0.1, 0.3, 0.5))+0.15))) %>% 
  mutate(y1 = case_when(AgeGr == 100 ~ y1 + 0,
                        AgeGr == 90 ~ y1 + 1,
                        AgeGr == 80 ~ y1 + 2,
                        AgeGr == 70 ~ y1 + 3),
         y2 = case_when(AgeGr == 100 ~ y2 + 0,
                        AgeGr == 90 ~ y2 + 1,
                        AgeGr == 80 ~ y2 + 2,
                        AgeGr == 70 ~ y2 + 3))

pdf("figures/le_reference.pdf", family = "Times", width = 14, 
    pointsize = 12)
par(mar=c(5,3,2,1), xpd = TRUE)
layout(rbind(c(1,2), c(3,3)), height = c(0.9, 0.1))

plot(x=0, y=0, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
     xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
segments(x0=seq(0, 20, by = 2), y0=0, x1=seq(0, 20, by = 2), y1=4, 
         col = "lightgray", lwd = 2.5)
segments(x0=seq(1, 19, by = 2), y0=0, x1=seq(1, 19, by = 2), y1=4, 
         col = "lightgray", lwd = 0.5)

axis(1, at = seq(0, 20, by = 2), lwd = 3, cex.axis = 1.3)
axis(1, at = seq(1, 19, by = 2), lwd = 1, labels = FALSE)

axis(2, at = c(0.5, 1.5, 2.5, 3.5), 
     labels = c("Age 100", "Age 90", "Age 80", "Age 70"), 
     lwd = 0, cex.axis = 1.3)

le_trend %>% 
  filter(sex == 0) %>%
  group_by(care, AgeGr) %>% 
  group_map(~polygon(x=c(0, rep(.$le_med_r,2), 0),y=c(.$y1,.$y1, .$y2, .$y2 ), col=.$color.y, 
                   border = "white"), .keep = TRUE)

le_HMD %>% 
  filter(sex == 0 & Age %in% c(70, 80, 90, 100) & Year == 2018) %>% 
  mutate(color.y = '#7fcdbb',
         y1= 0.7,
         y2= 0.7+0.15) %>%
  mutate(y1 = case_when(Age == 100 ~ y1 + 0,
                        Age == 90 ~ y1 + 1,
                        Age == 80 ~ y1 + 2,
                        Age == 70 ~ y1 + 3),
         y2 = case_when(Age == 100 ~ y2 + 0,
                        Age == 90 ~ y2 + 1,
                        Age == 80 ~ y2 + 2,
                        Age == 70 ~ y2 + 3)) %>% 
  group_by(Age) %>% 
  group_map(~polygon(x=c(0, rep(.$ex,2), 0),y=c(.$y1,.$y1, .$y2, .$y2 ), col=.$color.y, 
                     border = "white"), .keep = TRUE)

mtext("Women", 3, line = 0.3, cex = 1.3)
mtext("Remaining life expectancy", 1, line = 2.5, cex = 1.2)

####################################################
# Males
plot(x=0, y=0, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
     xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
segments(x0=seq(0, 20, by = 2), y0=0, x1=seq(0, 20, by = 2), y1=4, 
         col = "lightgray", lwd = 2.5)
segments(x0=seq(1, 19, by = 2), y0=0, x1=seq(1, 19, by = 2), y1=4, 
         col = "lightgray", lwd = 0.5)

axis(1, at = seq(0, 20, by = 2), lwd = 3, cex.axis = 1.3)
axis(1, at = seq(1, 19, by = 2), lwd = 1, labels = FALSE)

axis(2, at = c(0.5, 1.5, 2.5, 3.5), 
     labels = c("Age 100", "Age 90", "Age 80", "Age 70"), 
     lwd = 0, cex.axis = 1.3)

le_trend %>% 
  filter(sex == 1) %>%
  group_by(care, AgeGr) %>% 
  group_map(~polygon(x=c(0, rep(.$le_med_r,2), 0),y=c(.$y1,.$y1, .$y2, .$y2 ), col=.$color.y, 
                     border = "white"), .keep = TRUE)

le_HMD %>% 
  filter(sex == 1 & Age %in% c(70, 80, 90, 100) & Year == 2018) %>% 
  mutate(color.y = '#7fcdbb',
         y1= 0.7,
         y2= 0.7+0.15) %>%
  mutate(y1 = case_when(Age == 100 ~ y1 + 0,
                        Age == 90 ~ y1 + 1,
                        Age == 80 ~ y1 + 2,
                        Age == 70 ~ y1 + 3),
         y2 = case_when(Age == 100 ~ y2 + 0,
                        Age == 90 ~ y2 + 1,
                        Age == 80 ~ y2 + 2,
                        Age == 70 ~ y2 + 3)) %>% 
  group_by(Age) %>% 
  group_map(~polygon(x=c(0, rep(.$ex,2), 0),y=c(.$y1,.$y1, .$y2, .$y2 ), col=.$color.y, 
                     border = "white"), .keep = TRUE)

mtext("Men", 3, line = 0.3, cex = 1.3)
mtext("Remaining life expectancy", 1, line = 2.5, cex = 1.2)

# Legend
par(mar = c(0,0,0,0))
plot(x=0, y=0, typ = "n", 
     xaxs = "i", yaxs = "i", bty = "n", ylim = c(0, 4),
     xlim=c(0,20), ylab = NA, xlab = NA, xaxt = "n", yaxt = "n")
legend("center", legend = c("Total", "No care", "Home care", "Care home"), 
       col = c('#7fcdbb','#41b6c4','#2c7fb8','#253494'), pch = 16, bty = "n", horiz = TRUE, cex = 1.7)

dev.off()
