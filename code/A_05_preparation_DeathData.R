# Prepare death data
# Load packages and functions
source("code/A_00_packages.R")

# Load datafiles
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)

var_types <- c("n", "n", "n", "n", "c", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", 
               "n", "n", "c")

dat <- read_delim("oneYearPD_data_20210914.txt", delim = " ", 
                    col_types = as.list(var_types))

ageBreaks <- c(seq(70, 105, by = 0.25), 120) 
labels <- seq(70, 105, by = 0.25)

codCode <- cbind(c("U", "A", "B", "C", "D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E", "F", "G",
                   "H0", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "I", "J", "K", "L", "M",
                   "N", "O", "P", "Q", "R", "V", "W", "X", "Y"),
                 c(1, 2, 2, rep(3, 6), rep(4, 4), 5:7, rep(8, 6), rep(9, 4), 10:19, rep(20, 4)))

dat <- 
  dat %>% 
  dplyr::select(LopNr, sex, care0, Age, CoD, DeathYear) %>% 
  mutate(chapter = ifelse(substr(CoD, 1, 1) %in% c("D", "H"),
                      substr(CoD, 1, 2), substr(CoD, 1, 1)),
         CodGr = mapvalues(chapter, from = codCode[,1], to = codCode[,2]),
         Sex=ifelse(sex == 2, 0, 1),
         Care = mapvalues(care0, from = 1:3, to = c("NC", "HC", "CH")),
         AgeGr = as.numeric(as.character(cut(Age, breaks = ageBreaks, 
                                 labels = labels, include.lowest = TRUE)))) %>% 
  filter(Age >= 70)

# 3678 don't have a care status
# missCare <- dat %>% filter(is.na(Care))
# dat <- 
#   dat %>% 
#   mutate(Care = ifelse(is.na(Care), "NC", Care))
# 
# update care status 

miss_care <- read_delim("missinglopnr_status.csv", delim = ",")
table(miss_care$status)
miss_care <- 
  miss_care %>% 
  mutate(Care = mapvalues(status, 
                          from = c("nocare", "homecare", "carehome"), 
                          to = c("NC", "HC", "CH"))) %>% 
  dplyr::select(lopnr, Care) %>% 
  rename(LopNr = lopnr, Care_n = Care)

dat <- 
  dat %>%
  left_join(miss_care) %>% 
  mutate(Care = ifelse(!is.na(Care_n), Care_n, Care))

# Use 100+ as group

dat_agg <- 
  dat %>%
  mutate(AgeGr = ifelse(AgeGr > 100, 100, AgeGr)) %>% 
  count(DeathYear, Sex, Care, AgeGr, CodGr) %>% 
  rename(Dx = n, Year = DeathYear)

# save(dat, file = "deaths_15to20_above70.RData")
# Load Cause of death data for ages below 70 

setwd("C:/careLifeExp/care_LifeExp_paper")
cod <- read_delim("data_input/cod_Sweden_20152020.csv", delim = ";", skip = 1, 
                  col_types = cols("n", "c", "c", "n", "n", "n", "n", "n", "n", "n", 
                                   "n", "n", "n", "n", "n", "n", "n", "n", "n", "n",
                                   "n", "n", "n", "n"))
cod1 <- 
  cod %>% 
  pivot_longer(cols = names(cod)[4:23], names_to = "Age", values_to = "Deaths")

ageRecode <- cbind(unique(cod1$Age), seq(0, 95, by = 5)) 
sexRecode <- cbind(unique(cod1$Kön), c(1,0)) 

cod2 <- 
  cod1 %>% 
  mutate(AgeGr = as.numeric(mapvalues(Age, from = ageRecode[,1], to = ageRecode[,2])),
         Sex = as.numeric(mapvalues(Kön, from = sexRecode[,1], to = sexRecode[,2])),
         chapter = ifelse(substr(Diagnos, 1, 1) %in% c("D", "H"),
                          substr(Diagnos, 1, 2), substr(Diagnos, 1, 1)),
         CodGr = mapvalues(chapter, from = codCode[,1], to = codCode[,2]),
         Dx = ifelse(is.na(Deaths), 0, Deaths), 
         Care = "NC") %>% 
  rename(Year = År) %>%
  filter(Age < 70) %>% 
  dplyr::select(Year, AgeGr, CodGr, Care, Sex, Dx)

death_dat <- bind_rows(cod2, dat_agg)
save(death_dat, file = "data_inter/deaths_2015_to_2020.RData")
# tail(death_dat)

# # Create Plot for distribution of Covid deaths
# 
# c19 <- 
#   death_dat %>%
#   filter(CodGr == 1 & Year == 2020) %>% 
#   mutate(Age = as.integer(AgeGr)) %>% 
#   group_by(Age, Care, Sex) %>% 
#   summarise(Dx = sum(Dx))
# 
# colCare <- c(rgb(254/255,217/255,142/255, alpha = 0.4),
#              rgb(254/255,153/255,41/255, alpha = 0.4),
#              rgb(204/255,76/255,2/255, alpha = 0.4))
# 
# pdf("dist_c19_deaths_males.pdf", family = "Times")
# plot(x=0:100, y=0:100, typ = "n", bty = "n", xaxt = "n", yaxt = "n",
#      xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, ylim = c(0,300))
# 
# segments(x0=0, y0=seq(0, 300, by = 50), x1=100, y1=seq(0, 300, by = 50),
#          col = "lightgray")
# 
# nc <- 
#   c19 %>% 
#   filter(Sex == 1 & Care == "NC") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# hc <- 
#   c19 %>% 
#   filter(Sex == 1 & Care == "HC") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# ch <- 
#   c19 %>% 
#   filter(Sex == 1 & Care == "CH") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# for(i in unique(c19$Age)){
#   if(i %in% c(99, 100)){
#     xx <- c(rep(i,2), rep(i+1,2))
#     yy <- c(0, rep(0,2), 0)
#     polygon(x=xx, y=yy, col = colCare[1], border = "white")
#   }else{
#   nc.plot <- 
#     nc %>% 
#     filter(Age == i) %>% 
#     mutate(Age2 = Age + n)
#   xx <- c(rep(nc.plot$Age,2), rep(nc.plot$Age2,2))
#   yy <- c(0, rep(nc.plot$Dx,2), 0)
#   polygon(x=xx, y=yy, col = colCare[1], border = "white")
#   }
# if(i >= 70){
#   hc.plot <- 
#     hc %>% 
#     filter(Age == i) 
#   yy <- c(nc.plot$Dx, rep(hc.plot$Dx+nc.plot$Dx,2), nc.plot$Dx)
#   polygon(x=xx, y=yy, col = colCare[2], border = "white")
#   
#   ch.plot <- 
#     ch %>% 
#     filter(Age == i) 
#   yy <- c(hc.plot$Dx+nc.plot$Dx, 
#           rep(hc.plot$Dx+nc.plot$Dx+ch.plot$Dx,2),
#           hc.plot$Dx+nc.plot$Dx)
#   polygon(x=xx, y=yy, col = colCare[3], border = "white")
# }
# }
# par(las = 0)
# mtext("Age", 1, line = 2.8, cex = 1.3)
# mtext("Number of Deaths", 2, line = 2.8, cex = 1.3)
# legend("topleft", legend = c("No care", "Home care", "Care home"), 
#        pch = 15, col = colCare, bty = "n", cex = 1.2)
# par(las = 2)
# axis(1, at = seq(0, 100, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# axis(2, at = seq(0, 300, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
# 
# dev.off()
# 
# 
# pdf("dist_c19_deaths_females.pdf", family = "Times")
# plot(x=0:100, y=0:100, typ = "n", bty = "n", xaxt = "n", yaxt = "n",
#      xaxs = "i", yaxs = "i", xlab = NA, ylab = NA, ylim = c(0,300))
# 
# segments(x0=0, y0=seq(0, 300, by = 50), x1=100, y1=seq(0, 300, by = 50),
#          col = "lightgray")
# 
# nc <- 
#   c19 %>% 
#   filter(Sex == 0 & Care == "NC") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# hc <- 
#   c19 %>% 
#   filter(Sex == 0 & Care == "HC") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# ch <- 
#   c19 %>% 
#   filter(Sex == 0 & Care == "CH") %>% 
#   mutate(n = ifelse(Age < 70, 5, 1))
# 
# for(i in unique(c19$Age)){
#   nc.plot <- 
#     nc %>% 
#     filter(Age == i) %>% 
#     mutate(Age2 = Age + n)
#   if(length(nc.plot$Dx) == 0){
#     xx <- c(rep(i,2), rep(i+1,2))
#     yy <- c(0, rep(0,2), 0)
#     polygon(x=xx, y=yy, col = colCare[1], border = "white")
#   }else{
#     xx <- c(rep(nc.plot$Age,2), rep(nc.plot$Age2,2))
#     yy <- c(0, rep(nc.plot$Dx,2), 0)
#     polygon(x=xx, y=yy, col = colCare[1], border = "white")
#   }
#   if(i >= 70){
#     hc.plot <- 
#       hc %>% 
#       filter(Age == i) 
#     yy <- c(yy[2], rep(yy[2]+hc.plot$Dx,2), yy[2])
#     polygon(x=xx, y=yy, col = colCare[2], border = "white")
#     
#     ch.plot <- 
#       ch %>% 
#       filter(Age == i) 
#     yy <- c(yy[2], 
#             rep(yy[2]+ch.plot$Dx,2),
#             yy[2])
#     polygon(x=xx, y=yy, col = colCare[3], border = "white")
#   }
# }
# par(las = 0)
# mtext("Age", 1, line = 2.8, cex = 1.3)
# mtext("Number of Deaths", 2, line = 2.8, cex = 1.3)
# legend("topleft", legend = c("No care", "Home care", "Care home"), 
#        pch = 15, col = colCare, bty = "n", cex = 1.2)
# par(las = 2)
# axis(1, at = seq(0, 100, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# axis(2, at = seq(0, 300, by = 50), labels = TRUE, lwd = 3, cex.axis = 1.3)
# 
# dev.off()
# 
