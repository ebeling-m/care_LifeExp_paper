# Preparation of inputs for matrix estimations
# Load packages and functions
wd.dir <- "C:/careLifeExp/careLE/aggregate_care"
setwd(wd.dir)
source("00_packages.R")

# Load datafiles
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)

dat1 <- read_delim("data_20210829_part1.csv", delim = ",")
dat2 <- read_delim("data_20210829_part2.csv", delim = ",")
dat3 <- read_delim("data_20210829_part3.csv", delim = ",")


datRaw <- bind_rows(dat1, dat2, dat3)

# Reformat dates and Event Variable
eventRe <- cbind(c("21: Deceased, Ok first gap in TPR", "50: Last date of follow-up", "20: Deceased",                            
                   "31: Emigrated, Ok first gap in TPR", "40: First gap in TPR, \"Emigrated/Lost\"", "30: Emigrated" ),
                 c(21, 50, 20, 31, 40, 30))


datRaw <- datRaw %>% mutate(deathdate = ifelse(deathdate == "", NA, deathdate))

dat <- 
  datRaw %>%
  mutate(startdate = as.Date(dateRe(startdate)),
         enddate = as.Date(dateRe(enddate)),
         x_lastdate = as.Date(dateRe(x_lastdate)),
         foddatn = as.Date(dateRe(foddatn)),
         deathdate = as.Date(dateRe(deathdate)),
         Event = mapvalues(event, 
                           from = eventRe[,1],
                           to = eventRe[,2]),
         Coh = as.numeric(substr(foddatn, 1,4)))

# Keep only persons that are not censored due to emigration (Event == 30,31,40)
dat1 <- 
  dat %>% 
  filter(Event %in% c(20, 21, 50))

length(unique(dat$lopnr))-length(unique(dat1$lopnr))

# 4772 individuals have been excluded due to emigration

# mark individuals that are not observed since 2015-01-01
# dateCheck <- function(startdate, id){
#   
#   X2015 <- if_else(any(startdate %in% "2015-01-01"), 
#                    1, 
#                    0)
#   out <- data.frame(X2015 = X2015, id = id[1])
#   return(out)
# }
# 
# 
# startDate <- 
#   dat1 %>% 
#   select(lopnr, startdate) %>%
#   group_by(lopnr) %>% 
#   group_map(~dateCheck(startdate = .$startdate, id = .$lopnr), 
#             .keep = TRUE) %>% 
#   bind_rows() %>% as_tibble() %>% rename(lopnr = id)
# 
# dat1 <- 
#   dat1 %>%   
#   left_join(startDate)
# 
# # All individuals are observed since 2015-01-01
# table(dat1$X2015)

# Check plausibility of start and enddate (e.g. death and transition in some month)
dat1 <- 
  dat1 %>%
  mutate(s_e = interval(start= startdate, end = enddate) / duration(num = 1, units = "years"))

# There are individuals that have a death date before start date 

dat2 <- 
  dat1 %>%
  mutate(deathYear = as.numeric(substr(deathdate, 1,4))) %>% 
  filter(deathYear >=2015 | is.na(deathYear))

# 30 individuals have been excluded due to irregular deathdate startdate combi
length(unique(dat2$lopnr))-length(unique(dat1$lopnr))


# Take out episodes that negative process time
dat3 <- 
  dat2 %>% 
  filter(s_e >= 0)

# No individuals lost due to this edits
length(unique(dat3$lopnr))-length(unique(dat2$lopnr))

# There are also individuals that have a death date but no transition to death 
# Moreover some individuals made transitions at the least day of follow up
# 2020-12-31, For these individuals a second record exist with start and end equal 
# to 2020-12-31
 # dat3 %>% filter(lopnr == 28598)
# test <- doubDeath %>% group_by(lopnr) %>% distinct(enddate, .keep_all = TRUE )
# test2 <- dat3 %>% group_by(lopnr) %>% count(enddate) %>% filter(n > 1)
# dat3 %>% filter(lopnr == 11961)
# distinct(enddate, .keep_all = TRUE )

# Solution select by individual only records with distinct enddate 
# (function takes first record. so if ordered chronologically takes the one we need)
dat4 <- dat3  %>% group_by(lopnr) %>% distinct(enddate, .keep_all = TRUE )

# No individuals lost due to this edits
length(unique(dat4$lopnr))-length(unique(dat3$lopnr))

# Edit now also cases where death as endstate is not recorded
doubDeath <- dat4 %>% 
  filter(enddate == deathdate & !is.na(deathdate) & !endstate == 4)

dat5 <- 
  dat4 %>%
  rowwise() %>% 
  mutate(endstate = ifelse(enddate == deathdate & 
                                !is.na(deathdate) & 
                                !endstate == 4,4, endstate)) %>% 
  group_by(lopnr) %>% 
  mutate(minStart = min(startdate))

# test <- dat5 %>% distinct(lopnr, minStart)
# table(test$minStart)
# 951 individuals are not observed since 2015-01-01
# Take out

dat5 <- dat5 %>% filter(minStart == "2015-01-01")


dat6 <- 
  dat5 %>% 
  select(lopnr, male, foddatn, minStart, x_lastdate, firsthomecare, 
         firstsarbo, deathdate, Event, Coh, startstate, startdate, exclude_lopnr) %>% 
  rowwise() %>% 
  mutate(firsthomecare = as.Date(dateRe(firsthomecare)),
         firstsarbo = as.Date(dateRe(firstsarbo)))

dat7 <- 
  dat6 %>% 
  filter(minStart == startdate)

# max(dat7$deathdate, na.rm = TRUE)
# dat7 %>% filter(deathdate == "2021-12-31")
# datRaw %>% filter(lopnr == 463546)
# 
# test

# Create dataset that will be merged with long format data 
dat_merge <- 
  dat7 %>% 
  mutate(lifespan = interval(start = foddatn, end = x_lastdate) / duration(num = 1, units = "years"),
         deathYear = as.numeric(substr(deathdate, 1, 4)), 
         deathMonth = as.numeric(substr(deathdate, 6, 7))) %>% 
  select(lopnr, male, lifespan, Event, exclude_lopnr, deathYear, deathMonth)

setwd(dat.dir)
save(dat_merge, file = "data_merge_longCare.RData")

# 
#   distinct(lopnr, .keep_all = TRUE)

# x <- dat7[8,]

byMonthLong <- function(x){
  
  month_seq <- as.Date(seq(x$minStart, x$x_lastdate, "months"))
  py <- interval(start = month_seq, end = c(month_seq[-1], x$x_lastdate)) / duration(num = 1, units = "years")
  year <- as.numeric(substr(month_seq, 1, 4))
  month <- as.numeric(substr(month_seq, 6, 7))
  age <- year-x$Coh
  t_p <- 1:length(month_seq)
  care <- rep(x$startstate, length(t_p))
  care[which(month_seq > as.Date(x$firsthomecare))] <- 2
  care[which(month_seq > as.Date(x$firstsarbo))] <- 3
  
  out <- tibble(py, year, month, age, t_p, care, lopnr = x$lopnr)
  return(out)
}
  

lopnr <- unique(dat7$lopnr)
cutLop <- seq(1, length(lopnr), length.out = 11)
lopBin <- cbind(cutLop[-length(cutLop)], cutLop[-1])
lopBin[2:10,1] <- lopBin[2:10,1]+1
namesLop <- paste("part", 1:10, sep = "_")

for(i in 1:10){
  lop.sel <- lopnr[lopBin[i,1]:lopBin[i,2]]
  dat.sel <- 
    dat7 %>%
    filter(lopnr %in% lop.sel) %>% 
    group_by(lopnr) %>% 
    group_map(~byMonthLong(x=.), .keep = TRUE) %>% 
    bind_rows()
  save(dat.sel, file = paste(namesLop[i], "_personYearsMonth.RData", sep = ""))
  print(i)
}





