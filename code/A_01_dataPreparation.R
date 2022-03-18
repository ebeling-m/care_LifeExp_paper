# Preparation of inputs for matrix estimations
# Load packages and functions
source("code/A_00_packages.R")

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

# Cohort range: 1899 to 1950 
plot(table(dat$Coh), xlab = "Birth Cohort", ylab = "Number of individuals")
# There is a jumb in the number of person between birth cohorts 1941 and 1942

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

# Change data format
allStart <- as.Date("2015-01-01")

longDat <- function(exclude, id, birthDate, startdate, enddate, startstate, endstate, sex){
  
  year <- interval(start= allStart, end = c(startdate, enddate[length(enddate)])) / duration(num = 1, units = "years")
  age <- interval(start= birthDate[1], end = c(startdate, enddate[length(enddate)])) / duration(num = 1, units = "years")
  state <- c(startstate, endstate[length(endstate)])
  out <- data.frame(exclude = rep(exclude[1], length(year)), id = rep(id[1], length(year)), sex = rep(sex[1], length(year)), year = year, age = age, state = state)
  return(out)
}

dat6.a <- 
  dat5 %>% 
  group_by(lopnr) %>% 
  group_map(~longDat(exclude = .$exclude_lopnr, id = .$lopnr, birthDate = .$foddatn, startdate = .$startdate, enddate = .$enddate,
                     startstate = .$startstate, endstate = .$endstate, sex = .$male), .keep = TRUE) %>% 
  bind_rows() %>% 
  as_tibble()

# save(dat6.a, file = "dataAge_Format20210831.RData")
# load("dataAge_Format20210831.RData")
# quantile(dat5$s_e, probs = seq(0, 1, by =0.01))
# # Quarter of an age year seems to be the most appropriate time unit
# hist(dat5$s_e, breaks = 30)

# Edit data to fit discrete time unit
# Merge cohorts and Event to arrange data 
dat7 <- 
  dat5 %>% 
  select(lopnr, Event, Coh) %>%
  rename(id = lopnr) %>% 
  distinct() %>% 
  right_join(dat6.a)

# dim(dat7) 
# dim(dat6.a)
 
# save(dat7, file = "dataAge_Format20210831.RData")
# Function to create data records in half-year steps
test1 <- dat7 %>% filter(id == 3)
test2 <- dat7 %>% filter(id == 737)

id <- test2$id
sex <- test2$sex
age <- test2$age
state <- test2$state
Coh <- test2$Coh
Event <- test2$Event

half_year <- function(id, sex, age, state, Coh, Event, exclude){
  
  # Calculate ages the person could observed
  # Cut method with 0.01 to include also very close values 
  # might cause some misclassification
  ages <- seq(2015-Coh[1], 2020-Coh[1], by = 0.25)
  breaks <- c(ages[1]-3, ages-1e-07,ages[length(ages)]+3)
  agesIn <- as.numeric(as.character(
    cut(age, breaks, labels = 0:(length(breaks)-2), include.lowest = TRUE)))
  # To omit double transitions in on age group
  agesIn.C <- agesIn[duplicated(agesIn, fromLast = TRUE) == FALSE]
  statesIN <- state[duplicated(agesIn, fromLast = TRUE) == FALSE]
  
  # Assign Ages where we have transitions and states
  n <- length(ages)+2
  dat_out <- cbind(breaks, c(NA, ages, NA), rep(0, n), rep(0, n), 
                   rep(0, n), rep(0, n), rep(0, n), rep(NA, n))
  dat_out[(agesIn+1),3] <- dat_out[(agesIn+1),3]+1
  dat_out[(agesIn.C+1),5] <- statesIN
  dat_out[2:n,4] <- dat_out[1:(n-1),5]
  
  statesVec <- rep(NA, n*2)
  statesVec[seq(1, (n*2)-1, by = 2)] <- dat_out[,4]
  statesVec[seq(2, (n*2), by = 2)] <- dat_out[,5]
  
  for(i in 1:length(statesVec)){
    statesVec[i] <- ifelse(statesVec[i] == 0, statesVec[i-1], statesVec[i])
  }
  
  dat_out[,6] <- statesVec[seq(1, (n*2)-1, by = 2)]
  dat_out[,7] <- statesVec[seq(2, (n*2), by = 2)]
  
  # Add original ages
  dat_out[(agesIn.C+1),8] <- age[duplicated(agesIn, fromLast = TRUE) == FALSE]
  dat_outF <- dat_out[,c(2, 4:8)]
  colnames(dat_outF) <- c("AgeGr", "St_Mod", "St_Org", "Start", "End", "Age")
  # Make indicator for where we observed two transitions in one age group
  dTrans <- ifelse(TRUE %in% duplicated(agesIn, fromLast = TRUE), rep(1, n), rep(0, n))
  
  out <- data.frame(id = rep(id[1], n), sex = rep(sex[1], n), Coh = rep(Coh[1], n), Event = rep(Event[1], n), exclude = rep(exclude[1],n),
                    dTrans = dTrans, dat_outF)
  return(out)
  
}

dat <- dat1 <- dat2 <- dat3 <- dat4 <- dat6.a <- datRaw <- NULL

cutId <- as.integer(seq(1, length(unique(dat7$id)), length.out = 5))

firstCut <- which(dat7$id == unique(dat7$id)[cutId[2]])[2]
secondCut <- which(dat7$id == unique(dat7$id)[cutId[3]])[2]
thirdCut <- which(dat7$id == unique(dat7$id)[cutId[4]])[2]
lastCut <- dim(dat7)[1]

quantile(dat7$age, probs = seq(0, 1, by = 0.025))

idGroups <- cbind(c(1, firstCut+1, secondCut+1, thirdCut+1), 
                  c(firstCut, secondCut, thirdCut, lastCut))
# dat7[c(secondCut, secondCut+1),]
dat.dir <- "C:/careLifeExp/data" 
setwd(dat.dir)
labFile <- c("P1", "P2", "P3", "P4")
for(i in 1:4){
dat8 <- 
  dat7[idGroups[i,1]:idGroups[i,2],] %>% 
  group_by(id) %>% 
  group_map(~half_year(id = .$id, sex = .$sex, age = .$age, 
                       state = .$state, Coh = .$Coh, Event = .$Event,
                       exclude = .$exclude), 
            .keep = TRUE) %>% 
  bind_rows()
  
write.table(dat8, file = paste("discreteData_20210901_", labFile[i], ".txt", sep = ""), row.names = FALSE)
print(i)
}

