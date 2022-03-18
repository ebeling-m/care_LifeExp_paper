# Life expectancy over years
# Fit aggregated data

# Load data
# Load packages and functions
source("code/A_00_packages.R")
getwd()

# Load datafiles
dat_all <- read_delim("data_inter/aggreagted_data_transitions.txt", delim = " ")

# All data
dat1_pop_all <- 
  dat_all %>%
  distinct(sex, AgeGr, Start, YearGr, exclude, .keep_all = TRUE) %>% 
  group_by(sex, AgeGr, Start, YearGr) %>% 
  summarise(Tpop = sum(TotalPop, na.rm = TRUE)) %>% 
  ungroup()

dat1_cases_all <- 
  dat_all %>%
  group_by(sex, AgeGr, Start, End, YearGr) %>% 
  summarise(Cases = sum(n, na.rm = TRUE)) %>% 
  ungroup()

ref.dat <- 
  expand_grid(AgeGr = seq(70, 104.75, by = 0.25), Start = as.numeric(1:4), 
              End = as.numeric(1:4), sex = 0:1, YearGr = 18) %>% 
  mutate(comb_t = paste(Start, End, sep = "-")) %>% 
  filter(comb_t %in% c("1-1", "1-2", "1-3", "1-4",
                       "2-2", "2-3", "2-4", 
                       "3-3", "3-4")) %>% 
  dplyr::select(AgeGr, Start, End, sex, YearGr)

dat1_all <- 
  ref.dat %>% 
  left_join(dat1_cases_all) %>%
  left_join(dat1_pop_all) %>% 
  arrange(sex, YearGr, Start, AgeGr) %>% 
  mutate(weight_c= ifelse(is.na(Cases), 0, 1),
         Cases = ifelse(is.na(Cases), 0, Cases),
         Tpop = ifelse(is.na(Tpop), 0.00001, Tpop)) %>% 
  mutate(p = Cases/Tpop)

# Smooth and prediction function 
gam_smooth <- function(x, iteration = 1000, numC = 10){
  
  # Model input
  cases <- x$Cases
  Age <- x$AgeGr
  p <- x$p
  pop <- x$Tpop
  w_obs <- x$weight_c
  fullAge <- seq(70, 104.75, by = 0.25)
  # misAge <- fullAge[!fullAge %in% Age]
  
  
  # Register Workers
  registerDoParallel(cores=numC)
  

  fits.store <- foreach(i = 1:iteration, 
                        .combine = "cbind", 
                        .packages = c("mgcv", "dplyr", "magrittr")) %dopar% {
  # Fit model transitions
  mod2 <- gam(rpois(length(cases), lambda = cases) ~ s(Age, bs="ps") + offset(log(pop)), 
              family = poisson(), weights = w_obs)

  # # Fit population to fill gaps in population counts
  # pop.mod <- gam(log(pop) ~ s(Age, bs="ps"))
  # pred.pop <- exp(predict(pop.mod, newdata = data.frame(Age=misAge)))

  # Prediction dat
  # otherVar <- x %>% dplyr::select(sex, Start, End, YearGr) %>% distinct()
  # misPop <- tibble(otherVar, AgeGr = misAge, Tpop = pred.pop, Cases = NA, p = NA)
  pred.dat <- 
    x %>% 
    # bind_rows(misPop) %>% 
    arrange(AgeGr) %>%
    mutate(pCase = exp(predict(mod2, newdata = data.frame(Age = AgeGr, pop = Tpop))))
  pred.dat$pCase
  }
  
  closeAllConnections()

  quant.fits <- apply(fits.store, MARGIN = 1, FUN = quantile, 
                      probs = c(0.05, 0.5, 0.95), na.rm = TRUE)

  NA.fit <- length(table(is.na(quant.fits)))
  
  # # Fit population to fill gaps in population counts
  # pop.mod <- gam(log(pop) ~ s(Age, bs="ps"))
  # pred.pop <- exp(predict(pop.mod, newdata = data.frame(Age=misAge)))
  
  
  # otherVar <- x %>% dplyr::select(sex, Start, End, YearGr) %>% distinct()
  # misPop <- tibble(otherVar, AgeGr = misAge, Tpop = pred.pop, Cases = NA, p = NA)
  
  pred.dat <- 
    x %>% 
    # bind_rows(misPop) %>% 
    arrange(AgeGr) %>%
    mutate(pCase = quant.fits["50%",],
           low = quant.fits["5%",],
           up = quant.fits["95%",],
           NA.fit = NA.fit)
  
  
  
  return(pred.dat)
}

# Fit the data

fit.dat_all <- 
  dat1_all %>% 
  group_by(sex, Start, End, YearGr) %>% 
  group_map(~gam_smooth(x=.), .keep = TRUE) %>% 
  bind_rows()

# Calculate fitted probabilities
fit.dat_all %<>% mutate(fitp_med = pCase/Tpop,
                    fitp_low = low/Tpop,
                    fitp_up = up/Tpop)


# save(fit.dat_exl, file = "fitted_transitionP_1521_exl.RData")
save(fit.dat_all, file = "data_inter/fitted_transitionP_1521_all.RData")

# Calculate probability to stay as the residual of the other transitions

fitStay_all <- 
  fit.dat_all %>% 
  filter(Start != End) %>% 
  group_by(sex, AgeGr, Start, YearGr) %>% 
  summarise(fitStay_med = 1-sum(as.vector(fitp_med)),
            fitStay_low = 1-sum(as.vector(fitp_low)), 
            fitStay_up = 1-sum(as.vector(fitp_up))) %>% 
  mutate(End = Start)

fitfinal_all <- 
  fit.dat_all %>% 
  left_join(fitStay_all) %>% 
  arrange(YearGr, AgeGr, Start, End) %>% 
  mutate(finalP_med = ifelse(is.na(fitStay_med), fitp_med, fitStay_med),
         finalP_low = ifelse(is.na(fitStay_low), fitp_low, fitStay_low),
         finalP_up = ifelse(is.na(fitStay_up), fitp_up, fitStay_up))

save(fitfinal_all, file = "data_inter/aggregtedData_fittedProb_all.RData")


# colT <- c('#1b9e77','#d95f02','#7570b3')
# colTa <- adjustcolor(colT, alpha.f = 0.2) 
# 
# lineFct <- function(x){
#   xx <- c(x$AgeGr, rev(x$AgeGr))
#   yy <- c(x$finalP_low, rev(x$finalP_up))
#   polygon(x=xx, y=yy, col = x$tCol, border = x$tCol)
#   lines(x=x$AgeGr, y=x$p, typ = "p", pch = 16, col = x$tCol, cex = 0.8)
#   lines(x=x$AgeGr, y=x$p, typ = "l", lwd = 1, col = x$col)
#   lines(x=x$AgeGr, y=x$finalP_med, typ = "l", lwd = 3, col = x$col)
# }
# 
# 
# # Make plot with transition probabilities
# 
# pdf("figures/transitionProb_2015to2020_females.pdf", family = "Times",
#     width = 20, height = 20, pointsize = 12)
# 
# layout(matrix(1:20, ncol = 4, byrow = TRUE))
# 
# 
# for(i in 15:19){
# # Transition from No care
#   plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#      ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#      yaxs = "i")
# axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# par(las = 0)
# axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#      labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, cex.axis = 1.3)
# 
# fitfinal_all %>% 
#   filter(Start == 1 & Start != End & sex == 0 & YearGr == i) %>% 
#   mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#          col = mapvalues(End, from = 2:4, to = colT)) %>% 
#   group_by(End, YearGr) %>%
#   group_map(~lineFct(x=.), .keep = TRUE)
# 
# if(i == 15){
# legend("bottomright", legend = c("To Home Care", "To Care Home", "To Death"),
#        col = colT, pch = 15, bty = "n")
# mtext("From No Care", 3, line = 1)
# }
# 
# mtext(paste("20",i, "-20", i+1, sep = ""), 3, line = 1, adj = 0)
# 
# if(i == 17){
# mtext("Transition probability (log - scale)", 2, line = 2.5)
# }
# 
# if(i ==19){
# mtext("Age", 1, line = 2.5)
# }
# # Home care
# 
# plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#      ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#      yaxs = "i")
# axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# par(las = 0)
# axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#      labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, 
#      cex.axis = 1.3)
# 
# fitfinal_all %>% 
#   filter(Start == 2 & Start != End & sex == 0 & YearGr == i) %>% 
#   mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#          col = mapvalues(End, from = 2:4, to = colT)) %>% 
#   group_by(End, YearGr) %>%
#   group_map(~lineFct(x=.), .keep = TRUE)
# 
# if(i == 15){
#   legend("bottomright", legend = c("To Care Home", "To Death"),
#        col = colT[2:3], pch = 15, bty = "n")
# 
#   mtext("From Home Care", 3, line = 1)
# }
# 
# if(i ==19){
#   mtext("Age", 1, line = 2.5)
# }
# # Care Home
# 
# plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#      ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#      yaxs = "i")
# axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# par(las = 0)
# axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#      labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, 
#      cex.axis = 1.3)
# 
# fitfinal_all %>% 
#   filter(Start == 3 & Start != End & sex == 0 & YearGr == i) %>% 
#   mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#          col = mapvalues(End, from = 2:4, to = colT)) %>% 
#   group_by(End, YearGr) %>%
#   group_map(~lineFct(x=.), .keep = TRUE)
# 
# if(i == 15){
#   legend("bottomright", legend = c("To Death"),
#        col = colT[3], pch = 15, bty = "n")
#   mtext("From Care Home", 3, line = 1)
# }
# 
# if(i ==19){
#   mtext("Age", 1, line = 2.5)
# }
# 
# # Staying
# plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#      ylim = c(0.75, 1), xaxt = "n", yaxt = "n", xaxs = "i",
#      yaxs = "i")
# axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
# par(las = 0)
# axis(2, at = seq(0.75, 1, by = 0.05), 
#      labels = TRUE, lwd = 3, cex.axis = 1.3)
# 
# fitfinal_all %>% 
#   filter(Start == End & sex == 0 & Start != 4 & YearGr == i) %>% 
#   mutate(tCol = mapvalues(End, from = 1:3, to = colTa),
#          col = mapvalues(End, from = 1:3, to = colT)) %>% 
#   group_by(Start, YearGr) %>%
#   group_map(~lineFct(x=.), .keep = TRUE)
# 
# if(i == 15){
#   legend("bottomleft", legend = c("In No Care", "In Home Care", "In Care Home"),
#        col = colT[1:3], pch = 15, bty = "n")
#   mtext("Staying in State", 3, line = 1)
# }
# 
# if(i ==19){
#   mtext("Age", 1, line = 2.5)
# }}
# dev.off()
# 
# #######################################
# # Males
# 
# # Make plot with transition probabilities
# 
# pdf("figures/transitionProb_2015to2020_males.pdf", family = "Times",
#     width = 20, height = 20, pointsize = 12)
# 
# layout(matrix(1:20, ncol = 4, byrow = TRUE))
# 
# 
# for(i in 15:19){
#   # Transition from No care
#   plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#        ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#        yaxs = "i")
#   axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
#   par(las = 0)
#   axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#        labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, cex.axis = 1.3)
#   
#   fitfinal_all %>% 
#     filter(Start == 1 & Start != End & sex == 1 & YearGr == i) %>% 
#     mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#            col = mapvalues(End, from = 2:4, to = colT)) %>% 
#     group_by(End, YearGr) %>%
#     group_map(~lineFct(x=.), .keep = TRUE)
#   
#   if(i == 15){
#     legend("bottomright", legend = c("To Home Care", "To Care Home", "To Death"),
#            col = colT, pch = 15, bty = "n")
#     mtext("From No Care", 3, line = 1)
#   }
#   
#   mtext(paste("20",i, "-20", i+1, sep = ""), 3, line = 1, adj = 0)
#   
#   if(i == 17){
#     mtext("Transition probability (log - scale)", 2, line = 2.5)
#   }
#   
#   if(i ==19){
#     mtext("Age", 1, line = 2.5)
#   }
#   # Home care
#   
#   plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#        ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#        yaxs = "i")
#   axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
#   par(las = 0)
#   axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#        labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, 
#        cex.axis = 1.3)
#   
#   fitfinal_all %>% 
#     filter(Start == 2 & Start != End & sex == 1 & YearGr == i) %>% 
#     mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#            col = mapvalues(End, from = 2:4, to = colT)) %>% 
#     group_by(End, YearGr) %>%
#     group_map(~lineFct(x=.), .keep = TRUE)
#   
#   if(i == 15){
#     legend("bottomright", legend = c("To Care Home", "To Death"),
#            col = colT[2:3], pch = 15, bty = "n")
#     
#     mtext("From Home Care", 3, line = 1)
#   }
#   
#   if(i ==19){
#     mtext("Age", 1, line = 2.5)
#   }
#   # Care Home
#   
#   plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#        ylim = c(0.0001, 0.4), log = "y", xaxt = "n", yaxt = "n", xaxs = "i",
#        yaxs = "i")
#   axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
#   par(las = 0)
#   axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4), 
#        labels = paste(c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.4)), lwd = 3, 
#        cex.axis = 1.3)
#   
#   fitfinal_all %>% 
#     filter(Start == 3 & Start != End & sex == 1 & YearGr == i) %>% 
#     mutate(tCol = mapvalues(End, from = 2:4, to = colTa),
#            col = mapvalues(End, from = 2:4, to = colT)) %>% 
#     group_by(End, YearGr) %>%
#     group_map(~lineFct(x=.), .keep = TRUE)
#   
#   if(i == 15){
#     legend("bottomright", legend = c("To Death"),
#            col = colT[3], pch = 15, bty = "n")
#     mtext("From Care Home", 3, line = 1)
#   }
#   
#   if(i ==19){
#     mtext("Age", 1, line = 2.5)
#   }
#   
#   # Staying
#   plot(x=70:105, y=70:105, typ = "n", bty = "n", xlab = NA, ylab = NA, 
#        ylim = c(0.75, 1), xaxt = "n", yaxt = "n", xaxs = "i",
#        yaxs = "i")
#   axis(1, at = seq(70, 105, by = 5), labels = TRUE, lwd = 3, cex.axis = 1.3)
#   par(las = 0)
#   axis(2, at = seq(0.75, 1, by = 0.05), 
#        labels = TRUE, lwd = 3, cex.axis = 1.3)
#   
#   fitfinal_all %>% 
#     filter(Start == End & sex == 1 & Start != 4 & YearGr == i) %>% 
#     mutate(tCol = mapvalues(End, from = 1:3, to = colTa),
#            col = mapvalues(End, from = 1:3, to = colT)) %>% 
#     group_by(Start, YearGr) %>%
#     group_map(~lineFct(x=.), .keep = TRUE)
#   
#   if(i == 15){
#     legend("bottomleft", legend = c("In No Care", "In Home Care", "In Care Home"),
#            col = colT[1:3], pch = 15, bty = "n")
#     mtext("Staying in State", 3, line = 1)
#   }
#   
#   if(i ==19){
#     mtext("Age", 1, line = 2.5)
#   }}
# dev.off()

