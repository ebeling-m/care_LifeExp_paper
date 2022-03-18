# Preparation of inputs for matrix estimations
# Load packages and functions
source("code/A_00_packages.R")

load("data_inter/aggregtedData_fittedProb_all.RData")

# Function for calculating life expectancy by care status
# Transition probabilities are based on three month periods

# Inputs
# Number of transient stages: 3
.tau <-  3
# number of absorbing stages: 1
.alpha <-  1
# Number of age classes seq(70, 104.75, by = 0.25): 140
.om <- length(unique(fitfinal_all$AgeGr))
# Total number of states 
.s <- .tau*.om + .alpha

# test data
# x <-
# fitfinal_all %>%
# filter(YearGr == 19 & sex == 0)
  
le_matrix <- function(tau=.tau, alpha=.alpha, om=.om, s=.s, x, value = "median"){
  
  # Prepare input data
  if(value == "obs"){
    x <- 
      x %>%
      dplyr::select(sex, AgeGr, Start, End, YearGr, p) %>% 
      rename(finalP = p)
  }

  if(value == "median"){
   x <- 
     x %>%
     dplyr::select(sex, AgeGr, Start, End, YearGr, finalP_med) %>% 
     rename(finalP = finalP_med)
  }
  
  if(value == "low"){
    x <- 
      x %>%
      dplyr::select(sex, AgeGr, Start, End, YearGr, finalP_low) %>% 
      rename(finalP = finalP_low)
  }
  
  if(value == "up"){
    x <- 
      x %>%
      dplyr::select(sex, AgeGr, Start, End, YearGr, finalP_up) %>% 
      rename(finalP = finalP_up)
  }
  
  # identity matrices
  Itau <- diag(tau)
  Iom <- diag(om)
  Ialpha <- diag(alpha)
  
  # Calculate Ux and Mx
  U_mat <- list()
  M_mat <- list()
  
  for(i in 1:om){
    # Transient Matrix (U)
    care1 <- 
      x %>%
      filter(AgeGr == unique(x$AgeGr)[i] & !End == 4 )
    
    U_x <- matrix(0, nrow = 3, ncol = 3)
    U_x[,1] <- care1$finalP[1:3]
    U_x[2:3,2] <- care1$finalP[4:5]
    U_x[3,3] <- care1$finalP[6]
    
    # Mortality Matrix (M)
    care2 <- 
      x %>%
      filter(AgeGr == unique(x$AgeGr)[i] & End == 4 & !Start == 4)
    
    M_x <- matrix(care2$finalP, nrow = 1)
    
    # Record Matricies
    U_mat[[i]] <- U_x
    M_mat[[i]] <- M_x
  }
  
  # Age advancement matrix (D)
  # All individuals die in last age group
  D <- list()
  D[[1]] <- D[[2]] <- D[[3]] <-  cbind(rbind(rep(0, om-1), 
                                             diag(om-1)), 
                                       rep(0, om)) 

  # Calculate U_tilde Equation 9 
  U_block <- bdiag(U_mat)
  D_block <- bdiag(D)

  # Vec-permutation matrix
  K <- commutation.matrix(tau, om)
  
  U_tilde <- t(K) %*% D_block %*% K %*% U_block
  
  # Mortality matrix 
  M_tilde <- matrix(unlist(M_mat), nrow = 1)
  
  # Transition matrix P_tilde
  P_tilde_1 <- rbind(U_tilde, M_tilde)
  zero_Mat <- matrix(0, nrow = tau*om, ncol = alpha)
  I_alpha <- diag(alpha, alpha)
  P_tilde_2 <- rbind(zero_Mat, I_alpha)
  P_tilde <- cbind(P_tilde_1, P_tilde_2)
  
  # Fundamental matrix for U_tilde
  I_Utilde <- diag(tau*om)
  I_U <- I_Utilde-U_tilde
  fund_N <- solve(I_U)
  
  
  # Life expectancy based on fundamental matrix
  remain <- t(rep(1, om*tau)) %*% fund_N * 0.25
  colnames(remain) <- rep(c("NC", "HC", "CH"), om)
  
  
  # Define reward matrix
  # Health occupancy matrix H
  H <- matrix(0, nrow = tau, ncol = om)
  colnames(H) <- seq(70, 104.75, by = 0.25)
  rownames(H) <- c("NC", "HC", "CH")
  H.NC <- H.HC <- H.CH <- H
  H.NC[,] <- 1
  H.HC[c("HC", "CH"),] <- 1
  H.CH["CH",] <- 1
  
  # occupancy indicator vector h
  h.NC <- vec(H.NC)
  h.HC <- vec(H.HC)
  h.CH <- vec(H.CH)
  
  # # Logical complement of the entries of h
  # h_inv.NC <- abs(h.NC-1)
  # h_inv.HC <- abs(h.HC-1)
  # h_inv.CH <- abs(h.CH-1)

  # Create vector of ones and zero matrix
  e <- matrix(1, nrow = tau*om, ncol = 1)
  zero_Mat <- matrix(0, nrow = tau*om, ncol = alpha)
  
  # Rewards matrix first moment
  # first moment = mean
  B_ti_1_NC <- 0.5*((e%*%t(h.NC))+(h.NC%*%t(e)))
  C_ti_1_NC <- 0.5*(1%*%t(h.NC))
  R_tilde_1_NC <- cbind(rbind(B_ti_1_NC, C_ti_1_NC), rbind(zero_Mat, 0))

  B_ti_1_HC <- 0.5*((e%*%t(h.HC))+(h.HC%*%t(e)))
  C_ti_1_HC <- 0.5*(1%*%t(h.HC))
  R_tilde_1_HC <- cbind(rbind(B_ti_1_HC, C_ti_1_HC), rbind(zero_Mat, 0))
  
  B_ti_1_CH <- 0.5*((e%*%t(h.CH))+(h.CH%*%t(e)))
  C_ti_1_CH <- 0.5*(1%*%t(h.CH))
  R_tilde_1_CH <- cbind(rbind(B_ti_1_CH, C_ti_1_CH), rbind(zero_Mat, 0))
  
  # The matrix Z slices off the rows and columns of ~P and ~Ri
  Itau <-  diag(tau*om)
  Z <- cbind(Itau,rep(0,tau*om))
  es <- matrix(1,s,1)
  
  # Remaining LE as reward
  rho1_NC <- t(fund_N) %*% Z %*% t(P_tilde*R_tilde_1_NC) %*% es
  rho1_HC <- t(fund_N) %*% Z %*% t(P_tilde*R_tilde_1_HC) %*% es
  rho1_CH <- t(fund_N) %*% Z %*% t(P_tilde*R_tilde_1_CH) %*% es
  
  le_NC <- rho1_NC[seq(1,418, by = 3),1]*0.25
  le_HC <- rho1_HC[seq(2,419, by = 3),1]*0.25
  le_CH <- rho1_CH[seq(3,420, by = 3),1]*0.25
  
  reward_LE <- tibble(le_r=c(le_NC, le_HC, le_CH), 
                      AgeGr = rep(seq(70, 104.75, by = 0.25),3),
                      care = rep(c("NC", "HC", "CH"), each = 140))
                      
  # Calculate remaining life expectancy
  remain_LE <- 
    as.data.frame.table(as.matrix(remain)) %>% 
    as_tibble() %>% 
    mutate(AgeGr = rep(seq(70, 104.75, by = 0.25),each = 3), 
           sex = x$sex[1],
           YearGr = x$YearGr[1]) %>% 
    dplyr::select(Var2, Freq, AgeGr, sex, YearGr) %>% 
    rename(care = Var2, le = Freq) %>% 
    left_join(reward_LE)
  
  return(remain_LE)
}  

# Run estimation
remain_LE_obs <-
  fitfinal_all %>%
  group_by(YearGr, sex) %>% 
  group_map(~le_matrix(x=., value = "obs"), .keep = TRUE) %>% 
  bind_rows() %>% 
  rename(le_obs = le,
         le_obs_r = le_r)

remain_LE_med <-
  fitfinal_all %>%
  group_by(YearGr, sex) %>% 
  group_map(~le_matrix(x=., value = "median"), .keep = TRUE) %>% 
  bind_rows() %>% 
  rename(le_med = le,
         le_med_r = le_r)

remain_LE_low <-
  fitfinal_all %>%
  group_by(YearGr, sex) %>% 
  group_map(~le_matrix(x=., value = "low"), .keep = TRUE) %>% 
  bind_rows() %>% 
  rename(le_low = le,
         le_low_r = le_r)

remain_LE_up <-
  fitfinal_all %>%
  group_by(YearGr, sex) %>% 
  group_map(~le_matrix(x=., value = "up"), .keep = TRUE) %>% 
  bind_rows() %>% 
  rename(le_up = le,
         le_up_r = le_r)

# Population proportions

popProp <- 
  fitfinal_all %>% 
  dplyr::select(AgeGr, Start, sex, YearGr, Tpop) %>% 
  distinct() %>% 
  group_by(AgeGr, sex, YearGr) %>% 
  mutate(relPop = Tpop/sum(Tpop)) %>% 
  arrange(sex, YearGr, AgeGr, Start) %>% 
  mutate(Start = mapvalues(Start, from = 1:3, to = c("NC", "HC", "CH"))) %>% 
  rename(care = Start)


remain_LE <- 
  remain_LE_obs %>% 
  left_join(remain_LE_med) %>% 
  left_join(remain_LE_low) %>% 
  left_join(remain_LE_up) %>% 
  left_join(popProp) %>% 
  group_by(AgeGr, YearGr, sex) %>% 
  mutate(leT_obs = sum(le_obs*relPop),
         leT_med = sum(le_med*relPop),
         leT_low = sum(le_low*relPop),
         leT_up = sum(le_up*relPop),
         leT_obs_r = sum(le_obs_r*relPop),
         leT_med_r = sum(le_med_r*relPop),
         leT_low_r = sum(le_low_r*relPop),
         leT_up_r = sum(le_up_r*relPop))


save(remain_LE, file = "data_inter/leEstimates.RData")

