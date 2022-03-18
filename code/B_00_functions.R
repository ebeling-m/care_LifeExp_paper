# Author: Enrique Acosta
# kikepaila@gmail.com
# Script elaborated for 2020 YLL by care status in Sweden
# Last update: 2021-10-06

# Description:
# Functions and preparation of environment script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")


# Installing missing packages
# ===========================

# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}

library(pacman)

# Required CRAN packages
pkgs <- c("tidyverse",
          "here",
          "ISOweek",
          "lubridate",
          "mgcv")

# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(c(pkgs)))==0) {
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# ====


# loading required packages
# =========================
p_load(pkgs, character.only = TRUE)


# ====
# function to copy objects to the clipboard, so one can paste it on Excel 
copy_to_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# functions for baseline mortality estimation
# ===========================================

# fitting the model for monthly excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline <- function(db, knots = 3){
  
  
  if(!is.na(knots)){
    gam_model <- 
      gam(Dx ~ t_p + 
            s(month, bs = 'cp', k = knots) +
            offset(log(Nx)), 
          weights = w,
          data = db, 
          family = "quasipoisson")
  }else{
    gam_model <- 
      gam(Dx ~ t_p + 
            s(month, bs = 'cp') +
            offset(log(Nx)), 
          weights = w,
          data = db, 
          family = "quasipoisson")
  }
  
  resp <- predict(gam_model, newdata = db, type = "response")
  
  db %>% 
    mutate(bsn = resp,
           p_score = Dx / bsn,
           dts_r = Dx / Nx,
           bsn_r = bsn / Nx) %>% 
    left_join(simul_intvals(gam_model, db, 1000), 
              by = "date") %>% 
    mutate(ll_r = ll / Nx,
           ul_r = ul / Nx)
}


# fitting the model for annual excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# db <- 
#   dts_exp %>% 
#   mutate(w = ifelse(year == 2020, 0, 1),
#          date = year) %>% 
#   filter(age <= 100) %>% 
#   group_by(sex, age) %>% 
#   mutate(t = 1:n()) %>% 
#   filter(age == 0,
#          sex == "f")

est_annual_baseline <- function(db){
  
  print(paste0("age ", unique(db$age), ", sex ", unique(db$sex)))
  
  model <- 
    glm(Dx ~ t + offset(log(Nx)), 
        weights = w,
        data = db, 
        family = "quasipoisson")
  
  resp <- predict(model, newdata = db, type = "response")
  
  db %>% 
    mutate(bsn = resp,
           p_score = Dx / bsn,
           dts_r = Dx / Nx,
           bsn_r = bsn / Nx) %>% 
    left_join(simul_intvals(model, db, 1000), 
              by = "date") %>% 
    mutate(ll_r = ll / Nx,
           ul_r = ul / Nx)
  
}

# bootstrapping using Jonas' method 
simul_intvals <- function(model, db, nsim){
  model_type <- model$method
  # matrix model
  if(model_type == "glm.fit"){
    X_prd <- model.matrix(model, data = db, na.action = na.pass)
  }
  if(model_type == "GCV"){
    X_prd <- predict(model, newdata = db, type = 'lpmatrix')
  }
  # estimated coefficients
  beta <- coef(model)
  # offsets
  offset_prd <- matrix(log(db$Nx))
  
  # applying Huber-White adjustment for robust estimators 
  # beta_sim <- MASS::mvrnorm(nsim, beta, sandwich::vcovHAC(model))
  beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
  Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd%*%b + offset_prd))
  
  y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
    y <- mu <- Ey
    # NA's can't be passed to the simulation functions, so keep them out
    idx_na <- is.na(mu) 
    mu_ <- mu[!idx_na] 
    N <- length(mu_)
    phi <- summary(model)$dispersion
    # in case of under-dispersion, sample from Poisson
    if (phi < 1) { phi = 1 }
    y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
    return(y)
  })
  
  ints_simul <- 
    db %>% 
    select(date)
  
  colnames_y_sim <- paste0('deaths_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  ints_simul <-
    ints_simul %>%
    pivot_longer(cols = starts_with('deaths_sim'),
                 names_to = 'sim_id', values_to = 'deaths_sim') %>%
    group_by(date) %>%
    summarise(
      ll = quantile(deaths_sim, 0.05, na.rm = TRUE),
      ul = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}

# function to estimate baseline mortality with user specifications
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
give_me_baseline <- 
  function(db_in, 
           age_gr = 5,
           age_cl = 100,
           last_date = "2020-02-15",
           mths_exc = c(0),
           knots = 3){
    
    # preparing data to fit, aggregating age intervals, setting close age
    db_to_fit <- 
      db_in %>% 
      mutate(age_gr = age - age %% age_gr,
             age_gr = ifelse(age_gr > age_cl, age_cl, age_gr)) %>% 
      group_by(year, month, t_p, date, care, sex, age_gr) %>% 
      summarise(Dx = sum(Dx),
                Nx = sum(Nx)) %>% 
      ungroup() %>% 
      mutate(w = ifelse(date <= last_date & !month %in% mths_exc, 1, 0))
    
    # fitting baseline mortality
    db_bsn <- 
      db_to_fit %>% 
      arrange(care, sex, age_gr, date) %>% 
      group_by(care, sex, age_gr) %>% 
      do(est_baseline(db = .data, knots = knots)) %>% 
      ungroup() %>% 
      mutate(excess = ifelse(date > last_date & Dx > ul, "y", "n"))
    
    # saving estimates
    write_rds(db_bsn, paste0("data_inter/baselines_age", age_gr, "_", age_cl, "plus_exc_", mths_exc, "_", knots, "knots.rds"))
    
    # plotting all estimates
    db_bsn %>% 
      ggplot()+
      geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.2, fill = "#118ab2")+
      geom_line(aes(date, ll_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
      geom_line(aes(date, ul_r), size = 0.1, alpha = 0.3, col = "#118ab2")+
      geom_line(aes(date, dts_r), size = 0.2, alpha = 0.2)+
      geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.8)+
      geom_point(aes(date, dts_r, col = excess), size = 0.3, alpha = 0.7)+
      geom_vline(xintercept = ymd("2020-02-15"), linetype = "dashed", 
                 col = "#06d6a0",
                 alpha = 0.5,
                 size = 0.3)+
      scale_color_manual(values = c("#073b4c", "#ef476f"))+
      facet_wrap(age_gr ~ care + sex, scales = "free", ncol = 6)+
      labs(title = paste0("age", age_gr, "_", age_cl, "plus_excl_", mths_exc))+
      theme_bw()+
      theme(
        legend.position = "none",
        plot.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(margin = margin(b = 0, t = 0),
                                  size = 7),
        strip.background = element_blank()
      )
    ggsave(here("figures", paste0("baselines_age", age_gr, "_", age_cl, "plus_exc_", mths_exc, "_", knots, "knots.pdf")),
           width = 10,
           height = length(unique(db_to_fit$age_gr)) * 2.5,
           limitsize = FALSE)

  }


