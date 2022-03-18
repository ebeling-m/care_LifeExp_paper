# Function and packages that are required for the analysis of life 
# expectancy by care status 
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)


# Additional Function borrowed from plyr package

round_any <- function (x, accuracy, f = round) 
{
  f(x/accuracy) * accuracy
}

mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

# Function for calculating life expectancy based on aggreagted data

lifetableOne <- function(mx){
  n <- rep(1, length(mx))
  ax <- rep(0.5, length(mx))
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[length(qx)] <- 1
  px <- 1-qx
  lx <- c(100000, cumprod(px)*100000)[-(length(mx)+1)]
  dx <- c(-diff(lx), lx[length(lx)])
  Lx1 <- n[-length(n)]*lx[-1]+ax[-length(ax)]*dx[-length(dx)]
  Lx2 <- dx[length(dx)]/mx[length(mx)]
  Lx <- c(Lx1, Lx2)
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  out <- ex[1]
  # out <- data.frame(lx=lx, Lx=Lx, Tx=Tx, ex=ex)
  return(out)
}

# # Function to reformat dates
# monthSh <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
# monthNu <- c(paste("0", 1:9, sep = ""), "10", "11", "12")

month_key <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06", jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")


dateRe <- function(x){
  if(is.na(x) == TRUE)
    {out <- NA}else{
  year <- substr(x, 6, 9)
  day <- substr(x, 1, 2)
  month <- substr(x, 3, 5)
  monthD <- recode(month, !!!month_key)
  out <- paste(year, monthD, day, sep = "-")}
  return(out)
}
