#~~~~~~~~~~~~~~~~~~~~~~~
#Load required packages:
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(stars) #spatiotemporal rasters
library(ncmeta)
library(IDPmisc)
library(mblm) #package for claculating then-seil slope (mblm())
#~~~~~~~~~~~~~~~~~~~~~~~

######## ~~ FUNCTIONS FOR PROCESSING VARIABLES FROM STARS OBJECTS ~~ #########
#Calculate slope of the time series
slope <- function(stars_object, years = NULL, na.rm = TRUE, N_min_values = 3) {
  
  
  isfinite <- is.finite(stars_object)
  
  if (na.rm) {
    if (any(!isfinite)) {
      stars_object <- stars_object[isfinite]
      years <- years[isfinite]
    }
  }
  
  if (sum(isfinite) >= N_min_values) {
    years = 1:sum(isfinite)
    
    unname(coef(mblm::mblm(stars_object ~ years, repeated = FALSE)))[2]
  } else {
    rep(NA, 1)
  }
}

# #Is slope2 being used?...
# slope2 <- function(stars_object, years = 1:39, na.rm = TRUE, N_min_values = 3) {
#   
#   isfinite <- is.finite(stars_object)
#   
#   if (na.rm) {
#     if (any(!isfinite)) {
#       stars_object <- stars_object[isfinite]
#       years <- years[isfinite]
#     }
#   }
#   
#   if (sum(isfinite) >= N_min_values) {
#     unname(coef(mblm::mblm(stars_object ~ years, repeated = FALSE)))[2]
#   } else {
#     rep(NA, 1)
#   }
# }


#Subtract the trend from the time series (detrend)
detrend <- function(stars_object){
    stars_object - seq_along(stars_object)*slope(stars_object)
}

#Salculate SD for detrended time series
detrend_sd <- function(stars_object, na.rm = TRUE, N_min_values = 3) {
  isfinite <- is.finite(detrend(stars_object))
  
  if (na.rm) {
    if (any(!isfinite)) {
      stars_object <- stars_object[isfinite]
      
    }
  }
  if (sum(isfinite) >= N_min_values) {
    
  sd(stars_object)
  } else {
    rep(NA, 1)
  }
}

#Calculate CV for detrended time series
detrend_cv <- function(stars_object) {
  stars_object.m <- st_apply(stars_object, MARGIN = c("lon", "lat"), FUN = mean, na.rm = TRUE)
  sd(detrend(stars_object))/stars_object.m
}

#Functions to get future-weather related variables
#Get absolute mean differences between consecutive years and calculate the mean of the series
get_differences <- function(hist_series) {
  x <- st_apply(hist_series, MARGIN = c("lon", "lat"), FUN = diff, na.rm = TRUE)
  x <- abs(x)    
  st_apply(x, MARGIN = c("lon", "lat"), FUN = mean, na.rm = TRUE)
}

#Get mean differences between two consecutive years (standardized: divided by the long term mean)
get_st_differences <- function(hist_series) {
  x <- st_apply(hist_series, MARGIN = c("lon", "lat"), FUN = diff, na.rm = TRUE)
  x <- abs(x)    
  st_apply(x, MARGIN = c("lon", "lat"), FUN = mean, na.rm = TRUE)/st_apply(hist_series, MARGIN = c("lon", "lat"), FUN = mean, na.rm = TRUE)
}

#Get the trend of absolute first differences
get_differences_sl <- function(hist_series) {
  x <- st_apply(hist_series, MARGIN = c("lon", "lat"), FUN = diff, na.rm = TRUE)
  x <- abs(x)  
  st_apply(x, MARGIN = c("lon", "lat"), FUN = slope, na.rm = TRUE, single_arg = TRUE)
  
}

get_zscore <- function(fut_clim, hist_clim, hist_sd, na.rm = TRUE){
  zs <-  (fut_clim - hist_clim) / hist_sd
}



####### ~~ SINGLE FUNCTION TO SUMMARIZE HISTORICAL VARIABLES INTO A SINGLE STARS OBJECT ~~ ########
#Function to get variables for historical weather
get_his <- function(hist_clim, hist_series) {
  
    hist_clim <- st_apply(hist_clim, 
                          MARGIN = c("lon", "lat"), 
                          FUN = mean,
                          single_arg = TRUE) 

  #detrended sd
  sd <- st_apply(hist_series, 
                 MARGIN = c("lon", "lat"), 
                 FUN = detrend_sd,
                 single_arg = TRUE)
  #detrended cv
  cv <- sd/hist_clim

  sl <-  st_apply(hist_series, 
                  MARGIN = c("lon", "lat"), 
                  FUN = slope,
                  single_arg = TRUE)
  if (names(hist_series) == "wddseasonality" | 
      names(hist_series) == "swaseasonality" |
      names(hist_series) == "cwdseasonality" |
      names(hist_series) == "prseasonality") {
    diffm <- get_differences(hist_series)
    
  } else {
    diffm <- get_st_differences(hist_series)
  }
  
  diffsl <- get_differences_sl(hist_series)
  x <- c(hist_clim, sd, cv, sl, diffm, diffsl)
  names(x) <- c("mean", "sd", "cv", "slope", "diff", "diffsl")
  return(x)
  
}



####### ~~ SINGLE FUNCTION TO SUMMARIZE FUTURE VARIABLES INTO A SINGLE STARS OBJECT ~~ ########
#Function to get variables for future weather (list of mid, end)
#Uses SD from previous function (must run sd function first and use object as input here)
get_fut <- function(future_clim, hist_clim, agree_45, hist_sd) {
  
  #remove time dimension from historical clim
  hist_clim <- st_apply(hist_clim, 
           MARGIN = c("lon", "lat"), 
           FUN = mean)

  l <- list(mid = NA, end = NA)
  
  for (i in 1:2)  {
    fut_clim <- st_apply(future_clim[,,,i], 
                  MARGIN = c("lon", "lat"), 
                  FUN = mean)
    #subtract historical from future time period to get change variable
    change <- fut_clim - hist_clim
    #agreement
    agree <- st_apply(agree_45[,,,i], MARGIN = c("lon", "lat"), FUN = mean, na.rm = TRUE)
    #calculate zscore 
    zs <-  get_zscore(fut_clim, hist_clim, hist_sd)
    
    l[[i]] <- c(fut_clim, change, agree, zs)
    names(l[[i]]) <- c("mean", "change", "agree", "zscore")
    
  }
  return(l)
  
}


####### ~~ FUNCTIONS TO SUMMARIZE QUARTERLY VARIABLES ~~ ########
get_quarterly_hist <- function(hist_clim, hist_series) {
  x<- list(NA)
  y <- list(NA)
  z <- list(q1 = NA, q2 = NA, q3 = NA, q4 = NA)
  
  
  for (i in 1:4) {
      
      x[[i]] <- hist_clim[,,,i]
 
      y[[i]] <- hist_series[,,,seq(from = i, to = 160, by = 4)]
      
    z[[i]] <- get_his(x[[i]], y[[i]])
  }
  return(z)
  
}


get_quarterly_fut <- function(fut_45_clim, hist_q, agree_45) {
  
  m <- list(NA)
  e <- list(NA)
  a <- list(NA)
  b <- list(NA)
  f <- list(q1 = NA, q2 = NA, q3 = NA, q4 = NA)
  g <- list(q1 = NA, q2 = NA, q3 = NA, q4 = NA)
  
    xm <- fut_45_clim[,,,1:4]
    xe <- fut_45_clim[,,,5:8]
    
    a1 <- agree_45[,,,1:4]
    a2 <- agree_45[,,,5:8]
    
  for (i in 1:4) {
      
      m[[i]] <- xm[,,,i]
      e[[i]] <- xe[,,,i]
      a[[i]] <- a1[,,,i]
      b[[i]] <- a2[,,,i]
      
      g[[i]] <- list(a[[i]], b[[i]])
      g[[i]] <- c(g[[i]][[1]], g[[i]][[2]])

      f[[i]] <- list(m[[i]], e[[i]])
      f[[i]] <- c(f[[i]][[1]], f[[i]][[2]])
      
      x[[i]] <-  get_fut(f[[i]], hist_q[[i]][1], g[[i]], hist_q[[i]][2,,])

  } 
    return(x)
}
    

#~~~~~~~~~~~~~~~~~~~~~~~
#THE END




