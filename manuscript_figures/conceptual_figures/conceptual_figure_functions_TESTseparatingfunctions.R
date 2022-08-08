
#------ Data for conceptual figures of new drought metrics ------
# * Overall conditions (TDD, TDD longest duration, WDD, DDD, SWA, CWD,
#   First/last frost)
# * Seasonal variability (TDD seasonal variability, SWA seasonal variability,
#   CWD seasonal variability)
# * Seasonal moisture timing (WDD seasonality, SWA seasonality, PPT seasonality,
#   CWD seasonality)
# * Extreme hot-dry conditions (CWD extreme 10day, DDD longest duration, DSI
#   mean, DSI number)
# * Recruitment (Spring/Fall index, spring/fall timing, and # of years with
#   recruitment)


#----- Load packages -----
library(remotes)
library(zoo)
library(scales)
library(usethis)
library(memisc)
library(sf)
library(Hmisc)
library(rmapshaper)

install.packages(grDevices)
version(grDevices)

#library(rSW2metrics)
usethis::create_github_token()
usethis::edit_r_environ()

GITHUB_PAT <- "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"

remotes::install_github("dchenow1/rSW2metrics")

sitename <- "002544_HotDeserts_Site_2544"


#--------- Set directory ----------
dir_prj <- "manuscript_figures/conceptual_figures"
dir_data <- file.path(dir_prj, "data")
dir_sim <- file.path(dir_data, "3_Runs")  



############################################################################################################################################
#------- Extract metrics from soilwat output -------
############################################################################################################################################

extract_metrics <- function(sitename, label = NULL) {
  name_sw2_run <- sitename #this stays in function
  
  if(is.null(label)) label <- sitename
  
  list_years_scen <- list(1971:2010)
  id_scen <- 1
  
  list_metrics <- list(
    TDD = "metric_TDDat5C",
    WDD = "metric_WDDat5C0to100cm15bar",
    DDD = "metric_DDDat5C0to100cm30bar",
    SWA = "metric_SWAat0to100cm39bar",
    CWD = "metric_CWD",
    Frost = "metric_FrostDaysAtNeg5C",
    PPTseas = "metric_CorTempPPT",
    DSI = "metric_DSIat0to100cm15bar",
    # Recruitment: use `metric_RecruitmentIndex_v4` if 0-5 cm soil layer is available
    Recruitment = "metric_RecruitmentIndex_v5",
    ET = "metric_ETyr"
  )
  
  #------ Load soils ------
  soil_variables <- rSW2metrics::list_soil_variables()
  soils_all <- lapply(
    file.path(
      dir_data,
      "soils",
      paste0("input_soillayers_", soil_variables, ".rds")
    ),
    readRDS
  )
  
  names(soils_all) <- names(soil_variables)
  
  used_soil <- rSW2metrics:::prepare_soils_for_site(
    path = dir_sim,
    name_sw2_run = name_sw2_run,
    name_sw2_run_soils = rSW2metrics::shorten_run_names(
      name_sw2_run,
      N_discard = 1
    ),
    soils = soils_all,
    soil_variables = soil_variables,
    var_soilsite = "Part_Label"
  )
  # ---------------------
  

  #------- Load daily rSOILWAT2 output ------
  sim_data <- rSW2metrics:::collect_sw2_sim_data(
    path = dir_sim,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    output_sets = list(
      clim = list(
        sw2_tp = "Day",
        sw2_outs = c("TEMP", "TEMP", "PRECIP"),
        sw2_vars = c(tmin = "min_C", tmean = "avg_C", ppt = "ppt"),
        varnames_are_fixed = TRUE
      ),
      et = list(
        sw2_tp = "Day",
        sw2_outs = c("PET", "AET"),
        sw2_vars = c(pet = "pet_cm", et = "evapotr_cm"),
        varnames_are_fixed = TRUE
      ),
      swp = list(
        sw2_tp = "Day",
        sw2_outs = "SWPMATRIC",
        sw2_vars = c(swp = "Lyr"),
        varnames_are_fixed = FALSE
      ),
      swc = list(
        sw2_tp = "Day",
        sw2_outs = "SWCBULK",
        sw2_vars = c(swc = "Lyr"),
        varnames_are_fixed = FALSE
      )
    )
  )
  #-----------------------------
  
  
  
  #------ Extract metrics ------
  foo_args_metrics <- list(
    path = dir_sim,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen,
    list_years_scen_used = list_years_scen,
    soils = used_soil
  )
  
  sim_metrics <- lapply(
    list_metrics,
    rSW2metrics:::formatted_metric_1sim,
    foo_args = c(foo_args_metrics, out = "ts_years")
  )
  
  
  sim_metrics_raw <- lapply(
    list_metrics,
    rSW2metrics:::formatted_metric_1sim,
    foo_args = c(foo_args_metrics, out = "raw")
  )
  #---------------------------
  
  return(list(sim_data,
              sim_metrics,
              sim_metrics_raw,
              used_soil))
  
  }
  
  #-------- Save objects? --------
    # save(
    #   sim_data,
    #   sim_metrics,
    #   sim_metrics_raw,
    #   used_soil,
    #   file = file.path(dir_data, "Metrics", paste0(name_sw2_run, ".rda"))
    # )

  #---------------------------
  







############################################################################################################################################
#------- Reformat extracted metrics for a mean year -------
############################################################################################################################################

#------- Reformat into daily values ---------
day.mean.format <- function(extract_list = extract_list, 
                        rolling.mean = TRUE,
                        rolling.mean.k = 7,
                        lower = 0.1,
                        upper = 0.9) {
  
  sim_data <- extract_list[[1]]
  sim_metrics <- extract_list[[2]]
  sim_metrics_raw <- extract_list[[3]]
  used_soil <- extract_list[[4]]

  Tmean_Meanyear <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Day"]), mean)
  Tmin_Meanyear <- aggregate(sim_data$clim$values$tmin, list(sim_data$clim$time[, "Day"]), mean)
  PPT_Meanyear <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Day"]), mean)
  AET_Meanyear <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Day"]), mean)
  PET_Meanyear <- aggregate(sim_data$et$values$pet, list(sim_data$et$time[, "Day"]), mean)
  SWP_Meanyear <- aggregate(sim_data$swp$values, list(sim_data$swp$time[, "Day"]), mean)
  
  TDD_Meanyear <- aggregate(sim_metrics_raw$TDD[[1]]$values$mdd, list(sim_metrics_raw$TDD[[1]]$time[, "Day"]), mean)
  WDD_Meanyear <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Day"]), mean)
  DDD_Meanyear <- aggregate(sim_metrics_raw$DDD[[1]]$values$mdd, list(sim_metrics_raw$DDD[[1]]$time[, "Day"]), mean)
  SWA_Meanyear <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Day"]), mean)
  CWD_Meanyear <- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Day"]), mean) 
  
  #Get the rolling mean for a year if rolling.mean argument is TRUE (default); 
  #use rolling.mean.k to choose k for rollmean_year() (default k is 7)  
  
  if (rolling.mean == TRUE) {
    rollmean_year <- function(x, k = rolling.mean.k) {
      x = x[-60,]
      endfill = x[1:(k/2),]
      begfill = x[c((max(x$Group.1)-k/2 + 1):max(x$Group.1)-1),]
      new <- rbind(begfill, x, endfill)
      newroll <- rollmean(new, k)
      newroll <- data.frame(newroll[-366,])
      newroll$Group.1 <- 1:365
      return(newroll)
    }
    
    Tmean_rollMeanyear <- rollmean_year(Tmean_Meanyear)
    Tmin_rollMeanyear <- rollmean_year(Tmin_Meanyear)
    PPT_rollMeanyear <- rollmean_year(PPT_Meanyear)
    AET_rollMeanyear <- rollmean_year(AET_Meanyear)
    PET_rollMeanyear <- rollmean_year(PET_Meanyear)
    SWP_rollMeanyear <- rollmean_year(SWP_Meanyear)
    TDD_rollMeanyear <- rollmean_year(TDD_Meanyear)
    WDD_rollMeanyear <- rollmean_year(WDD_Meanyear)
    DDD_rollMeanyear <- rollmean_year(DDD_Meanyear)
    SWA_rollMeanyear <- rollmean_year(SWA_Meanyear)
    CWD_rollMeanyear <- rollmean_year(CWD_Meanyear)
  }
  
  #Get upper and lower percentiles that will be plotted around the mean
  #upper and lower percentiles are set by arguments (lower = 0.1 and upper = 0.9 by default)
  
  percentiles <- function(x){
    quantile(x, c(lower, upper))
  }
  
  SWA_percentiles <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Day"]), percentiles)[2][[1]]
  SWA_percentiles <- data.frame(SWA_percentiles)
  SWA_percentiles$Group.1 <- c(1:366)
  SWA_rollpercentiles <- rollmean_year(SWA_percentiles)
  
  AET_percentiles <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Day"]),  percentiles)[2][[1]]
  AET_percentiles <- data.frame(AET_percentiles)
  AET_percentiles$Group.1 <- c(1:366)
  AET_rollpercentiles <- rollmean_year(AET_percentiles)
  
  WDD_percentiles <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Day"]), percentiles)[2][[1]]
  WDD_percentiles <- data.frame(WDD_percentiles)
  WDD_percentiles$Group.1 <- c(1:366)
  WDD_rollpercentiles <- rollmean_year(WDD_percentiles)
  
  day_list <- list(Tmean_rollMeanyear,
                   Tmin_rollMeanyear,
                   PPT_rollMeanyear,
                   AET_rollMeanyear,
                   PET_rollMeanyear,
                   SWP_rollMeanyear,
                   TDD_rollMeanyear,
                   WDD_rollMeanyear,
                   DDD_rollMeanyear,
                   SWA_rollMeanyear,
                   CWD_rollMeanyear,
                   SWA_rollpercentiles,
                   AET_rollpercentiles,
                   WDD_rollpercentiles)
  
  names(day_list) <- c("Tmean_rollMeanyear",
                          "Tmin_rollMeanyear",
                          "PPT_rollMeanyear",
                          "AET_rollMeanyear",
                          "PET_rollMeanyear",
                          "SWP_rollMeanyear",
                          "TDD_rollMeanyear",
                          "WDD_rollMeanyear",
                          "DDD_rollMeanyear",
                          "SWA_rollMeanyear",
                          "CWD_rollMeanyear",
                          "SWA_rollpercentiles",
                          "AET_rollpercentiles",
                          "WDD_rollpercentiles")
  
  if (rolling.mean == TRUE) {
    return(day_list)
  }
  

}

#-----------------------------------------



#------- Reformat into monthly data -------
month.mean.format <- function(extract_list = extract_list) {
  
  sim_data <- extract_list[[1]]
  sim_metrics <- extract_list[[2]]
  sim_metrics_raw <- extract_list[[3]]
  used_soil <- extract_list[[4]]
  
    Tmean_Mean_monthly <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Month"]), mean)
    WDD_Mean_monthly <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Month"]), mean)
    SWA_Mean_monthly <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Month"]), mean)
    CWD_Mean_monthly <- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Month"]), mean)
    PPT_Mean_monthly <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Month"]), mean)  
    
    month_list <- list(Tmean_Mean_monthly,
                WDD_Mean_monthly,
                SWA_Mean_monthly,
                CWD_Mean_monthly,
                PPT_Mean_monthly)
    
    names(month_list) <- list("Tmean_Mean_monthly",
                             "WDD_Mean_monthly",
                             "SWA_Mean_monthly",
                             "CWD_Mean_monthly",
                             "PPT_Mean_monthly")
    
   return(month_list)
}
#-----------------------------------------



#------- Reformat into time series means -------
year.mean.format <- function(extract_list = extract_list) {
  
  sim_data <- extract_list[[1]]
  sim_metrics <- extract_list[[2]]
  sim_metrics_raw <- extract_list[[3]]
  used_soil <- extract_list[[4]]
  
  FirstFrost_Mean <- mean(sim_metrics_raw$Frost[[1]]["FirstFrost", ]) #or could use sim_metrics$Frost[which(sim_metrics$Frost$group=="FirstFrost"), -c(1,2)]
  LastFrost_Mean <- mean(sim_metrics_raw$Frost[[1]]["LastFrost", ]) #or could use sim_metrics$Frost[which(sim_metrics$Frost$group=="LastFrost"), -c(1,2)]
  PPTseas_Mean <- mean(sim_metrics_raw$PPTseas[[1]]["seasonality", ])
  
  year_list <- list(FirstFrost_Mean,
                    LastFrost_Mean,
                    PPTseas_Mean)
  
  names(year_list) <- list("FirstFrost_Mean",
                           "LastFrost_Mean",
                           "PPTseas_Mean")
  return(year_list)
}
#-----------------------------------------



#------- Reformat extracted metrics by year -------
yearly.means.format <- function(extract_list = extract_list) {
  
  sim_data <- extract_list[[1]]
  sim_metrics <- extract_list[[2]]
  sim_metrics_raw <- extract_list[[3]]
  used_soil <- extract_list[[4]]
  
  PET_eachyear <- aggregate(sim_data$et$values$pet, list(sim_data$et$time[, "Year"]), sum)
  AET_eachyear <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Year"]), sum)
  PPT_eachyear <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Year"]), sum)
  CWD_eachyear<- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Year"]), sum)
  SWA_eachyear <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Year"]), mean)
  
  DSI_Lengths <- sim_metrics$DSI[which(sim_metrics$DSI$group=="mean"), -c(1,2)]
  DSI_Number <- sim_metrics$DSI[which(sim_metrics$DSI$group=="N"), -c(1,2)]
  
  Recruitment <- sim_metrics$Recruitment #[which(sim_metrics$Recruitment$group=="N"), -c(1,2)]
  for(v in 1:dim(Recruitment)[1]){
    assign(paste(Recruitment$group[v]), as.numeric(Recruitment[v, -c(1,2)]))
  } 
  
  yearly_means_list <- list(PET_eachyear,
                            AET_eachyear,
                            PPT_eachyear,
                            CWD_eachyear,
                            SWA_eachyear,
                            DSI_Lengths,
                            DSI_Number,
                            Recruitment)
  
  names(yearly_means_list) <- list("PET_eachyear",
                                   "AET_eachyear",
                                   "PPT_eachyear",
                                   "CWD_eachyear",
                                   "SWA_eachyear",
                                   "DSI_Lengths",
                                   "DSI_Number",
                                   "Recruitment")
  return(yearly_means_list)
  
}
#-------------------------------------
  
  


#Make list of colors used throughout
color.set.default <- list(temperatureCOLOR = "#9B2226",
                  mintemperatureCOLOR = "#CA6702",
                  precipCOLOR = "#005F73", 
                  soilCOLORS = viridis::cividis(6), 
                  SWA_COLOR = "#0A9396",
                  DSI_COLOR = "#001219",
                  PET_COLOR = "#EE9B00",
                  AET_COLOR = "#94D2BD",
                  CWD_COLOR = "#E9D8A6",
                  dryCOLOR = "#CA6702",
                  wetCOLOR = "#005F73",
                  neutralCOLOR = "#E9D8A6")
#-------------------------------------



#testing stuff
sitename <-  "002544_HotDeserts_Site_2544"
repsitenames_ecoregions <- repsitenames

color.set <- color.set.default
##


#--------- Extract metrics into objects and draw the conceptual figure  ----------
make.site.figure <- function(sitename,
                            label = NULL,
                            repsitenames = NULL, #
                            rolling.mean = TRUE, #whether or not to use a rolling mean
                            rolling.mean.k = 7, #number of days for which to use a rolling mean (default is 7)
                            upper = 0.9, #percent of distribution for plotting upper bound of the distribution (default is 90%)
                            lower = 0.1, #percent of distribution for plotting lower bound of the distribution (default is 10%)
                            layout.matrix = NULL, #be able to set layout other than the default
                            color.set = color.set.default) #assign a list of colors in the same order as the default
  { 

  
 #use the sitename as the label (does this do anything that sitename does not?) 
 if (is.null(label) == TRUE) label <- sitename
  
 extract_list <- extract_metrics(sitename)
 
 Tmean_Meanyear <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Day"]), mean)
 
 sim_data <- extract_list[[1]]
 sim_metrics <- extract_list[[2]]
 sim_metrics_raw <- extract_list[[3]]
 used_soil <- extract_list[[4]]
  
 #extract soilwat output to get objects for each of the variables
 day_list <- day.mean.format(extract_list)
 for(i in 1:length(day_list)[1]){
   if (grepl("percentiles", names(day_list[i]))) {
     assign(names(day_list[i]), as.data.frame(day_list[[i]][,-3]))
   } else {
     assign(names(day_list[i]), as.data.frame(day_list[[i]][,-1]))
   }
   
 } 
 
 #day_list[10]
 
 

 month_list <- month.mean.format(extract_list)
 for(i in 1:length(month_list)[1]){
   assign(names(month_list[i]), as.numeric(month_list[[i]][,2]))
 }
 
 year_list <- year.mean.format(extract_list)
 for(i in 1:length(year_list)[1]){
   assign(names(year_list[i]), as.numeric(year_list[[i]]))
 }
 
 yearly_means_list <- yearly.means.format(extract_list)
 for(i in 1:length(yearly_means_list)[1]) {
     if (length(yearly_means_list[[i]]) <= 2) {
       assign(names(yearly_means_list[i]), as.numeric(yearly_means_list[[i]][,2]))
     } else if (grepl("Recruitment", names(yearly_means_list[i]))) {
       for(v in 1:dim(Recruitment)[1]){
         assign(paste(Recruitment$group[v]), as.numeric(Recruitment[v, -c(1,2)]))
       } 
     } else {
     assign(names(yearly_means_list[i]), as.numeric(t(yearly_means_list[[i]])))
   }
 }
 #-------------------------------------------------
 
  #------- Set colors based on variables ---------
 #color.set is an argument where the default is set above with the functions
    temperatureCOLOR <- color.set[[1]]
    mintemperatureCOLOR <- color.set[[2]]
    precipCOLOR <- color.set[[3]]
    soilCOLORS <- color.set[[4]]
    SWA_COLOR <- color.set[[5]]
    DSI_COLOR <- color.set[[6]]
    PET_COLOR <- color.set[[7]]
    AET_COLOR <- color.set[[8]]
    CWD_COLOR <- color.set[[9]]
    dryCOLOR <- color.set[[10]]
    wetCOLOR <- color.set[[11]]
    neutralCOLOR <- color.set[[12]]

  #------ Set up plot layout ------
  if (is.null(layout.matrix) == TRUE) {
    lay = matrix(c(rep(c(1, 2, seq(2, 10, by = 2)), 2), c(3, 12, 12, seq(5, 11, by = 2), 3, 12, 12, seq(5, 11, 2))), nrow=7, ncol=4)
  } else {
    lay = layout.matrix
  }
  quartz(width=6.5, height=7)
  layout(lay,
         heights = c(0.5,1.5,0.5,2,2,2,0.5),
         widths = c(2,2,0.5,0.5))
  
  #################################
  
  #-------- ROW 1: Title --------
  x = c(1,2)
  y = c(1,2)
  par(mar=c(0, 3.2, 0, 1), xpd = FALSE)
  
  ecoregion_label <- repsitenames$ecoregion[repsitenames$site == label]
  plot(y ~ x, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', xaxs = "i", yaxs = "i")
  mtext(ecoregion_label[1], at = c(1.01), col="black", font=2, cex=1, padj = 0, adj = 0, line = -2.2)
  
  #Use Tmean_Meanyear to get dates for 365 days of the year; xValues will be used to plot the x-axis; row 60 (leap day) is removed and ignored
  xValues <- as.Date(Tmean_Meanyear$Group.1,  origin = "2019-12-31")[-60]
  #-------------------------
  #-------- PLOT 1 ---------
  #~~~~~~~ Climate plot (temp axis) ~~~~~~~
  par(mar=c(0, 3.5, 0, 3.5), xpd = FALSE)
  
  TempLimits <- c(min(c(Tmin_rollMeanyear,0)), max(c(Tmean_rollMeanyear), max(PPT_rollMeanyear*10*30/2)))
  PlotLimits <- TempLimits*1.2
  plot(Tmean_rollMeanyear ~ xValues, type="l", xlab="", yaxt='n',  xaxt='n', ylab="", ylim=PlotLimits, col=temperatureCOLOR, lwd=1, xaxs = "i", yaxs = "i")
  axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n", cex = 12)
  mgp.axis(2, col="black", at=pretty(c(TempLimits[1], min(TempLimits[2], 50))), col.axis="black", las=1, cex.axis = 1, mgp = c(1,2,0))
  abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
  mtext("Daily Temperature (°C)", side=2, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  lines(Tmin_rollMeanyear~ xValues, lwd=1, col=mintemperatureCOLOR)

  #~~~~~~~~ Climate plot (precipitation axis) ~~~~~~~~
  par(new=TRUE)
  
  plot(PPT_rollMeanyear*10, ylim=PlotLimits*2/30, type="l", axes=FALSE, bty = "n", xlab = "", ylab = "", lwd=1, col=precipCOLOR,  xaxs = "i", yaxs = "i")
  mgp.axis(4, col="black", at=pretty(c(0, TempLimits[2]*2/30)), col.axis="black", las=1, cex.axis = 1, mgp = c(1,2,0))
  mtext("Mean Daily Precipitation (mm)" , side=4, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  abline(h=0, lwd=0.5, lty=2)

  #~~~~~~~~~~ Plot labels ~~~~~~~~~~
  #white background box
  rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = 350, ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
  #panel title
  rect(xleft = 1, xright = 62, ybottom = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.18), ytop = max(PlotLimits*2/30), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 1, y = max(PlotLimits*2/30*0.89), labels = "Temperature &\nPrecipitation", col="black", font = 2, cex = 0.8, pos = 4)
  #correlation with temp box
  rect(xleft = 62, xright = 165, ybottom = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.18), ytop = max(PlotLimits*2/30), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 62, y = max(PlotLimits*2/30*0.89), labels = "Monthly Precipitation\nCorr. with Temperature", col="black", cex = 0.7, pos = 4)
  text(x = 133, y = max(PlotLimits*2/30*0.89), labels = paste("= ",round(mean(as.numeric(sim_metrics$PPTseas[-c(1,2)])), 2)), col="black", cex = 0.7, pos = 4)
  #panel label
  text(x = -4, y = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.30), labels = "A", col="black", font = 2, cex = 2.5, pos = 4, offset = 1)
  #legend inside the plot border
  text(x = 354, y = max(PlotLimits*2/30*0.95), labels = "Mean Daily Temperature", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 354, y = max(PlotLimits*2/30*0.89), labels = "Minimum Daily Temperature", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 354, y = max(PlotLimits*2/30*0.83), labels = "Mean Daily Precipitation", col="black", font = 1, cex = 0.6, pos = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.95), max(PlotLimits*2/30*0.95)), type = "l", col = temperatureCOLOR, lwd = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.89), max(PlotLimits*2/30*0.89)), type = "l", col = mintemperatureCOLOR, lwd = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.83), max(PlotLimits*2/30*0.83)), type = "l", col = precipCOLOR, lwd = 2)
  
  #~~~~~~~~~ Climate variables ~~~~~~~~~~
  par(mar=c(0,0,0,0))
  x <- c(1:5)
  y <- c(1:5)
  plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1, xaxs = "i", yaxs = "i")

  #-------------------------
  #-------- PLOT 2 ---------
  
  #~~~~~~~ Soil water plot (SWP axis) ~~~~~~~
  
  par(mar=c(0, 3.5, 0, 3.5), bg = "white")
  addspace <- c(min(SWP_rollMeanyear/-10), 0)[1]*1.2 - c(min(SWP_rollMeanyear/-10), 0)[1]
  PlotLimits <- c(min(SWP_rollMeanyear/-10)[1], (0 - addspace))
  plot(SWP_rollMeanyear$swp.Lyr_1/-10 ~ xValues, type="l",  xlab = "", yaxt='n', xaxt='n', ylab="", lwd=1, col=soilCOLORS[5], ylim=PlotLimits,  xaxs = "i", yaxs = "i")
  mgp.axis(2, col="black",  col.axis="black", las=1, cex.axis = 1, mgp = c(1,2,0))
  mtext("Soil Water Potential (MPa)", side=2, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  lines(SWP_rollMeanyear$swp.Lyr_2/-10~ xValues,  lwd=1, col=soilCOLORS[4])
  lines(SWP_rollMeanyear$swp.Lyr_3/-10~ xValues, lwd=1, col=soilCOLORS[3])
  lines(SWP_rollMeanyear$swp.Lyr_4/-10~ xValues,  lwd=1, col=soilCOLORS[2])
  abline(h=c(0), lty=2, col="black", lwd = 0.5)
  axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n")
  abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
  
  #~~~~~~~~ Soil water plot (SWA axis) ~~~~~~~~
  
  par(new=TRUE)
  SWALimits <- c(0, max(SWA_rollMeanyear*2))
  PlotLimits <- SWALimits*1.2
  plot(SWA_rollMeanyear$`day_list[[i]][, -1]`, type="l",  axes=FALSE, bty = "n", xlab = "", ylab = "", col=SWA_COLOR, lwd=1, ylim=PlotLimits,  xaxs = "i", yaxs = "i")
  polygon(x=c(SWA_rollMeanyear$Group.1, rev(SWA_rollMeanyear$Group.1)), y=c(SWA_rollMeanyear[,2], rep(0, 365)), col=alpha(SWA_COLOR, 0.3), border = NA)
  mgp.axis(4, col="black",  col.axis="black", las=1, at=pretty(c(0, max(SWA_rollMeanyear[,2]))), cex.axis = 1, mgp = c(1,2,0))
  mtext("Soil Water Availability (mm)", side=4, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  lines(x = SWA_rollMeanyear, y = SWA_rollpercentiles[,1], lty = 3, col = SWA_COLOR, lwd = 1)
  lines(x = SWA_rollMeanyear, y = SWA_rollpercentiles[,2], lty = 3, col = SWA_COLOR, lwd = 1)
  
  #~~~~~~~~~~ Plot labels ~~~~~~~~~~
  #white background box
  rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = 365, ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
  #panel title
  rect(xleft = 1, xright = 80, ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 1, y = max(PlotLimits*0.89), labels = "Soil Water Potential\n& Availability", col="black", font = 2, cex = 0.8, pos = 4)
  #correlation with temp box
  rect(xleft = 80 , xright = 198, ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 78, y = max(PlotLimits*0.89), labels = "Monthly Soil Water Availability\nCorr. with Temperature", col="black", cex = 0.7, pos = 4)
  text(x = 168, y = max(PlotLimits*0.89), labels = paste("= ", round(mean(as.numeric(sim_metrics$SWA[which(sim_metrics$SWA$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 0.7, pos = 4)
  #panel label
  text(x = -4, y = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.30), labels = "B", col="black", font = 2, cex = 2.5, pos = 4, offset = 1)
  #legend inside the plot border
  text(x = 354, y = max(PlotLimits*0.95), labels = "Soil Water Availability:", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 354, y = max(PlotLimits*0.89), labels = "10th and 90th Percentiles", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 285, y = max(PlotLimits*0.95), labels = "Soil Water Potential:", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 230, y = max(PlotLimits*0.89), labels = "shallow", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 277, y = max(PlotLimits*0.89), labels = "deep soils", col="black", font = 1, cex = 0.6, pos = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = SWA_COLOR, lwd = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*0.89), max(PlotLimits*0.89)), type = "l", col = SWA_COLOR, lwd = 2, lty = 3)
  lines(x = c(226,231), y = c(max(PlotLimits*0.89), max(PlotLimits*0.89)), type = "l", col = soilCOLORS[5], lwd = 2)
  lines(x = c(273,278), y = c(max(PlotLimits*0.89), max(PlotLimits*0.89)), type = "l", col = soilCOLORS[2], lwd = 2)
  #arrow between soil legend elements
  arrows(x0 = 235, x1 = 243, y0 = max(PlotLimits*0.89), y1 = max(PlotLimits*0.89), length = 0.03, code = 3, col = "black", lwd = 0.6)
  
  #~~~~~~~~~ Climate and soil water variables ~~~~~~~~~~
  par(mar=c(0, 0, 0, 0.5), xpd = NA)
  x <- c(1:5)
  y <- c(1:5)
  plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1)

  rect(xleft = max(x*0.15), ybottom = max(x*0.88), xright = max(x*0.60), ytop = max(x), border = NA, col = alpha(temperatureCOLOR, 0.25))
  mtext("Mean Annual\nTemperature", at = max(x*0.19), side=3, adj=0, line=-0.8, col="black", font=1, padj = 1, cex = 0.5)
  rect(xleft = max(x*0.61), ybottom = max(x*0.88), xright = max(x), ytop = max(x), border = NA, col = alpha(temperatureCOLOR, 0.25))
  mtext(paste(round(mean(sim_data$clim$values$tmean), 1), "°C"), at = max(x*0.63), side=3, adj=0, line=-1.2, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.15), ybottom = max(x*0.75), xright = max(x*0.60), ytop = max(x*0.87), border = NA, col = alpha(precipCOLOR, 0.25))
  mtext("Mean Annual\nPrecipitation", at = max(x*0.19), side=3, adj=0, line=-2.6, col="black", font=1, padj = 1, cex = 0.5)
  rect(xleft = max(x*0.61), ybottom = max(x*0.75), xright = max(x), ytop = max(x*0.87), border = NA, col = alpha(precipCOLOR, 0.25))
  mtext(paste(round(mean(PPT_eachyear$x)*10, 0), "mm"), at = max(x*0.63), side=3, adj=0, line=-2.8, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.15), ybottom = max(x*0.66), xright = max(x), ytop = max(x*0.74), border = NA, col = "gray90")
  mtext("Dry Soil Intervals", at = max(x*0.19), side=3, adj=0, line=-4.4, col="black", font=2, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.15), ybottom = max(x*0.44), xright = max(x*0.55), ytop = max(x*0.65), border = NA, col = "gray90")
  rect(xleft = max(x*0.56), ybottom = max(x*0.44), xright = max(x), ytop = max(x*0.65), border = NA, col = "gray90")
  mtext("mean interval\nduration", at = max(x*0.19), side=3, adj=0, line=-5.7, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(round(mean(as.numeric(DSI_Lengths)), 1), "days/year"), at = max(x*0.58), side=3, adj=0, line=-5.7, col="black", font=1, padj = 1, cex = 0.5)
  mtext("mean number", at = max(x*0.19), side=3, adj=0, line=-7.3, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(round(mean(as.numeric(DSI_Number)), 1), "/year"), at = max(x*0.58), side=3, adj=0, line=-7.3, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.15), ybottom = max(x*0.33), xright = max(x), ytop = max(x*0.43), border = NA, col = alpha(SWA_COLOR, 0.15))
  mtext("Soil Water Availability", at = max(x*0.19), side=3, adj=0, line=-8.9, col="black", font=2, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.15), ybottom = max(x*0.15), xright = max(x*0.52), ytop = max(x*0.32), border = NA, col = alpha(SWA_COLOR, 0.15))
  rect(xleft = max(x*0.53), ybottom = max(x*0.15), xright = max(x), ytop = max(x*0.32), border = NA, col = alpha(SWA_COLOR, 0.15))
  mtext("mean", side=3, adj=0, line=-10.2, col="black", font=1, padj = 1, cex = 0.5, at = max(x*0.19))
  mtext(paste(round(mean(SWA_eachyear[,2]) ,1), "mm/day"), at = max(x*0.55), side=3, adj=0, line=-10.2, col="black", font=1, padj = 1, cex = 0.5)
  mtext("monthly CV ", at = max(x*0.19), side=3, adj=0, line=-11.2, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(round(mean(as.numeric(sim_metrics$SWA[which(sim_metrics$SWA$group=="seasonal_variability"), -c(1,2)])), 2)), at = max(x*0.55), side=3, adj=0, line=-11.2, col="black", font=1, padj = 1, cex = 0.5)
  
  #-------------------------
  #-------- PLOT 3 ---------
  
  #~~~~~~~~ PET, ET and CWD plot ~~~~~~~~
  par(mar=c(0, 3.5, 0, 3.5), xpd = FALSE)
  ETLimits <- c(0, max(PET_rollMeanyear$x)) * 10
  PlotLimits <- ETLimits*1.2
  CWDrollmean_10day <- rollmean(sim_metrics_raw$CWD[[1]]$values$cwd, 10, fill=NA)
  CWDrollmean_10dayMAX <- aggregate(CWDrollmean_10day, by=list(sim_metrics_raw$CWD[[1]]$time[, "Year"]), max, na.rm=TRUE)$x
  CWDrollmean_10dayMAX_DOY <- sim_metrics_raw$CWD[[1]]$time[match(CWDrollmean_10dayMAX, CWDrollmean_10day), "Day"]
  meanDAYs <- seq(round(mean(CWDrollmean_10dayMAX_DOY), 0)-4, round(mean(CWDrollmean_10dayMAX_DOY), 0)+5) 
  plot(AET_rollMeanyear$x*10 ~ xValues, type="n", xlab="Day of Year", ylab="", col="darkgreen", xaxt='n', yaxt = "n", ylim=PlotLimits, las=1,  xaxs = "i", yaxs = "i")
  mgp.axis(2, col="black",  col.axis="black", las=1, at=pretty(PlotLimits), cex.axis = 1, mgp = c(1,2,0))
  mtext("mm/day", side=2, line=1.8, font=2, adj = 0, cex = 0.6)
  
  axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n")
  abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
  polygon(x=c(xValues, rev(xValues)), y=c(PET_rollMeanyear$x*10, rev(AET_rollMeanyear$x*10)), col=CWD_COLOR, border = NA)
  polygon(x=c(xValues, rev(xValues)), y=c(AET_rollMeanyear$x*10, rep(0, 365)), col=alpha(AET_COLOR, 0.5), border = NA)
  lines(PET_rollMeanyear$x*10 ~ xValues, col=PET_COLOR, lwd=2.5)
  lines(AET_rollMeanyear$x*10 ~ xValues, col=alpha(SWA_COLOR, 0.8), lwd=1)

  lines(AET_rollpercentiles[,1]*10 ~ xValues, col=alpha(SWA_COLOR, 1), lwd=1, lty = 3)
  lines(AET_rollpercentiles[,2]*10 ~ xValues, col=alpha(SWA_COLOR, 1), lwd=1, lty = 3)
  
  abline(v=as.Date(c(mean(CWDrollmean_10dayMAX_DOY)-4, mean(CWDrollmean_10dayMAX_DOY)+5), origin = "2019-12-31"), lty=2, col="#9B2226", lwd = 0.8)
  
  #~~~~~~~~~~ Plot labels ~~~~~~~~~~
  #white box background
  rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = xValues[350], ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
  #panel title
  rect(xleft = xValues[1], xright = xValues[77], ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = xValues[1], y = max(PlotLimits*0.89), labels = "Potential & Actual\nEvapotranspiration", col="black", font = 2, cex = 0.8, pos = 4)
  ##correlation with temp box
  rect(xleft = xValues[77], xright = xValues[192], ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = xValues[75], y = max(PlotLimits*0.89), labels = "Monthly Climatic Water Deficit\nCorr. with Temperature", col="black", cex = 0.7, pos = 4)
  text(x = xValues[165], y = max(PlotLimits*0.89), labels = paste("= ", round(mean(as.numeric(sim_metrics$CWD[which(sim_metrics$CWD$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 0.7, pos = 4)
  #panel label
  text(x = xValues[10], y = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.30), labels = "C", col="black", font = 2, cex = 2.5, pos = 2, offset = -1.25)
  #legend inside the plot border
  text(x = xValues[354], y = max(PlotLimits*0.95), labels = "Actual Evapotranspiration:", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.89), labels = "10th and 90th Percentiles", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.83), labels = "Climatic Water Deficit", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.77), labels = "Potential Evapotranspiration", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[260], y = max(PlotLimits*0.89), labels = "Climatic Water Deficit\n10-Day Max Timing", col="black", font = 1, cex = 0.6, pos = 2)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = AET_COLOR, lwd = 2)
  lines(x = c(xValues[352],xValues[362]), y = c(max(PlotLimits*0.89), max(PlotLimits*0.89)), type = "l", col = alpha(SWA_COLOR, 0.8), lwd = 1.5, lty = 3)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.83), max(PlotLimits*0.83)), type = "l", col = CWD_COLOR, lwd = 2)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.77), max(PlotLimits*0.77)), type = "l", col = PET_COLOR, lwd = 2)
  lines(x = c(xValues[258],xValues[270]), y = c(max(PlotLimits*0.91), max(PlotLimits*0.91)), type = "l", col = "#9B2226", lwd = 1.5, lty = 2)
  
  #~~~~~~~~~ Variables ~~~~~~~~~~
  par(mar=c(0, 0, 0, 0.5), xpd = NA)
  x <- c(1:5)
  y <- c(1:5)
  plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1,  xaxs = "i", yaxs = "i")
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.85), xright = max(x*0.97), ytop = max(x*0.95), border = NA, col = alpha(PET_COLOR, 0.4))
  mtext("Evapotranspiration", at = max(x*-0.02), side=3, adj=0, line=-1.4, col="black", font=2, padj = 1, cex = 0.5)
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.68), xright = max(x*0.23), ytop = max(x*0.84), border = NA, col = alpha(PET_COLOR, 0.4))
  rect(xleft = max(x*0.24), ybottom = max(x*0.68), xright = max(x*0.97), ytop = max(x*0.84), border = NA, col = alpha(PET_COLOR, 0.4))
  mtext(paste("potential      ", round(mean(PET_eachyear$x) ,1), "mm/year"), at = max(x*-0.02), side=3, adj=0, line=-2.8, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste("actual          ", round(mean(AET_eachyear$x) ,1), "mm/year"), at = max(x*-0.02), side=3, adj=0, line=-3.8, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.57), xright = max(x*0.97), ytop = max(x*0.67), border = NA, col = alpha(CWD_COLOR, 0.4))
  mtext("Climatic Water Deficit", at = min(x*-0.02), side=3, adj=0, line=-5.5, col="black", font=2, padj = 1, cex = 0.5)
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.33), xright = max(x*0.28), ytop = max(x*0.56), border = NA, col = alpha(CWD_COLOR, 0.4))
  rect(xleft = max(x*0.29), ybottom = max(x*0.33), xright = max(x*0.97), ytop = max(x*0.56), border = NA, col = alpha(CWD_COLOR, 0.4))
  mtext(paste("mean              ", round(mean(CWD_eachyear$cwd/10) ,1), "mm/year"), at = min(x*-0.02), side=3, adj=0, line=-7, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste("monthly CV     ", round(mean(as.numeric(sim_metrics$CWD[which(sim_metrics$CWD$group=="seasonal_variability"), -c(1,2)])), 2)), at = min(x*-0.02), side=3, adj=0, line=-8, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste("10-day max    ", round(mean(CWDrollmean_10dayMAX), 2), "mm/day"), at = min(x*-0.02), side=3, adj=0, line=-9, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.2), xright = max(x*0.97), ytop = max(x*.30), border = NA, col = "gray85")
  mtext("Total Growing Degree Days ", at = min(x*-0.03), side=3, adj=0, line=-11, col="black", font=2, padj = 1, cex = 0.5)
  
  #-----------------------
  #------- PLOT 4 --------
  
  #~~~~~~~ Degree day plot ~~~~~~~~
  par(mar=c(0, 3.5, 0, 3.5), xpd = FALSE)
  DDLimits <- c(min(TDD_rollMeanyear$x), max(TDD_rollMeanyear$x))
  PlotLimits <- DDLimits*1.2
  plot(TDD_rollMeanyear$x~ xValues, type="n", xlab="", ylab="", xaxt='n',yaxt = "n", las=1,  xaxs = "i", yaxs = "i", ylim = PlotLimits)
  mgp.axis(2, col="black",  col.axis="black", las=1, at=pretty(PlotLimits), cex.axis = 1, mgp = c(1,2,0))
  axis(1, at=pretty(xValues, 12), labels=FALSE)
  abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
  mtext("degree days", side=2, line=1.8, font=2, adj = 0, cex = 0.6)
  polygon(x=c(xValues, rev(xValues)), y=c(TDD_rollMeanyear$x, rep(0, 365)), col=neutralCOLOR, border = NA)
  polygon(x=c(xValues, rev(xValues)), y=c(TDD_rollMeanyear$x - DDD_rollMeanyear$x, rev(TDD_rollMeanyear$x)), col=dryCOLOR, border = NA)
  lines(WDD_rollpercentiles[,1] ~ xValues, col=alpha(wetCOLOR, 1), lwd=1, lty = 3)
  lines(WDD_rollpercentiles[,2] ~ xValues, col=alpha(wetCOLOR, 1), lwd=1, lty = 3)
  
  polygon(x=c(xValues, rev(xValues)), y=c(WDD_rollMeanyear$x, rep(0, 365)), col=alpha(wetCOLOR, 0.75), border = NA)
  lines(TDD_rollMeanyear$x~ xValues, col="black", lwd=2.5 )
  
  TDDmean <- round(mean(as.numeric(sim_metrics$TDD[1, -c(1,2)]), 2))
  DDDmean <- round(mean(as.numeric(sim_metrics$DDD[1, -c(1,2)]), 2))
  WDDmean <- round(mean(as.numeric(sim_metrics$WDD[1, -c(1,2)]), 2))
  NDDmean <- TDDmean - DDDmean - WDDmean
  
  springRECdays <- round(c(mean(SpringRecruitment_DOY, na.rm=TRUE), mean(SpringRecruitment_DOY, na.rm=TRUE) + mean(SpringRecruitment_DurationDays, na.rm=TRUE)), 0)
  springRECdates <- as.Date(springRECdays ,  origin = "2019-12-31")
  fallRECdays <- round(c(mean(FallRecruitment_DOY, na.rm=TRUE), mean(FallRecruitment_DOY, na.rm=TRUE) + mean(FallRecruitment_DurationDays, na.rm=TRUE)), 0)
  fallRECdates <- as.Date(fallRECdays ,  origin = "2019-12-31")
  
  rect(xleft = springRECdates[1], xright = springRECdates[2], ybottom = 0, ytop = max(PlotLimits), col = alpha("forestgreen", 0.1), border = "black", lty = 2, lwd = 0.6)
  rect(xleft = fallRECdates[1], xright = fallRECdates[2], ybottom = 0, ytop = max(PlotLimits), col = alpha("forestgreen", 0.1), border = "black", lty = 2, lwd = 0.6)
  
  #~~~~~~~~~~ Plot labels ~~~~~~~~~~
  #white box background
  rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = xValues[350], ytop = max(PlotLimits), col = alpha("white", 0.8), border = NA, lwd = 1.5, lty = 2)
  #panel title 
  rect(xleft = 1, xright = xValues[55], ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.5))
  text(x = xValues[1], y = max(PlotLimits*0.9), labels = "Wet & Dry\nDegree Days", col="black", font = 2, cex = 0.8, pos = 4)
  #correlation with temp box
  rect(xleft = xValues[55], xright = xValues[160], ybottom = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.18), ytop = max(PlotLimits), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = xValues[52], y = max(PlotLimits*0.9), labels = "Monthly Wet Degree Days\nCorr. with Temperature", col="black", cex = 0.7, pos = 4)
  text(x = xValues[130], y = max(PlotLimits*0.9), labels = paste(" = ", round(mean(as.numeric(sim_metrics$WDD[which(sim_metrics$WDD$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 0.7, pos = 4)
  #panel label
  text(x = xValues[10], y = (max(PlotLimits)-(max(PlotLimits)-min(PlotLimits))*0.30), labels = "D", col="black", font = 2, cex = 2.5, pos = 2, offset = -1.25)
  #legend inside the plot border
  text(x = xValues[354], y = max(PlotLimits*0.95), labels = "Wet Degree Days:", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.89), labels = "10th and 90th Percentiles", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.83), labels = "Dry Degree Days", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[354], y = max(PlotLimits*0.77), labels = "Neutral Degree Days", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[260], y = max(PlotLimits*0.95), labels = "Total Degree Days", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = xValues[260], y = max(PlotLimits*0.89), labels = "Recruitment Period", col="black", font = 1, cex = 0.6, pos = 2)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = wetCOLOR, lwd = 2)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.89), max(PlotLimits*0.89)), type = "l", col = wetCOLOR, lwd = 2, lty = 3)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.83), max(PlotLimits*0.83)), type = "l", col = dryCOLOR, lwd = 2)
  lines(x = c(xValues[352],xValues[360]), y = c(max(PlotLimits*0.77), max(PlotLimits*0.77)), type = "l", col = neutralCOLOR, lwd = 2)
  lines(x = c(xValues[261],xValues[271]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = "black", lwd = 2)
  rect(xleft = xValues[261], ybottom = max(PlotLimits*0.88), xright = xValues[271], ytop = max(PlotLimits*0.92), col = alpha("forestgreen", 0.15), border = "black", lwd = 0.5, lty = 2)
  
  #~~~~~~~~~ Variables ~~~~~~~~~~
  par(mar=c(0, 0, 0, 0.5), xpd = NA)
  x <- c(1:5)
  y <- c(1:5)
  plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1, xaxs = "i", yaxs = "i")
  
  rect(xleft = min(x*-0.05), ybottom = max(x*0.61), xright = max(x*0.52), ytop = max(x*0.99), border = NA, col = "gray85")
  rect(xleft = max(x*0.53), ybottom = max(x*0.61), xright = max(x*0.97), ytop = max(x*0.99), border = NA, col = "gray85")
  
  mtext(paste("mean"), at = min(x*-0.02), side=3, adj=0, line=-0.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(TDDmean, "dd/year"), at = max(x*0.55), side=3, adj=0, line=-0.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext("monthly CV", at = min(x*-0.02), side=3, adj=0, line=-1.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(round(mean(as.numeric(sim_metrics$TDD[which(sim_metrics$TDD$group=="seasonal_variability"), -c(1,2)])), 2)), at = max(x*0.55), side=3, adj=0, line=-1.6, col="black", font=1, padj = 1, cex = 0.5)
  
  mtext(paste("dry degree days"), at = min(x*-0.02), side=3, adj=0, line=-2.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste("wet degree days"), at = min(x*-0.02), side=3, adj=0, line=-3.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste("neutral degree days"), at = min(x*-0.02), side=3, adj=0, line=-4.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(DDDmean, "ddyear"), at = max(x*0.55), side=3, adj=0, line=-2.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(WDDmean, "dd/year"), at = max(x*0.55), side=3, adj=0, line=-3.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(NDDmean, "dd/year"), at = max(x*0.55), side=3, adj=0, line=-4.6, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = min(x*-0.05), ybottom = min(x), xright = max(x*0.41), ytop = max(x*0.51), border = NA, col = alpha("forestgreen", 0.25))
  mtext("recruitment max\nwet degree days", at = min(x*-0.02), side=3, adj=0, line=-8.2, col="black", font=1, padj = 1, cex = 0.5)
  mtext("% years\nwith recruitment", at = min(x*-0.02), side=3, adj=0, line=-10.25, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.42), ybottom = max(x*0.52), xright = max(x*0.69), ytop = max(x*0.60), border = NA, col = alpha("forestgreen", 0.25))
  rect(xleft = max(x*0.70), ybottom = max(x*0.52), xright = max(x*0.97), ytop = max(x*0.60), border = NA, col = alpha("forestgreen", 0.25))
  mtext("spring", at = max(x*0.44), side=3, adj=0, line=-6.5, col="black", font=1, padj = 1, cex = 0.5)
  mtext("fall", at = max(x*0.72), side=3, adj=0, line=-6.5, col="black", font=1, padj = 1, cex = 0.5)
  
  rect(xleft = max(x*0.42), ybottom = min(x), xright = max(x*0.69), ytop = max(x*0.51), border = NA, col = alpha("forestgreen", 0.25))
  rect(xleft = max(x*0.70), ybottom = min(x), xright = max(x*0.97), ytop = max(x*0.51), border = NA, col = alpha("forestgreen", 0.25))
  mtext(paste(round(mean(SpringRecruitment_maxWDD, na.rm=TRUE), 0), "dd"), at = max(x*0.44), side=3, adj=0, line=-8.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste((sum(!is.na(SpringRecruitment_maxWDD))/length(SpringRecruitment_maxWDD)*100), "%", sep = ""), at = max(x*0.44), side=3, adj=0, line=-10.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste(round(mean(FallRecruitment_maxWDD, na.rm=TRUE), 0), "dd"), at = max(x*0.72), side=3, adj=0, line=-8.6, col="black", font=1, padj = 1, cex = 0.5)
  mtext(paste((sum(!is.na(FallRecruitment_maxWDD))/length(FallRecruitment_maxWDD)*100), "%", sep = ""), at = max(x*0.72), side=3, adj=0, line=-10.6, col="black", font=1, padj = 1, cex = 0.5)
  
  #---------------------
  #------- ROW 6: ------
  #empty plot with month x-axis
  par(mar=c(0, 3.5, 0, 3.5), xpd = FALSE)
  
  plot(c(1:13), xaxt = "n", yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = "", xaxs = "i", yaxs = "i")
  mtext(at = c(1.25:12.25), line = -1, month.abb[seq(1,12)], font = 2, padj = 0, adj = 0, cex = 0.6)
  
  sf::sf_use_s2(FALSE)
  
  plot.new()
  
  par(mar = c(0.5,0,0,0.5))
  plot(st_geometry(st_transform(ms_simplify(extent_ecoregions, keep = 0.01, keep_shapes = TRUE), st_crs(ppts_hist))), 
       xlim = c(st_bbox(extent_mask)[1] + 0.5, st_bbox(extent_mask)[3]), 
       ylim = c(st_bbox(extent_mask)[2] + 1, st_bbox(extent_mask)[4]),
       lwd = 0.15, col = alpha(extent_ecoregions$newcolor, 0.25), border = NA)
  box(lwd = 0.25)
  
  plot(st_geometry(st_transform(ms_simplify(states_mask, keep = 0.01), st_crs(ppts_hist))), lwd = 0.2, add = T)
  plot(st_geometry(st_transform(ms_simplify(provinces_mask, keep = 0.01), st_crs(ppts_hist))), lwd = 0.2, add = T)
 # par(new = TRUE)
  
  #plot(st_geometry(st_transform(ms_simplify(extent_ecoregions[extent_ecoregions$newname == ecoregion_label,], keep = 0.01, keep_shape = TRUE), st_crs(ppts_hist))), add = T, lwd = 0.1, col = extent_ecoregions$newcolor[extent_ecoregions$newname == ecoregion_label][[1]], border = "black")
  
  points(repsitenames$Y_WGS84[repsitenames$ecoregion == ecoregion_label] ~ repsitenames$X_WGS84[repsitenames$ecoregion == ecoregion_label], xlim = c(-123.125,-96.0625), ylim = c(29.5, 49), col = "black", pch = 16, cex = 1)
  
  quartz.save(file=file.path(dir_prj, paste0("MeanMetrics_", sitename, ".jpg")), type = "jpg", device = dev.cur(), dpi = 300)
  dev.off()
  
} 


make.site.figure(sitename = "002544_HotDeserts_Site_2544", repsitenames = repsitenames_ecoregions, color.set.default)



sitename <- "002544_HotDeserts_Site_2544"



install.packages("ggplot2")
install.packages("pryr")
install.packages("devtools")
devtools::install_github("hadley/lineprof")




siteNames <- c("02468_CaliforniaAnnuals_Site_2468", 
               #"002544_HotDeserts_Site_2544",
               "047771_Western_Gap_Site_47771",
               "001373_ColdDeserts_Site_1373",
               "010371_Western_Gap_Site_10371",
               "014441_ColdDeserts_Site_14441",
               "007843_HotDeserts_Site_7843",
               "015490_NorthernMixedSubset_Site_15490",
               "029128_Western_Gap_Site_29128",
               "051626_Western_Gap_Site_51626",
               "030639_Western_Gap_Site_30639",
               "005743_ColdDeserts_Site_5743")

for (s in siteNames){
  
  makeFigure1site(sitename=s, repsitenames = repsitenames)
  
}








plot.site.climate(sitename = "002544_HotDeserts_Site_2544")

plot.site.climate <- function(sitename = NULL) {
  
  extract_list <- extract_metrics(sitename)
  
  Tmean_Meanyear <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Day"]), mean)
  
  sim_data <- extract_list[[1]]
  sim_metrics <- extract_list[[2]]
  sim_metrics_raw <- extract_list[[3]]
  used_soil <- extract_list[[4]]
  
  #extract soilwat output to get objects for each of the variables
  day_list <- day.mean.format(extract_list)
  for(i in 1:length(day_list)[1]){
    if (grepl("percentiles", names(day_list[i]))) {
      assign(names(day_list[i]), as.data.frame(day_list[[i]][,c(-3)]))
    } else {
      assign(names(day_list[i]), as.numeric(day_list[[i]][,2]))
    }
    
  } 
  
  #################################
  
  #-------- ROW 1: Title --------
  x = c(1,2)
  y = c(1,2)
  par(mar=c(0, 3.2, 0, 1), xpd = FALSE)
  
  ecoregion_label <- repsitenames$ecoregion[repsitenames$site == label]
  plot(y ~ x, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', xaxs = "i", yaxs = "i")
  mtext(ecoregion_label[1], at = c(1.01), col="black", font=2, cex=1, padj = 0, adj = 0, line = -2.2)
  
  #Use Tmean_Meanyear to get dates for 365 days of the year; xValues will be used to plot the x-axis; row 60 (leap day) is removed and ignored
  xValues <- as.Date(Tmean_Meanyear$Group.1,  origin = "2019-12-31")[-60]
  #-------------------------
  #-------- PLOT 1 ---------
  #~~~~~~~ Climate plot (temp axis) ~~~~~~~
  par(mar=c(0, 3.5, 0, 3.5), xpd = FALSE)
  
  TempLimits <- c(min(c(Tmin_rollMeanyear,0)), max(c(Tmean_rollMeanyear), max(PPT_rollMeanyear*10*30/2)))
  PlotLimits <- TempLimits*1.2
  plot(Tmean_rollMeanyear ~ xValues, type="l", xlab="", yaxt='n',  xaxt='n', ylab="", ylim=PlotLimits, col=temperatureCOLOR, lwd=1, xaxs = "i", yaxs = "i")
  axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n", cex = 12)
  mgp.axis(2, col="black", at=pretty(c(TempLimits[1], min(TempLimits[2], 50))), col.axis="black", las=1, cex.axis = 1, mgp = c(1,2,0))
  abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
  mtext("Daily Temperature (°C)", side=2, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  lines(Tmin_rollMeanyear~ xValues, lwd=1, col=mintemperatureCOLOR)
  
  #~~~~~~~~ Climate plot (precipitation axis) ~~~~~~~~
  par(new=TRUE)
  
  plot(PPT_rollMeanyear*10, ylim=PlotLimits*2/30, type="l", axes=FALSE, bty = "n", xlab = "", ylab = "", lwd=1, col=precipCOLOR,  xaxs = "i", yaxs = "i")
  mgp.axis(4, col="black", at=pretty(c(0, TempLimits[2]*2/30)), col.axis="black", las=1, cex.axis = 1, mgp = c(1,2,0))
  mtext("Mean Daily Precipitation (mm)" , side=4, line=1.8, col="black", font=2, adj = 0, cex = 0.6)
  abline(h=0, lwd=0.5, lty=2)
  
  #~~~~~~~~~~ Plot labels ~~~~~~~~~~
  #white background box
  rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = 350, ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
  #panel title
  rect(xleft = 1, xright = 62, ybottom = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.18), ytop = max(PlotLimits*2/30), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 1, y = max(PlotLimits*2/30*0.89), labels = "Temperature &\nPrecipitation", col="black", font = 2, cex = 0.8, pos = 4)
  #correlation with temp box
  rect(xleft = 62, xright = 165, ybottom = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.18), ytop = max(PlotLimits*2/30), border = "black", lwd = 0.5, col = alpha("gray92", 0.6))
  text(x = 62, y = max(PlotLimits*2/30*0.89), labels = "Monthly Precipitation\nCorr. with Temperature", col="black", cex = 0.7, pos = 4)
  text(x = 133, y = max(PlotLimits*2/30*0.89), labels = paste("= ",round(mean(as.numeric(sim_metrics$PPTseas[-c(1,2)])), 2)), col="black", cex = 0.7, pos = 4)
  #panel label
  text(x = -4, y = (max(PlotLimits*2/30)-(max(PlotLimits*2/30)-min(PlotLimits*2/30))*0.30), labels = "A", col="black", font = 2, cex = 2.5, pos = 4, offset = 1)
  #legend inside the plot border
  text(x = 354, y = max(PlotLimits*2/30*0.95), labels = "Mean Daily Temperature", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 354, y = max(PlotLimits*2/30*0.89), labels = "Minimum Daily Temperature", col="black", font = 1, cex = 0.6, pos = 2)
  text(x = 354, y = max(PlotLimits*2/30*0.83), labels = "Mean Daily Precipitation", col="black", font = 1, cex = 0.6, pos = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.95), max(PlotLimits*2/30*0.95)), type = "l", col = temperatureCOLOR, lwd = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.89), max(PlotLimits*2/30*0.89)), type = "l", col = mintemperatureCOLOR, lwd = 2)
  lines(x = c(352,360), y = c(max(PlotLimits*2/30*0.83), max(PlotLimits*2/30*0.83)), type = "l", col = precipCOLOR, lwd = 2)
}





































