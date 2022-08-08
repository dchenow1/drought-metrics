
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
#
#--- Inputs ------
library(remotes)
library(zoo)
library(scales)
library(usethis)
library(memisc)
library(sf)
#library(rSW2metrics)
usethis::create_github_token()
usethis::edit_r_environ()

GITHUB_PAT <- "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"

remotes::install_github("dchenow1/rSW2metrics")


  
sitename <- "000006_NorthernMixedSubset_Site_6"
sitename <- "02468_CaliforniaAnnual_Site_2468"
sitename <- "010371_Western_Gap_Site_10371"



  
  
#--------- Set directory ----------
  
dir_prj <- "/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/conceptual_figures"
dir_data <- file.path(dir_prj, "data")
dir_sim <- file.path(dir_data, "3_Runs")  
  
  
dev.off()

#take out all the function defining in the beginning and make the functions default in the workspace

#--------- Define function for drawing conceptual figure  ----------

makeFigure1site <- function(sitename, label=NULL, repsitenames = NULL){

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
    
    
    
    #-------- Save objects --------
    if (FALSE) {
      save(
        sim_data,
        sim_metrics,
        sim_metrics_raw,
        used_soil,
        file = file.path(dir_data, "Metrics", paste0(name_sw2_run, ".rda"))
      )
    }
    
    ############################################################################################################################################
    
    
    #str(sim_data)
    #str(sim_metrics)
    #str(sim_metrics_raw)
    #str(used_soil)
    
    #mean(as.numeric(sim_metrics$PPTseas[-c(1,2)]))
    
    #------- Reformat data for a mean year -------

    Tmean_Meanyear <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Day"]), mean)
    #plot(Tmean_Meanyear, type="l")
    Tmin_Meanyear <- aggregate(sim_data$clim$values$tmin, list(sim_data$clim$time[, "Day"]), mean)
    
    PPT_Meanyear <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Day"]), mean)

    AET_Meanyear <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Day"]), mean)
    PET_Meanyear <- aggregate(sim_data$et$values$pet, list(sim_data$et$time[, "Day"]), mean)
    SWP_Meanyear <- aggregate(sim_data$swp$values, list(sim_data$swp$time[, "Day"]), mean)
    #plot(SWP_Meanyear$swp.Lyr_1, type="l")
    
    TDD_Meanyear <- aggregate(sim_metrics_raw$TDD[[1]]$values$mdd, list(sim_metrics_raw$TDD[[1]]$time[, "Day"]), mean)
    #plot(TDD_Meanyear, type="l")
    WDD_Meanyear <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Day"]), mean)
    #plot(WDD_Meanyear, type="l")
    DDD_Meanyear <- aggregate(sim_metrics_raw$DDD[[1]]$values$mdd, list(sim_metrics_raw$DDD[[1]]$time[, "Day"]), mean)
    #plot(DDD_Meanyear, type="l")
    
    SWA_Meanyear <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Day"]), mean)
    # plot(SWA_Meanyear, type="l")
    CWD_Meanyear <- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Day"]), mean)
    # plot(CWD_Meanyear, type="l")
  
    FirstFrost_Mean <- mean(sim_metrics_raw$Frost[[1]]["FirstFrost", ]) #or could use sim_metrics$Frost[which(sim_metrics$Frost$group=="FirstFrost"), -c(1,2)]
    LastFrost_Mean <- mean(sim_metrics_raw$Frost[[1]]["LastFrost", ]) #or could use sim_metrics$Frost[which(sim_metrics$Frost$group=="LastFrost"), -c(1,2)]
    PPTseas_Mean <- mean(sim_metrics_raw$PPTseas[[1]]["seasonality", ])
    DSI_Lengths <- sim_metrics$DSI[which(sim_metrics$DSI$group=="mean"), -c(1,2)]
    DSI_Number <- sim_metrics$DSI[which(sim_metrics$DSI$group=="N"), -c(1,2)]
    
    Recruitment <- sim_metrics$Recruitment #[which(sim_metrics$Recruitment$group=="N"), -c(1,2)]
    for(v in 1:dim(Recruitment)[1]){
      assign(paste(Recruitment$group[v]), as.numeric(Recruitment[v, -c(1,2)]))
      #print(Recruitment$group[v])
    } 
    
    percentiles <- function(x){
      quantile(x, c(0.1, 0.9))
    }
    
    SWA_10and90per <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Day"]), percentiles)[2][[1]]
    AET_10and90per <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Day"]),  percentiles)[2][[1]]
    WDD_10and90per <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Day"]), percentiles)[2][[1]]

    
    #-------------------------------------
    
    #------- Reformat monthly data -------
    
    Tmean_Mean_monthly <- aggregate(sim_data$clim$values$tmean, list(sim_data$clim$time[, "Month"]), mean)
    WDD_Mean_monthly <- aggregate(sim_metrics_raw$WDD[[1]]$values$mdd, list(sim_metrics_raw$WDD[[1]]$time[, "Month"]), mean)
    SWA_Mean_monthly <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Month"]), mean)
    CWD_Mean_monthly <- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Month"]), mean)
    PPT_Mean_monthly <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Month"]), mean)
    
    PET_eachyear <- aggregate(sim_data$et$values$pet, list(sim_data$et$time[, "Year"]), sum)
    AET_eachyear <- aggregate(sim_data$et$values$et, list(sim_data$et$time[, "Year"]), sum)
    
    CWD_eachyear<- aggregate(sim_metrics_raw$CWD[[1]]$values, list(sim_metrics_raw$CWD[[1]]$time[, "Year"]), sum)
    SWA_eachyear <- aggregate(sim_metrics_raw$SWA[[1]]$values, list(sim_metrics_raw$SWA[[1]]$time[, "Year"]), mean)
    PPT_eachyear <- aggregate(sim_data$clim$values$ppt, list(sim_data$clim$time[, "Year"]), sum)
    
    xValues <- as.Date(Tmean_Meanyear$Group.1,  origin = "2019-12-31")   
    #-------------------------------------
    
    
    #------- Set colors based on variables ---------
    temperatureCOLOR <- "#9B2226"
    mintemperatureCOLOR <- "#CA6702"
    precipCOLOR <- "#005F73"
    
    soilCOLORS <- viridis::cividis(6)
    SWA_COLOR <- "#0A9396"
    DSI_COLOR <- "#001219"
    
    PET_COLOR <- "#EE9B00"
    AET_COLOR <- "#94D2BD"
    CWD_COLOR <- "#E9D8A6"
    
    dryCOLOR <- "#CA6702"
    wetCOLOR <- "#005F73"
    neutralCOLOR <- "#E9D8A6"
    #-------------------------------------
    
    
    #------ Make plot  layout ------
    #1st and 6th row are buffer (title and x axis) so that panels can be the same height
    #right-hand column only contains legends and values
    lay = matrix(c(rep(c(1, 2, seq(2, 10, by = 2)), 2), c(12, 12, seq(3, 11, by = 2), 12, 12, 3, seq(5, 11, 2))), nrow=7, ncol=4)
    quartz(width=13, height=15)
    layout(lay,
           heights = c(0.5,1.5,0.5,2,2,2,0.5),
           widths = c(2,2,0.5,0.5))
    
    ############################################################################################################################################
    
    #------ Begin writing plots -------
    

    #-------- ROW 1: Title --------
    x = c(1,2)
    y = c(1,2)
    par(mar=c(0, 5, 0, 4))
    
    ecoregion_label = repsitenames$ecoregion[repsitenames$site == label]
    plot(y ~ x, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', xaxs = "i", yaxs = "i")
    mtext(ecoregion_label, at = c(1), col="black", font=2, cex=1.5, padj = 0, adj = 0, line = -5)
    
    
    #-------- PLOT 1 ---------
    #~~~~~~~ Climate plot ~~~~~~~
    
    par(mar=c(0, 5, 0, 4))
    
    TempLimits <- c(min(c(Tmean_Meanyear$x), 0), max(c(Tmean_Meanyear$x), max(PPT_Meanyear$x*10*30/2)))
    PlotLimits <- TempLimits*1.2
    plot(Tmean_rollMeanyear$x ~ xValues, type="l", xlab="", yaxt='n',  xaxt='n', ylab="", ylim=PlotLimits, col=temperatureCOLOR, lwd=2, xaxs = "i", yaxs = "i")
    #lines(rollmean(Tmean_Meanyear$x, 30, fill=NA), type="l", xlab="Day of Year", yaxt='n', ylab="", ylim=TempLimits, col="brown", lwd=2)
    axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n")
    axis(2, col="black", at=pretty(c(TempLimits[1], min(TempLimits[2], 50))), col.axis="black", las=1)
    abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
    #mtext(label, side=3, line=0, col="black", font=2, cex=1.5)
    mtext("Daily Temperature (*C)", side=2, line=2.5, col="black", font=2, adj = 0)
    lines(Tmin_Meanyear$x~ xValues, lwd=2, col=mintemperatureCOLOR)
    #lines(rollmean(Tmin_Meanyear$x, 30, fill=NA), lwd=2, col="purple")
    
    #~~~~~~~~ Climate plot precipitation axis ~~~~~~~~
    
    par(new=TRUE)
   
    plot(PPT_Meanyear$x*10, ylim=PlotLimits*2/30, type="l", axes=FALSE, bty = "n", xlab = "", ylab = "", lwd=2, col=precipCOLOR,  xaxs = "i", yaxs = "i")
    #lines(rollsum(PPT_Meanyear$x, 30, fill=NA), ylim=TempLimits*2, type="l", axes=FALSE, bty = "n", xlab = "", ylab = "", lwd=2, col="blue")
    axis(4, col="black", at=pretty(c(0, TempLimits[2]*2/30)), col.axis="black", las=1)
    mtext("Mean Daily Precipitation (mm)" , side=4, line=2.9, col="black", font=2, adj = 0)
    abline(h=0, lwd=0.5, lty=2)
    #abline(h=2, lwd=0.5, lty=2)
    #abline(v=c(FirstFrost_Mean, LastFrost_Mean), lty=2)
    #abline(v=c(quantile(sim_metrics_raw$Frost[[1]]["LastFrost", ], c(0.1, 0.9)), quantile(sim_metrics_raw$Frost[[1]]["FirstFrost", ], c(0.1, 0.9))), lwd=.5, lty=3)
   
    rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = 350, ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
    
    #legend inside the plot border
    text(x = 275, y = max(PlotLimits*2/30*0.95), labels = "Mean Daily Temperature", col="black", font = 2, cex = 1, pos = 2)
    text(x = 275, y = max(PlotLimits*2/30*0.90), labels = "Minimum Daily Temperature", col="black", font = 2, cex = 1, pos = 2)
    text(x = 350, y = max(PlotLimits*2/30*0.95), labels = "Mean Daily Precipitation", col="black", font = 2, cex = 1, pos = 2)
    lines(x = c(277,287), y = c(max(PlotLimits*2/30*0.95), max(PlotLimits*2/30*0.95)), type = "l", col = temperatureCOLOR, lwd = 4)
    lines(x = c(277,287), y = c(max(PlotLimits*2/30*0.90), max(PlotLimits*2/30*0.90)), type = "l", col = mintemperatureCOLOR, lwd = 4)
    lines(x = c(352,362), y = c(max(PlotLimits*2/30*0.95), max(PlotLimits*2/30*0.95)), type = "l", col = precipCOLOR, lwd = 4)
    
    rect(xleft = 0, xright = 70, ybottom = max(PlotLimits*2/30*0.833), ytop = max(PlotLimits*2/30), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = 2, y = max(PlotLimits*2/30*0.91), labels = "Temperature and\nPrecipitation", col="black", font = 2, cex = 1.5, pos = 4)
    
    rect(xleft = 70, xright = 151, ybottom = max(PlotLimits*2/30*0.833), ytop = max(PlotLimits*2/30), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = 71, y = max(PlotLimits*2/30*0.91), labels = "Monthly Precipitation\nCorrelation with Temperature", col="black", cex = 1, pos = 4)
    text(x = 131, y = max(PlotLimits*2/30*0.91), labels = paste("= ",round(mean(as.numeric(sim_metrics$PPTseas[-c(1,2)])), 2)), col="black", cex = 1, pos = 4)
  #~~~~~~~~~ Variables ~~~~~~~~~~
    #scale_fill_brewer(palette = "Set3") 
    par(mar=c(0,1,0,2))
    x <- c(1:5)
    y <- c(1:5)
    plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1, xaxs = "i", yaxs = "i")
       
    #rect(xleft = c(0.93), ybottom = c(1.7), xright = c(2.3), ytop = c(2.05), border = temperatureCOLOR, lwd = 2)
    mtext("Mean Annual\nTemperature", at = min(x*1.5), side=3, adj=0, line=3, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(sim_data$clim$values$tmean), 1), "C"), at = max(x*0.7), side=3, adj=0, line=2.5, col="black", font=2, padj = 1, cex = 0.8)
   
     #rect(xleft = c(0.93), ybottom = c(1.3), xright = c(2.3), ytop = c(1.65), border = precipCOLOR, lwd = 2)
    mtext("Mean Annual\nPrecipitation", at = min(x*1.5), side=3, adj=0, line=-1, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(PPT_eachyear$x)*10, 0), "mm"), at = max(x*0.7), side=3, adj=0, line=-1.5, col="black", font=2, padj = 1, cex = 0.8)
    #--------------------------------
    
    
    
    
    #-------- PLOT 2 ---------
    
    #~~~~~~~ SWP and SWA plot ~~~~~~~
  
    par(mar=c(0, 5, 0, 4), bg = "white")
    addspace <- c(min(SWP_Meanyear[, -1]/-10), 0)[1]*1.2 - c(min(SWP_Meanyear[, -1]/-10), 0)[1]
    PlotLimits <- c(min(SWP_Meanyear[, -1]/-10)[1], (0 - addspace))
    #plot(SWP_Meanyear$swp.Lyr_1/-10~ xValues, type="l",  xlab = "", yaxt='n', xaxt='n', ylab="", lwd=3, col=soilCOLORS[5], ylim=c(min(SWP_Meanyear[, -1]/-10), 0),  xaxs = "i", yaxs = "i")
    plot(SWP_Meanyear$swp.Lyr_1/-10 ~ xValues, type="l",  xlab = "", yaxt='n', xaxt='n', ylab="", lwd=2.5, col=soilCOLORS[5], ylim=PlotLimits,  xaxs = "i", yaxs = "i")
    axis(2, col="black",  col.axis="black", las=1)
    mtext("Soil Water Potential (MPa)", side=2, line=2.5, col="black", font=2, adj = 0)
    lines(SWP_Meanyear$swp.Lyr_2/-10~ xValues,  lwd=2.5, col=soilCOLORS[4])
    lines(SWP_Meanyear$swp.Lyr_3/-10~ xValues, lwd=2.5, col=soilCOLORS[3])
    lines(SWP_Meanyear$swp.Lyr_4/-10~ xValues,  lwd=2.5, col=soilCOLORS[2])
    #abline(h=c(-1.5, 0, -3), lty=2, col="black")
    abline(h=c(0), lty=2, col="black", lwd = 0.5)
    axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n")
    abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
    

    #~~~~~~~~ SWA axis ~~~~~~~~
    
    par(new=TRUE)
    SWALimits <- c(0, max(SWA_Meanyear[,2]*2))
    PlotLimits <- SWALimits*1.2
    #plot(SWA_Meanyear, type="l",  axes=FALSE, bty = "n", xlab = "", ylab = "", col=SWA_COLOR, lwd=2, ylim=c(0, max(SWA_Meanyear[,2]*2)),  xaxs = "i", yaxs = "i")
    plot(SWA_Meanyear, type="l",  axes=FALSE, bty = "n", xlab = "", ylab = "", col=SWA_COLOR, lwd=2, ylim=PlotLimits,  xaxs = "i", yaxs = "i")
    polygon(x=c(SWA_Meanyear$Group.1, rev(SWA_Meanyear$Group.1)), y=c(SWA_Meanyear[,2], rep(0, 366)), col=alpha(SWA_COLOR, 0.3), border = NA)
    #lines(rollsum(PPT_Meanyear$x, 30, fill=NA), ylim=TempLimits*2, type="l", axes=FALSE, bty = "n", xlab = "", ylab = "", lwd=2, col="blue")
    axis(4, col="black",  col.axis="black", las=1, at=pretty(c(0, max(SWA_Meanyear[,2]))))
    mtext("Soil Water Availability (mm)", side=4, line=2.9, col="black", font=2, adj = 0)
    #lines(x = c(SWA_Meanyear$Group.1, rev(SWA_Meanyear$Group.1)), y = c((SWA_Meanyear[,2] + SWA_CV[,2]), rep(0, 366)), lty = 2, col = SWA_COLOR)
    lines(x = SWA_Meanyear$Group.1, y = SWA_10and90per[,1], lty = 3, col = SWA_COLOR, lwd = 1.75)
    lines(x = SWA_Meanyear$Group.1, y = SWA_10and90per[,2], lty = 3, col = SWA_COLOR, lwd = 1.75)

    rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = 365, ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
    
    #legend inside the plot border
    text(x = 350, y = max(PlotLimits*0.95), labels = "Soil Water Availability", col="black", font = 2, cex = 1, pos = 2)
    text(x = 350, y = max(PlotLimits*0.90), labels = "10th and 90th Percentiles", col="black", font = 2, cex = 1, pos = 2)
    text(x = 247, y = max(PlotLimits*0.95), labels = "Soil Water Potential:", col="black", font = 2, cex = 1, pos = 2)
    text(x = 275, y = max(PlotLimits*0.95), labels = "shallow soils", col="black", font = 1, cex = 1, pos = 2)
    text(x = 275, y = max(PlotLimits*0.88), labels = "deep soils", col="black", font = 1, cex = 1, pos = 2)
    lines(x = c(352,362), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = SWA_COLOR, lwd = 4)
    lines(x = c(352,362), y = c(max(PlotLimits*0.90), max(PlotLimits*0.90)), type = "l", col = SWA_COLOR, lwd = 2, lty = 3)
    lines(x = c(277,287), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = soilCOLORS[5], lwd = 4)
    lines(x = c(277,287), y = c(max(PlotLimits*0.88), max(PlotLimits*0.88)), type = "l", col = soilCOLORS[2], lwd = 4)
    #arrow between soil legend elements
    arrows(x0 = 282, x1 = 282, y0 = max(PlotLimits*0.895), y1 = max(PlotLimits*0.935), length = 0.04, code = 3, col = "black", lwd = 1.25)

    rect(xleft = 0, xright = 70, ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = 2, y = max(PlotLimits*0.91), labels = "Soil Water Potential\nand Availability", col="black", font = 2, cex = 1.5, pos = 4)
    
    rect(xleft = 70, xright = 151, ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = 71, y = max(PlotLimits*0.91), labels = "Monthly Soil Water Availability\nCorrelation with Temperature", col="black", cex = 1, pos = 4)
    text(x = 131, y = max(PlotLimits*0.91), labels = paste("= ", round(mean(as.numeric(sim_metrics$SWA[which(sim_metrics$SWA$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 1, pos = 4)
    
    #~~~~~~~~~ Variables ~~~~~~~~~~
    par(mar=c(0, 1, 0, 1))
    x <- c(1:5)
    y <- c(1:5)
    plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1)
    #rect(xleft = c(0), ybottom = c(1), xright = c(3.3), ytop = c(2.3), col = "gray95", lwd = 0)
  
    rect(xleft = min(x), ybottom = max(x*0.91), xright = max(x), ytop = max(x), border = NA, col = "gray90")
    mtext("Dry Soil Intervals", at = min(x*1.25), side=3, adj=0, line=-2, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = min(x), ybottom = max(x*0.72), xright = max(x), ytop = max(x*0.90), border = NA, col = "gray90")
    mtext("Mean Duration", at = min(x*1.25), side=3, adj=0, line=-5, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(as.numeric(DSI_Lengths)), 1), "days"), at = max(x*0.65), side=3, adj=0, line=-5, col="black", font=2, padj = 1, cex = 0.8)
    mtext("Mean Number", at = min(x*1.25), side=3, adj=0, line=-7, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(as.numeric(DSI_Number)), 1)), at = max(x*0.65), side=3, adj=0, line=-7, col="black", font=2, padj = 1, cex = 0.8)
   
    rect(xleft = min(x), ybottom = max(x*0.51), xright = max(x), ytop = max(x*0.60), border = NA, col = alpha(SWA_COLOR, 0.1))
    mtext("Soil Water Availability", at = min(x*1.25), side=3, adj=0, line=-13.75, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = min(x), ybottom = max(x*0.32), xright = max(x), ytop = max(x*0.50), border = NA, col = alpha(SWA_COLOR, 0.1))
    mtext("Mean", side=3, adj=0, line=-17, col="black", font=2, padj = 1, cex = 0.8, at = min(x*1.25))
    mtext(paste(" = ", round(mean(SWA_eachyear[,2]) ,1), "mm"), at = max(x*0.5), side=3, adj=0, line=-17, col="black", font=2, padj = 1, cex = 0.8)
    mtext("Monthly CV ", at = min(x*1.25), side=3, adj=0, line=-19, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste(" = ", round(mean(as.numeric(sim_metrics$SWA[which(sim_metrics$SWA$group=="seasonal_variability"), -c(1,2)])), 2)), at = max(x*0.6), side=3, adj=0, line=-19, col="black", font=2, padj = 1, cex = 0.8)
    

    #--------------------------------
    
    
    #-------- PLOT 3 ---------
   
    #~~~~~~~~ PET, ET and CWD plot ~~~~~~~~
    par(mar=c(0, 5, 0, 4))
    ETLimits <- c(0, max(PET_Meanyear$x)) * 10
    PlotLimits <- ETLimits*1.2
    CWDrollmean_10day <- rollmean(sim_metrics_raw$CWD[[1]]$values$cwd, 10, fill=NA)
    CWDrollmean_10dayMAX <- aggregate(CWDrollmean_10day, by=list(sim_metrics_raw$CWD[[1]]$time[, "Year"]), max, na.rm=TRUE)$x
    CWDrollmean_10dayMAX_DOY <- sim_metrics_raw$CWD[[1]]$time[match(CWDrollmean_10dayMAX, CWDrollmean_10day), "Day"]
    meanDAYs <- seq(round(mean(CWDrollmean_10dayMAX_DOY), 0)-4, round(mean(CWDrollmean_10dayMAX_DOY), 0)+5) 
    #plot(CWD_Meanyear, type="l", xlab="Day of Year", ylab="Daily climatic Water Deficit (mm)", lwd=2)
    #points(x=mean(CWDrollmean_10dayMAX_DOY), y=CWD_Meanyear[mean(CWDrollmean_10dayMAX_DOY), "cwd"], col="red")
    #lines(x=meanDAYs, y=CWD_Meanyear[meanDAYs, "cwd"], col="red", lwd=3)
    #plot(AET_Meanyear$x~ xValues, type="n", xlab="Day of Year", ylab="", col="darkgreen", xaxt='n', ylim=c(0, max(PET_Meanyear$x)), las=1,  xaxs = "i", yaxs = "i")
    plot(AET_Meanyear$x*10 ~ xValues, type="n", xlab="Day of Year", ylab="", col="darkgreen", xaxt='n', ylim=PlotLimits, las=1,  xaxs = "i", yaxs = "i")
    mtext("mm/day", side=2, line=2.5, font=2, adj = 0)
    
    axis(1, at=pretty(xValues, 12), labels=FALSE, xaxt = "n")
    abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
    polygon(x=c(xValues, rev(xValues)), y=c(PET_Meanyear$x*10, rev(AET_Meanyear$x*10)), col=CWD_COLOR, border = NA)
    polygon(x=c(xValues, rev(xValues)), y=c(AET_Meanyear$x*10, rep(0, 366)), col=alpha(AET_COLOR, 0.5), border = NA)
    lines(PET_Meanyear$x*10 ~ xValues, col=PET_COLOR, lwd=3)
    lines(AET_Meanyear$x*10 ~ xValues, col=alpha(SWA_COLOR, 0.8), lwd=3)
    
    #lines(AET_10and90per[,1]*10 ~ xValues, col=alpha("white", 0.1), lwd=10, lty = 1)
    #lines(AET_10and90per[,2]*10 ~ xValues, col=alpha("white", 0.1), lwd=10, lty = 1)
    
    lines(AET_10and90per[,1]*10 ~ xValues, col=alpha(SWA_COLOR, 1), lwd=1.75, lty = 3)
    lines(AET_10and90per[,2]*10 ~ xValues, col=alpha(SWA_COLOR, 1), lwd=1.75, lty = 3)
    
    abline(v=as.Date(c(mean(CWDrollmean_10dayMAX_DOY)-4, mean(CWDrollmean_10dayMAX_DOY)+5), origin = "2019-12-31"), lty=2, col="#9B2226", lwd = 1.25)

    rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = xValues[350], ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
    
    #legend inside the plot border
    text(x = xValues[350], y = max(PlotLimits*0.95), labels = "Actual Evapotranspiration", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[350], y = max(PlotLimits*0.90), labels = "10th and 90th Percentiles", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[275], y = max(PlotLimits*0.95), labels = "Climatic Water Deficit", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[275], y = max(PlotLimits*0.90), labels = "Potential Evapotranspiration", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[196], y = max(PlotLimits*0.92), labels = "Timing of Climatic\nWater Deficit\n10-Day Maximum", col="black", font = 2, cex = 1, pos = 2)
    lines(x = c(xValues[352],xValues[362]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = AET_COLOR, lwd = 4)
    lines(x = c(xValues[352],xValues[362]), y = c(max(PlotLimits*0.90), max(PlotLimits*0.90)), type = "l", col = alpha(SWA_COLOR, 0.8), lwd = 2, lty = 3)
    lines(x = c(xValues[277],xValues[287]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = CWD_COLOR, lwd = 4)
    lines(x = c(xValues[277],xValues[287]), y = c(max(PlotLimits*0.90), max(PlotLimits*0.90)), type = "l", col = PET_COLOR, lwd = 4)
    lines(x = c(xValues[198],xValues[208]), y = c(max(PlotLimits*0.925), max(PlotLimits*0.925)), type = "l", col = "#9B2226", lwd = 1.5, lty = 2)
    
    rect(xleft = 0, xright = xValues[70], ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = xValues[2], y = max(PlotLimits*0.91), labels = "Potential and Actual\nEvapotranspiration", col="black", font = 2, cex = 1.5, pos = 4)
    
    rect(xleft = xValues[70], xright = xValues[151], ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = xValues[71], y = max(PlotLimits*0.91), labels = "Monthly Climatic Water Deficit\nCorrelation with Temperature", col="black", cex = 1, pos = 4)
    text(x = xValues[131], y = max(PlotLimits*0.91), labels = paste("= ", round(mean(as.numeric(sim_metrics$CWD[which(sim_metrics$CWD$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 1, pos = 4)
    
    #~~~~~~~~~ Variables ~~~~~~~~~~
    par(mar=c(0, 1, 0, 1))
    
    x <- c(1:5)
    y <- c(1:5)
    plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1,  xaxs = "i", yaxs = "i")
    #rectangle for important values
    #rect(xleft = c(0), ybottom = c(1), xright = c(3.3), ytop = c(2.1), col = "gray95", lwd = 0)
    
    #rect(xleft = c(1), ybottom = c(1.58), xright = c(2.8), ytop = c(2.06), border = DSI_COLOR, lwd = 1.5)
    #rect(xleft = c(1), ybottom = c(1.6), xright = c(2.05), ytop = c(2), border = PET_COLOR, lwd = 1.5)
    mtext("Yearly Potential\nEvapotranspiration", at = min(x*1.25), side=3, adj=0, line=-2, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(PET_eachyear$x) ,1), "mm"), at = max(x*0.7), side=3, adj=0, line=-2.5, col="black", font=2, padj = 1, cex = 0.8)
    
    #rect(xleft = c(1), ybottom = c(1.15), xright = c(2.05), ytop = c(1.53), border = AET_COLOR, lwd = 1.5)
    mtext("Yearly Actual\nEvapotranspiration", at = min(x*1.25), side=3, adj=0, line=-7, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(AET_eachyear$x) ,1), "mm"), at = max(x*0.7), side=3, adj=0, line=-7.5, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = min(x), ybottom = max(x*0.51), xright = max(x), ytop = max(x*0.60), border = NA, col = alpha(CWD_COLOR, 0.2))
    mtext("Climatic Water Deficit", at = min(x*1.25), side=3, adj=0, line=-13.75, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = min(x), ybottom = max(x*0.25), xright = max(x), ytop = max(x*0.50), border = NA, col = alpha(CWD_COLOR, 0.2))
    mtext(paste("Mean      = ", round(mean(CWD_eachyear$cwd/10) ,1), "mm"), at = min(x*1.25), side=3, adj=0, line=-17, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("Monthly CV     = ", round(mean(as.numeric(sim_metrics$CWD[which(sim_metrics$CWD$group=="seasonal_variability"), -c(1,2)])), 2)), at = min(x*1.25), side=3, adj=0, line=-19, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("10-Day Max    = ", round(mean(CWDrollmean_10dayMAX), 2), "mm"), at = min(x*1.25), side=3, adj=0, line=-21, col="black", font=2, padj = 1, cex = 0.8)
    
    #--------------------------------
    
    
    #------- PLOT 4 --------
    
    #~~~~~~~ Degree day plot ~~~~~~~~
    par(mar=c(0, 5, 0, 4))
    DDLimits <- c(min(TDD_Meanyear$x), max(TDD_Meanyear$x))
    PlotLimits <- DDLimits*1.2
    plot(TDD_Meanyear$x~ xValues, type="n", xlab="", ylab="", xaxt='n',las=1,  xaxs = "i", yaxs = "i", ylim = PlotLimits)
    axis(1, at=pretty(xValues, 12), labels=FALSE)
    #axis(1, at=xValues[which(format(xValues, format = "%d")==15)], labels=month.abb[seq(1,12)], tick=FALSE, font=2)
    abline(v=pretty(xValues, 12), lwd=0.8, lty=3, col="gray")
    mtext("Degree Days", side=2, line=2.5, font=2, adj = 0)
    polygon(x=c(xValues, rev(xValues)), y=c(TDD_Meanyear$x, rep(0, 366)), col=neutralCOLOR, border = NA)
    #polygon(x=c(xValues, rev(xValues)), y=c(WDD_Meanyear$x + DDD_Meanyear$x, rep(0, 366)), col=dryCOLOR, border = NA)
    polygon(x=c(xValues, rev(xValues)), y=c(TDD_Meanyear$x - DDD_Meanyear$x, rev(TDD_Meanyear$x)), col=dryCOLOR, border = NA)
    lines(WDD_10and90per[,1] ~ xValues, col=alpha(wetCOLOR, 1), lwd=1.75, lty = 3)
    lines(WDD_10and90per[,2] ~ xValues, col=alpha(wetCOLOR, 1), lwd=1.75, lty = 3)
    
    polygon(x=c(xValues, rev(xValues)), y=c(WDD_Meanyear$x, rep(0, 366)), col=alpha(wetCOLOR, 0.75), border = NA)
    lines(TDD_Meanyear$x~ xValues, col="black", lwd=2)
    
    TDDmean <- round(mean(as.numeric(sim_metrics$TDD[1, -c(1,2)]), 2))
    DDDmean <- round(mean(as.numeric(sim_metrics$DDD[1, -c(1,2)]), 2))
    WDDmean <- round(mean(as.numeric(sim_metrics$WDD[1, -c(1,2)]), 2))
    NDDmean <- TDDmean - DDDmean - WDDmean
 
    springRECdays <- round(c(mean(SpringRecruitment_DOY, na.rm=TRUE), mean(SpringRecruitment_DOY, na.rm=TRUE) + mean(SpringRecruitment_DurationDays, na.rm=TRUE)), 0)
    springRECdates <- as.Date(springRECdays ,  origin = "2019-12-31")
    fallRECdays <- round(c(mean(FallRecruitment_DOY, na.rm=TRUE), mean(FallRecruitment_DOY, na.rm=TRUE) + mean(FallRecruitment_DurationDays, na.rm=TRUE)), 0)
    fallRECdates <- as.Date(fallRECdays ,  origin = "2019-12-31")

    #abline(v = springRECdates, col = "black", lty = 2, lwd = 1.25)
    #abline(v = fallRECdates, col = "black", lty = 2, lwd = 1.25)
    rect(xleft = springRECdates[1], xright = springRECdates[2], ybottom = 0, ytop = max(PlotLimits), col = alpha("forestgreen", 0.1), border = "black", lty = 2)
    rect(xleft = fallRECdates[1], xright = fallRECdates[2], ybottom = 0, ytop = max(PlotLimits), col = alpha("forestgreen", 0.1), border = "black", lty = 2)
    
    
    #legend inside the plot border
    rect(xleft = 0, ybottom = max(PlotLimits*0.833), xright = xValues[350], ytop = max(PlotLimits), col = alpha("white", 0.75), border = NA, lwd = 1.5, lty = 2)
    
    text(x = xValues[350], y = max(PlotLimits*0.95), labels = "Wet Degree Days", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[350], y = max(PlotLimits*0.90), labels = "10th and 90th Percentiles", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[275], y = max(PlotLimits*0.95), labels = "Dry Degree Days", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[275], y = max(PlotLimits*0.90), labels = "Neutral Degree Days", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[200], y = max(PlotLimits*0.95), labels = "Total Degree Days", col="black", font = 2, cex = 1, pos = 2)
    text(x = xValues[200], y = max(PlotLimits*0.90), labels = "Recruitment Period", col="black", font = 2, cex = 1, pos = 2)
    lines(x = c(xValues[352],xValues[362]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = wetCOLOR, lwd = 4)
    lines(x = c(xValues[352],xValues[362]), y = c(max(PlotLimits*0.90), max(PlotLimits*0.90)), type = "l", col = wetCOLOR, lwd = 2, lty = 3)
    lines(x = c(xValues[277],xValues[287]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = dryCOLOR, lwd = 4)
    lines(x = c(xValues[277],xValues[287]), y = c(max(PlotLimits*0.90), max(PlotLimits*0.90)), type = "l", col = neutralCOLOR, lwd = 4)
    lines(x = c(xValues[202],xValues[212]), y = c(max(PlotLimits*0.95), max(PlotLimits*0.95)), type = "l", col = "black", lwd = 4)
    rect(xleft = xValues[202], ybottom = max(PlotLimits*0.88), xright = xValues[212], ytop = max(PlotLimits*0.92), col = alpha("forestgreen", 0.1), border = "black", lwd = 1, lty = 2)
    
    
    rect(xleft = 0, xright = xValues[70], ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = xValues[2], y = max(PlotLimits*0.91), labels = "Wet and Dry\nDegree Days", col="black", font = 2, cex = 1.5, pos = 4)
    
    rect(xleft = xValues[70], xright = xValues[151], ybottom = max(PlotLimits*0.833), ytop = max(PlotLimits), border = "black", lwd = 1, col = alpha("gray92", 0.6))
    text(x = xValues[71], y = max(PlotLimits*0.91), labels = "Monthly Wet Degree Days\nCorrelation with Temperature", col="black", cex = 1, pos = 4)
    text(x = xValues[131], y = max(PlotLimits*0.91), labels = paste(" = ", round(mean(as.numeric(sim_metrics$WDD[which(sim_metrics$WDD$group=="seasonality"), -c(1,2)])), 2)), col="black", cex = 1, pos = 4)
    
    #~~~~~~~~~ Variables ~~~~~~~~~~
    par(mar=c(0, 1, 0, 1))
    
    x <- c(1:5)
    y <- c(1:5)
    plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1, xaxs = "i", yaxs = "i")
    #rectangle for important values
    #rect(xleft = c(0), ybottom = c(1), xright = c(3.3), ytop = c(2), col = "gray95", lwd = 0)
    
    rect(xleft = min(x), ybottom = max(x*0.91), xright = max(x), ytop = max(x), border = NA, col = "gray90")
    mtext("Total Degree Days: ", at = min(x*1.25), side=3, adj=0, line=-1.5, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = min(x), ybottom = max(x*0.75), xright = max(x), ytop = max(x*0.9), border = NA, col = "gray90")
    mtext(paste("Mean       = ", TDDmean, "degree days"), at = min(x*1.25), side=3, adj=0, line=-4.5, col="black", font=2, padj = 1, cex = 0.8)
    mtext("Monthly CV", at = min(x*1.25), side=3, adj=0, line=-6.5, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("= ", round(mean(as.numeric(sim_metrics$TDD[which(sim_metrics$TDD$group=="seasonal_variability"), -c(1,2)])), 2)), at = max(x*0.6), side=3, adj=0, line=-6.5, col="black", font=2, padj = 1, cex = 0.8)
    
    mtext(paste("Dry Degree Days     =", DDDmean), at = min(x*1.25), side=3, adj=0, line=-9.5, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("Wet Degree Days     =", WDDmean), at = min(x*1.25), side=3, adj=0, line=-11.5, col="black", font=2, padj = 1, cex = 0.8)
    mtext(paste("Neutral Degree Days     =", NDDmean), at = min(x*1.25), side=3, adj=0, line=-13.5, col="black", font=2, padj = 1, cex = 0.8)
    
    
    rect(xleft = min(x), ybottom = min(x), xright = max(x*0.68), ytop = max(x*0.46), border = NA, col = alpha("forestgreen", 0.2))
    mtext("Recruitment\nMaximum\nWet Degree Days", at = min(x*1.25), side=3, adj=0, line=-18.25, col="black", font=2, padj = 1, cex = 0.8)
    mtext("Percent Years\nwith Recruitment", at = min(x*1.25), side=3, adj=0, line=-22.5, col="black", font=2, padj = 1, cex = 0.8)
   
    rect(xleft = max(x*0.69), ybottom = max(x*0.47), xright = max(x*0.84), ytop = max(x*0.53), border = NA, col = alpha("forestgreen", 0.2))
    mtext("Spring", at = max(x*0.71), side=3, adj=0, line=-15.7, col="black", font=2, padj = 1, cex = 0.6)
     
    rect(xleft = max(x*0.69), ybottom = min(x), xright = max(x*0.84), ytop = max(x*0.46), border = NA, col = alpha("forestgreen", 0.2))
    mtext(round(mean(SpringRecruitment_maxWDD, na.rm=TRUE), 0), at = max(x*0.74), side=3, adj=0, line=-19.25, col="black", font=2, padj = 1, cex = 0.8)
    mtext((sum(!is.na(SpringRecruitment_maxWDD))/length(SpringRecruitment_maxWDD)*100), at = max(x*0.74), side=3, adj=0, line=-23, col="black", font=2, padj = 1, cex = 0.8)
    
    rect(xleft = max(x*0.85), ybottom = max(x*0.47), xright = max(x), ytop = max(x*0.53), border = NA, col = alpha("forestgreen", 0.2))
    mtext("Fall", at = max(x*0.89), side=3, adj=0, line=-15.7, col="black", font=2, padj = 1, cex = 0.6)
    
    rect(xleft = max(x*0.85), ybottom = min(x), xright = max(x), ytop = max(x*0.46), border = NA, col = alpha("forestgreen", 0.2))
    mtext(round(mean(FallRecruitment_maxWDD, na.rm=TRUE), 0), at = max(x*0.88), side=3, adj=0, line=-19.25, col="black", font=2, padj = 1, cex = 0.8)
    mtext((sum(!is.na(FallRecruitment_maxWDD))/length(FallRecruitment_maxWDD)*100), at = max(x*0.88), side=3, adj=0, line=-23, col="black", font=2, padj = 1, cex = 0.8)
    
    

    #I think percent() is more complicated than it needs to be; returning other stuff
    #mtext(percent(sum(!is.na(SpringRecruitment_maxWDD))/length(SpringRecruitment_maxWDD)), at = c(2.15), side=3, adj=0, line=-20, col="black", font=2, padj = 1, cex = 0.8)
    #mtext(percent(sum(!is.na(FallRecruitment_maxWDD))/length(FallRecruitment_maxWDD)), at = c(2.15), side=3, adj=0, line=-21, col="black", font=2, padj = 1, cex = 0.8)
    
    #-------------------------------
    
     
    #------- ROW 6: ------
    #empty plot with month x-axis
    par(mar=c(0, 5, 0, 4))
    
    plot(c(1:13), xaxt = "n", yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = "", xaxs = "i", yaxs = "i")
    mtext(at = c(1.25:12.25), line = -2, month.abb[seq(1,12)], font = 2, padj = 0, adj = 0)

    sf::sf_use_s2(FALSE)
    
    plot.new()
    
    #par(mar = c(0,0,10,10), pin = c(2,2))
    par(mar = c(5,1,6,1))
     plot(st_geometry(st_transform(ms_simplify(extent_ecoregions, keep = 0.01, keep_shapes = TRUE), st_crs(ppts_hist))), 
          xlim = c(st_bbox(extent_mask)[1] + 0.5, st_bbox(extent_mask)[3]), 
          ylim = c(st_bbox(extent_mask)[2] + 1, st_bbox(extent_mask)[4]),
          lwd = 0.15, col = alpha(extent_ecoregions$newcolor, 0.25), border = NA)
    box(lwd = 1)
    #par(new = TRUE)
    plot(st_geometry(st_transform(ms_simplify(states_mask, keep = 0.01), st_crs(ppts_hist))), lwd = 0.5, add = T)
    plot(st_geometry(st_transform(ms_simplify(provinces_mask, keep = 0.01), st_crs(ppts_hist))), lwd = 0.5, add = T)
    par(new = TRUE)
    
    plot(st_geometry(st_transform(ms_simplify(extent_ecoregions[extent_ecoregions$newname == ecoregion_label,], keep = 0.01, keep_shape = TRUE), st_crs(ppts_hist))), add = T, lwd = 0.15, col = extent_ecoregions$newcolor[extent_ecoregions$newname == ecoregion_label][[1]], border = NA)
    
    points(repsitenames$Y_WGS84[repsitenames$ecoregion == ecoregion_label] ~ repsitenames$X_WGS84[repsitenames$ecoregion == ecoregion_label], xlim = c(-123.125,-96.0625), ylim = c(29.5, 49), col = "black", pch = 15, cex = 1.5)
    
    quartz.save(file=file.path(dir_prj, paste0("MeanMetrics_", name_sw2_run, ".jpg")), type = "jpg", device = dev.cur(), dpi = 300)
    dev.off()
    
  } #End makeFigure1site
  

makeFigure1site("002544_HotDeserts_Site_2544", repsitenames = repsitenames)


dev.off()












makeFigure1site("02468_CaliforniaAnnuals_Site_2468", repsitenames = repsitenames)



siteNames <- c("02468_CaliforniaAnnuals_Site_2468", 
               "002544_HotDeserts_Site_2544",
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
  



sitename <- "002544_HotDeserts_Site_2544"










?rollmean





















#~~~~~~~~~ Legend ~~~~~~~~~~
par(mar=c(0, 2, 0, 3))

x <- c(1:3)
y <- c(1:3)
plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1)
rect(xleft = c(-0.5), ybottom = c(2.15), xright = c(2.4), ytop = c(3.5), col = "gray95", lwd = 0)

lines(x = c(1.05,1.35), y = c(2.9,2.9), type = "l", col = temperatureCOLOR, lwd = 4)
mtext("Mean Daily\nTemperature", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -1, cex = 0.8)

lines(x = c(1.05,1.35), y = c(2.62,2.62), type = "l", col = mintemperatureCOLOR, lwd = 4)
mtext("Minimum Daily\nTemperature", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -4, cex = 0.8)

lines(x = c(1.05,1.35), y = c(2.33,2.33), type = "l", col = precipCOLOR, lwd = 4)
mtext("Mean Daily\nPrecipitation", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -7, cex = 0.8)



#~~~~~~~~~ Legend ~~~~~~~~~~
#new empty plot
par(mar=c(0, 2, 0, 3))

x <- c(1:3)
y <- c(1:3)
plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1)
#rectangle for legend
rect(xleft = c(0), ybottom = c(2.27), xright = c(2.6), ytop = c(3.5), col = "gray95", lwd = 0) 

#Shallow soils label
lines(x = c(1.05,1.35), y = c(2.96,2.96), type = "l", col=soilCOLORS[4], lwd = 4)
mtext("Shallow Soils", side=3, at = c(1.5), line=-1, col="black", font=2, padj = 1, adj = 0, cex = 0.8)


#Deep soils label
lines(x = c(1.05,1.35), y = c(2.58,2.58), type = "l", col=soilCOLORS[1], lwd = 4)
mtext("Deep Soils", side=3, at = c(1.5), line=-5, col="black", font=2, padj = 1, adj = 0, cex = 0.8)

#SWA label
lines(x = c(1.05,1.35), y = c(2.40,2.40), type = "l", col = precipCOLOR, lwd = 4)
mtext("Soil Water Availability (mm)", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -7, cex = 0.8)


#~~~~~~~~~ Legend ~~~~~~~~~~
#new empty plot
par(mar=c(0, 2, 0, 2))

x <- c(1:3)
y <- c(1:3)
plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1,  xaxs = "i", yaxs = "i")
#rectangle for legend
rect(xleft = c(0), ybottom = c(2.1), xright = c(2.6), ytop = c(3.5), col = "gray95", lwd = 0) 

#PET label
lines(x = c(1.1,1.35), y = c(2.9,2.9), type = "l", col=PET_COLOR, lwd = 4)
mtext("Potential Evapo-Transpiration", side=3, at = c(1.5), line=-1, col="black", font=2, padj = 1, adj = 0, cex = 0.8)

#AET label
lines(x = c(1.1,1.35), y = c(2.72,2.72), type = "l", col = AET_COLOR, lwd = 4)
mtext("Actual Evapo-Transpiration", side=3, at = c(1.5), line=-3, col="black", font=2, padj = 1, adj = 0, cex = 0.8)

#CWD label
lines(x = c(1.1,1.35), y = c(2.56,2.56), type = "l", col = CWD_COLOR, lwd = 4)
mtext("Climatic Water Deficit", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -5, cex = 0.8)

#10-day max label
lines(x = c(1.15,1.15), y = c(2.22,2.35), type = "l", col = "#9B2226", lwd = 1.5, lty = 2)
lines(x = c(1.28,1.28), y = c(2.22,2.35), type = "l", col = "#9B2226", lwd = 1.5, lty = 2)
mtext("Timing of Climatic Water\nDeficit (mm) 10-Day Max", side=3, at = c(1.5), adj=0, col="black", font=2, padj = 1, line = -7.5, cex = 0.8)


#~~~~~~~~~ Legend ~~~~~~~~~~
#new empty plot
par(mar=c(0, 2, 0, 1))

x <- c(1:3)
y <- c(1:3)
plot(y ~ x, xaxt = 'n', bty = "n", yaxt = 'n', pch = '', ylab = '', xlab = '', lwd = 0.1)
#rectangle for legend
rect(xleft = c(0), ybottom = c(2.1), xright = c(2.2), ytop = c(3.5), col = "gray95", lwd = 0) 

#TDD label
lines(x = c(1.05,1.3), y = c(2.96,2.96), type = "l", col="black", lwd = 4)
mtext("Total Degree Days", side=3, at = c(1.4), line=-1, col="black", font=2, padj = 1, adj = 0, cex = 0.8)

#NDD label
lines(x = c(1.05,1.3), y = c(2.78,2.78), type = "l", col = neutralCOLOR, lwd = 4)
mtext("Neutral Degree Days", side=3, at = c(1.4), line=-3, col="black", font=2, padj = 1, adj = 0, cex = 0.8)

#DDD label
lines(x = c(1.05,1.3), y = c(2.60,2.60), type = "l", col = dryCOLOR, lwd = 4)
mtext("Dry Degree Days", side=3, at = c(1.4), adj=0, col="black", font=2, padj = 1, line = -5, cex = 0.8)

#WDD label
lines(x = c(1.05,1.3), y = c(2.42,2.42), type = "l", col = wetCOLOR, lwd = 4)
mtext("Wet Degree Days", side=3, at = c(1.4), adj=0, col="black", font=2, padj = 1, line = -7, cex = 0.8)

#Recruitment period label
rect(xleft = 1.05, ybottom = 2.15, xright = 1.3, ytop = 2.25, border = "forestgreen", lwd = 1.5, lty = 2)
mtext("Recruitment Period", side=3, at = c(1.4), adj=0, col="black", font=2, padj = 1, line = -9, cex = 0.8)







#plot(PPT_Mean_monthly$x ~ Tmean_Mean_monthly$x,  xlab="", type="n", yaxt='n',  xaxt='n', ylab="",  xaxs = "i", yaxs = "i",)
#abline(a = lm(PPT_Mean_monthly$x ~ Tmean_Mean_monthly$x)[[1]][[2]], b = lm(PPT_Mean_monthly$x ~ Tmean_Mean_monthly$x)[[1]][[1]])
box()
#rect(xleft = min(x*2), xright = max(x), ybottom = min(x), ytop = max(x), border = "black", lwd = 1, col = "white")
#rect(xleft = min(x*2), xright = max(x), ybottom = max(x*0.75), ytop = max(x), border = "black", lwd = 1)
mtext("Monthly Correlation\nwith Temperature", side = 1, line = -10, font = 2, cex = 0.8)

rect(xleft = min(x*2), xright = max(x), ybottom = min(x), ytop = min(x*2), border = "black", lwd = 1)
mtext("Precip", side = 1, line = -1, font = 2, cex = 0.8, at = max(x*0.5), adj = 0)

rect(xleft = min(x), xright = max(x), ybottom = min(x*2), ytop = max(x*0.75), border = "black", lwd = 1)
mtext("Temp", side = 2, line = -1, font = 2, cex = 0.8, at = max(x*0.5), adj = 0)

mtext(paste("r =",round(mean(as.numeric(sim_metrics$PPTseas[-c(1,2)])), 2)), side = 1, line = -10, font = 2, at = max(x*0.75))
rect(xleft = min(Tmean_Mean_monthly$x), xright = max(Tmean_Mean_monthly$x), ybottom = -max(PPT_Mean_monthly$x*0.1), ytop = min(PPT_Mean_monthly$x), border = "black", lwd = 1, col = "white")


#text(x = 2, y = 2, labels = "Temp & Precip", font = 2, cex = 1, pos = 4)
#text(x = 5, y = max(PlotLimits*2/30*0.925), labels = paste("Monthly Corr =", round(mean(as.numeric(sim_metrics$PPTseas[-c(1,2)])), 2)), font = 2, cex = 1, pos = 4)

