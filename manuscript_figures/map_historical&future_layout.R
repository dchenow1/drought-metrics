#~~~~~~~~~~~~~~~~~~~~~~~
source("/droughtmetrics_source/map_netcdf_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~
#load metrics saved objects
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/temp.precip")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/overall.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/seasonal.variability")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/moisture.seasonality")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/extreme.drought.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/recruitment.conditions")
#load mask shapefiles
load("data_preparation/saved_objects&workspaces/mapping_sf_objects/overlay_boundaries")


#~~~~~~~~~~~~~~~~~~~~~~~
#function to generate layout one metric at a time
generate_layout <- 
   function(hist, fut, filename, metric_name = NULL) { 
   #run reformatting plotting and pdf as an entire function
   
   
   #take agree out of the future objects 
   metric <- list(hist, fut[[1]], fut[[2]])
   #metric[[2]] <- metric[[2]][c(1,2,4)]
   #metric[[3]] <- metric[[3]][c(1,2,4)]
   agree.m <- fut[[1]][3]
   agree.e <- fut[[2]][3]
   
   
   map <- function(stars_object, stars_agree = NULL, metric_name = NULL, bound = NULL) {
      x1 <- rep(list(NA), length(stars_object[[1]]))
      x2 <- rep(list(NA), length(stars_object[[2]]))
      x3 <- rep(list(NA), length(stars_object[[3]]))
      p <- list(x1, x2, x3)
      for (i in 1:length(stars_object)) {
         for (j in 1:length(stars_object[[i]])) {
            if (names(stars_object[[i]][j]) == "mean") {
               lim = c(metric[[1]][[1]], metric[[2]][[1]], metric[[3]][[1]])
               
            } else if (names(stars_object[[i]][j]) == "change") {
               lim = c(metric[[2]][[2]], metric[[3]][[2]])
               
            } else if (names(stars_object[[i]][j]) == "zscore") {
               lim = c(metric[[2]][[4]], metric[[3]][[4]])
            } else {
               lim = stars_object[[i]][[j]]
            }
            
            p[[i]][[j]] <- plot_variables(stars_object[[i]][j], stars_agree, metric_name, lim)
            
            if (i == 2) {
               stars_agree = agree.m
            } else if (i == 3) {
               stars_agree = agree.e
            }
            
            # 
            # if (names(stars_object[[i]][j]) == "mean" | 
            #     names(stars_object[[i]][j]) == "change" |
            #     names(stars_object[[i]][j]) == "zscore") {
            #    
            #    quant = lim
            #    
            #  } else {
            #    
            #    quant = stars_object[[i]][j]
            # }
         }
      } 
      
      return(p)
   }
   
   x <- map(metric, metric_name = metric_name)
   
   # 
   # l <- rep(list(NA), 9)
   # for (i in 1:length(x)) {
   #   for (j in 1:length(x[[i]])) {
   #     for (k in 1:length(l)) {
   #       l[[k]] <- cowplot::as_grob(x[[i]][[j]])
   #       
   #     }
   #   }
   # }
   #do.call("grid.arrange", c(grobs = list(l[[1]][[1]]), rnow = 2))
   
   #cant get the loop to work--trying to make a list of 9 objects where each one is a grob--
   #then be able to plot the list of grobs with grid.arrange
   
   #instead assigning objects in the list to their own objects and using grid.arrange with the list of grobs
   #geom_text vs ggtitle
   
   z <- ggplot() + theme_void() + annotate("text", x=0, y=0, label=metric_name, color="black", size = 6)
   z1 <- ggplot() + theme_void() + annotate("text", x=-1, y=-1, label="Historical\nWeather", color="black", size = 8, angle = 90)
   z2 <- ggplot() + theme_void() + annotate("text", x=-1, y=-1, label="RCP 4.5\nMedian GCM", color="black", size = 8, angle = 90)
   z0 <- ggplot() + theme_void()
   
   b1 <- ggplot() + theme_void() + annotate("text", x=-2, y=2, label="Mid-Century\n(2021-2060)", color="black", size = 8)
   b2 <- ggplot() + theme_void() + annotate("text", x=-2, y=2, label="End-Century\n(2061-2100)", color="black", size = 8)
   
   ag1 <- ggplot() + theme_void() + annotate("text", x=-1.5, y=0, label="GCM Agreement\n(values < 7 == 7 in color scale)", color="black", size = 6)
   ag2 <- ggplot() + theme_void() + annotate("text", x=-1.5, y=0, label="GCM Agreement\n(values < 7 == 7 in color scale)", color="black", size = 6)
   
   
   a <- x[[1]][[1]]
   b <- x[[1]][[2]]
   c <- x[[1]][[3]]
   d <- x[[1]][[4]]
   e <- x[[1]][[5]]
   f <- x[[1]][[6]]
   g <- x[[2]][[1]]
   h <- x[[2]][[2]]
   i <- x[[2]][[3]]
   j <- x[[2]][[4]]
   k <- x[[3]][[1]]
   l <- x[[3]][[2]]
   m <- x[[3]][[3]]
   n <- x[[3]][[4]]
   
   
   pdf(filename, width = 37, height = 24)
   grid.arrange(grobs = list(z0,z,z0,z0,z0,z0,z0,
                             z1,a,b,c,d,e,f,
                             z2,g,h,j,k,l,n,
                             z0,b1,i,z0,b2,m,z0,
                             z0,z0,ag1,z0,z0,ag2,z0), 
                nrow = 5, 
                heights = c(2,7,7,7,1), 
                widths = c(1,6,6,6,6,6,6),
                top = textGrob(metric_name, 
                               just = "left", 
                               gpar(fontsize = 8)), 
                #bottom = textGrob("RCP4.5", just = "left")
                )
   dev.off()

}


generate_layout_n_years <- 
   function(metric1, metric2, filename, metric_name1 = NULL, metric_name2 = NULL) { 

      map <- function(stars_object, stars_agree = NULL, metric_name = NULL, bound = NULL) {
         p <- list(NA)
         lim = c(1:40)
         for (i in 1:length(stars_object)){
               p[[i]] <- plot_variables(stars_object[[i]], stars_agree, metric_name, lim = c(1:40))
      }
         return(p)
      }
      
      x <- map(metric1, metric_name1)
      y <- map(metric2, metric_name2)
      
      z1 <- ggplot() + theme_void() + annotate("text", x=-1, y=-1, label="Historical\nWeather", color="black", size = 8, angle = 90)
      z2 <- ggplot() + theme_void() + annotate("text", x=-1, y=-1, label="RCP 4.5\nMedian GCM", color="black", size = 8, angle = 90)
      z0 <- ggplot() + theme_void()
      z3 <- ggplot() + theme_void() + annotate("text", x=0, y=0, label="Number of Years\nwith Spring Recruitment", color="black", size = 10)
      z4 <- ggplot() + theme_void() + annotate("text", x=0, y=0, label="Number of Years\nwith Fall Recruitment", color="black", size = 10)
      
      
      b1 <- ggplot() + theme_void() + annotate("text", x=-2, y=2, label="Mid-Century\n(2021-2060)", color="black", size = 8)
      b2 <- ggplot() + theme_void() + annotate("text", x=-2, y=2, label="End-Century\n(2061-2100)", color="black", size = 8)
   
      
      a <- x[[1]]
      b <- x[[2]]
      c <- x[[3]]
      d <- y[[1]]
      e <- y[[2]]
      f <- y[[3]]
      
      pdf(filename, width = 30, height = 18)
      grid.arrange(grobs = list(z0,z3,z0,z4,z0,
                                z1,a,z0,d,z0,
                                z2,b,c,e,f,
                                z0,b1,b2,b1,b2), 
                   nrow = 4, 
                   heights = c(1,6,6,1), 
                   widths = c(1,4,4,4,4),
                   top = textGrob("metric_name", 
                                  just = "left", 
                                  gpar(fontsize = 8)), 
                   #bottom = textGrob("RCP4.5", just = "left")
      )
      dev.off()
      
   }

setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/recruitment")
generate_layout_n_years(sprec_n, frec_n, filename = "n_years.pdf", 
                        "Proportion of Years with Spring Recruitment", "Proportion of Years with Fall Recruitment")

#generate layouts 

#overall conditions
setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/overall_conditions")

generate_layout(tdd_hist, tdd_fut, filename = "tdd.pdf", 
                metric_name = 'Total Growing\nDegree Days\n(degree days)')

generate_layout(tddld_hist, tddld_fut, filename = "tddld.pdf", 
                metric_name = 'Total Growing Degree Days\nLongest Duration\n(days)')

generate_layout(wdd_hist, wdd_fut, filename = "wdd.pdf", 
                metric_name = 'Wet Degree Days\n(degree days)')

generate_layout(ddd_hist, ddd_fut, filename = "ddd.pdf", 
                metric_name = 'Dry Degree Days\n(degree days)')

generate_layout(swa_hist, swa_fut, filename = "swa.pdf", 
                metric_name = 'Soil Water Availability\n(mm)')

generate_layout(cwd_hist, cwd_fut, filename = "cwd.pdf", 
                metric_name = 'Climatic Water Deficit\n(mm)')

generate_layout(last_hist, last_fut, filename = "last.pdf", 
                metric_name = 'Last Spring Frost\n(day of year)')

generate_layout(first_hist, first_fut, filename = "first.pdf", 
                metric_name = 'First Fall Frost\n(day of year)')



#seasonal variability
setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/seasonal_variability")

generate_layout(tddseasvar_hist, tddseasvar_fut, filename = "tddseasvar.pdf", 
                metric_name = 'Total Growing Degree Days\nSeasonal Variability\n(monthly CV)')

generate_layout(swaseasvar_hist, swaseasvar_fut, filename = "swaseasvar.pdf", 
                metric_name = 'Soil Water Availability\nSeasonal Variability\n(monthly CV)')

generate_layout(cwdseasvar_hist, cwdseasvar_fut, filename = "cwdseasvar.pdf", 
                metric_name = 'Climatic Water Deficit\nSeasonal Variability\n(monthly CV)')






#seasonality
setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/seasonality")

generate_layout(wddseas_hist, wddseas_fut, filename = "wddseas.pdf", 
                metric_name = 'Wet Degree Days\nSeasonality\n(correlation of monthly WDD and monthly temp)')

generate_layout(swaseas_hist, swaseas_fut, filename = "swaseas.pdf", 
                metric_name = 'Soil Water Availability\nSeasonality\n(correlation of monthly SWA and monthly temp)')

generate_layout(cwdseas_hist, cwdseas_fut, filename = "cwdseas.pdf", 
                metric_name = 'Climatic Water Deficit\nSeasonality\n(correlation of monthly CWD and monthly temp)')

generate_layout(ppts_hist, ppts_fut, filename = "ppts.pdf", 
                metric_name = 'Precipitation Seasonality\n(correlation of monthly PPT and monthly temp)')


#extreme hot-dry conditions
setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/extreme_hot-dry_conditions")

generate_layout(cwdmx_hist, cwdmx_fut, filename = "cwdmx.pdf", 
                metric_name = 'Climatic Water Deficit\n10-Day Extreme\n(mm)')

generate_layout(dddsp_hist, dddsp_fut, filename = "dddsp.pdf", 
                metric_name = 'Dry Degree Days\nLongest Duration\n(degree days)')

generate_layout(dsi_hist, dsi_fut, filename = "dsi.pdf", 
                metric_name = 'Dry Soil Intervals\n(day)')

generate_layout(dsic_hist, dsic_fut, filename = "dsic.pdf", 
                metric_name = 'Number of Dry Soil Intervals')



#recruitment
setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/recruitment")

generate_layout(sprec_hist, sprec_fut, filename = "sprec.pdf", 
                metric_name = 'Spring Recruitment Index')

generate_layout(frec_hist, frec_fut, filename = "frec.pdf", 
                metric_name = 'Fall Recruitment Index')

generate_layout(sprecd_hist, sprecd_fut, filename = "sprecd.pdf", 
                metric_name = "Spring Recruitment Onset\n(day of year)")

generate_layout(frecd_hist, frecd_fut, filename = "frecd.pdf", 
                metric_name = "Fall Recruitment Onset\n(day of year)")


#temp&precip

setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/temp&precip")

generate_layout(tempy_hist, tempy_fut, filename = "tempy.pdf", 
                metric_name = 'Yearly Air Temperature')

generate_layout(precy_hist, precy_fut, filename = "precy.pdf", 
                metric_name = 'Yearly Precipitation (mm)')


#quarterly
#temp&precip

setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/temp_q")

generate_layout(tempy_q[[1]], tempy_fut_q[[1]], filename = "tempy_q1.pdf", 
                metric_name = 'Quarter 1 Air Temperature')

generate_layout(tempy_q[[2]], tempy_fut_q[[2]], filename = "tempy_q2.pdf", 
                metric_name = 'Quarter 2 Air Temperature')

generate_layout(tempy_q[[3]], tempy_fut_q[[3]], filename = "tempy_q3.pdf", 
                metric_name = 'Quarter 3 Air Temperature')

generate_layout(tempy_q[[4]], tempy_fut_q[[4]], filename = "tempy_q4.pdf", 
                metric_name = 'Quarter 4 Air Temperature')



setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/precip_q")

generate_layout(precy_q[[1]], precy_fut_q[[1]], filename = "precy_q1.pdf", 
                metric_name = 'Quarter 1 Precipitation (mm)')

generate_layout(precy_q[[2]], precy_fut_q[[2]], filename = "precy_q2.pdf", 
                metric_name = 'Quarter 2 Precipitation (mm)')

generate_layout(precy_q[[3]], precy_fut_q[[3]], filename = "precy_q3.pdf", 
                metric_name = 'Quarter 3 Precipitation (mm)')

generate_layout(precy_q[[4]], precy_fut_q[[4]], filename = "precy_q4.pdf", 
                metric_name = 'Quarter 4 Precipitation (mm)')





setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/wdd_q")

generate_layout(wdd_q[[1]], wdd_fut_q[[1]], filename = "wdd_q1.pdf", 
                metric_name = 'Quarter 1\nWet Degree Days\n(WDD)')

generate_layout(wdd_q[[2]], wdd_fut_q[[2]], filename = "wdd_q2.pdf", 
                metric_name = 'Quarter 2\nWet Degree Days\n(WDD)')

generate_layout(wdd_q[[3]], wdd_fut_q[[3]], filename = "wdd_q3.pdf", 
                metric_name = 'Quarter 3\nWet Degree Days\n(WDD)')

generate_layout(wdd_q[[4]], wdd_fut_q[[4]], filename = "wdd_q4.pdf", 
                metric_name = 'Quarter 4\nWet Degree Days\n(WDD)')



setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/ddd_q")

generate_layout(ddd_q[[1]], ddd_fut_q[[1]], filename = "ddd_q1.pdf", 
                metric_name = 'Quarter 1\nDry Degree Days\n(DDD)')

generate_layout(ddd_q[[2]], ddd_fut_q[[2]], filename = "ddd_q2.pdf", 
                metric_name = 'Quarter 2\nDry Degree Days\n(DDD)')

generate_layout(ddd_q[[3]], ddd_fut_q[[3]], filename = "ddd_q3.pdf", 
                metric_name = 'Quarter 3\nDry Degree Days\n(DDD)')

generate_layout(ddd_q[[4]], ddd_fut_q[[4]], filename = "ddd_q4.pdf", 
                metric_name = 'Quarter 4\nDry Degree Days\n(DDD)')




setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/tdd_q")

generate_layout(tdd_q[[1]], tdd_fut_q[[1]], filename = "tdd_q1.pdf", 
                metric_name = 'Quarter 1\nTotal Growing Degree Days\n(TDD)')

generate_layout(tdd_q[[2]], tdd_fut_q[[2]], filename = "tdd_q2.pdf", 
                metric_name = 'Quarter 2\nTotal Growing Degree Days\n(TDD)')

generate_layout(tdd_q[[3]], tdd_fut_q[[3]], filename = "tdd_q3.pdf", 
                metric_name = 'Quarter 3\nTotal Growing Degree Days\n(TDD)')

generate_layout(tdd_q[[4]], tdd_fut_q[[4]], filename = "tdd_q4.pdf", 
                metric_name = 'Quarter 4\nTotal Growing Degree Days\n(TDD)')






setwd("/Users/dchenoweth/Documents/USGS/R&R Project/R&RMetrics/Figures/map_layouts/quarterly/swa_q")

generate_layout(swa_q[[1]], swa_fut_q[[1]], filename = "swa_q1.pdf", 
                metric_name = 'Quarter 1 Soil Water Availability\n(mm)')

generate_layout(swa_q[[2]], swa_fut_q[[2]], filename = "swa_q2.pdf", 
                metric_name = 'Quarter 2 Soil Water Availability\n(mm)')

generate_layout(swa_q[[3]], swa_fut_q[[3]], filename = "swa_q3.pdf", 
                metric_name = 'Quarter 3 Soil Water Availability\n(mm)')

generate_layout(swa_q[[4]], swa_fut_q[[4]], filename = "swa_q4.pdf", 
                metric_name = 'Quarter 4 Soil Water Availability\n(mm)')

