library(sjPlot)

##################### ~~ Generate summary table for ecoregions ~~ #######################

#load metrics saved objects
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/temp.precip")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/overall.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/seasonal.variability")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/moisture.seasonality")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/extreme.drought.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/recruitment.conditions")


#Get climate summary tables for mean and variability measures: 
#This uses three variables: yearly air temp, yearly precip, and precip seasonality
tempy_polys_table <- agg_polys_table(tempy_hist, extent_ecoregions)
precy_polys_table <- agg_polys_table(precy_hist, extent_ecoregions)
ppts_polys_table <- agg_polys_table(ppts_hist, extent_ecoregions)

tempy_polys_table <- cbind(ecoregion = rownames(tempy_polys_table), tempy_polys_table[,c(2, 4)])
precy_polys_table <- cbind(ecoregion = rownames(precy_polys_table), precy_polys_table[,c(2, 4)])
ppts_polys_table <- cbind(ecoregion = rownames(ppts_polys_table), ppts_polys_table[,c(2, 3)])

tempy_polys_table <- tempy_polys_table[order(tempy_polys_table$ecoregion),]
precy_polys_table <- precy_polys_table[order(precy_polys_table$ecoregion),]
ppts_polys_table <- ppts_polys_table[order(ppts_polys_table$ecoregion),]

#Combine summaries of the three variables into a single data frame
climate <- cbind(tempy = tempy_polys_table,
                 precy = precy_polys_table,
                 ppts = ppts_polys_table)

metrics_ecoregion_summ <- climate[c(2,5,8)]
#Save the ecoregion climate summary table?
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables")

tab_dfs(list(climate), 
        file = "climate.doc",table.font.style = "normal",
        digits = 2)



#Function that retrieves the closest value to another specified value
closest <- function(xv,sv) {
  xv[which(abs(xv-sv)==min(abs(xv-sv)))] 
}


##################### ~~ Get a data frame of sites that are representative of ecoregions ~~ #####################

newlist <- rep(NA, length(climate[,1]))
newdf <- data.frame(ecoregion = climate[,1])

total <- c(tempy = tempy_hist[1], 
           precy = precy_hist[1], 
           ppts= ppts_hist[1])

for (i in 1:length(climate[,1])) {
    
  ee <- st_transform(extent_ecoregions, st_crs(total))
  tmp <- st_crop(total, ee[ee$newname == row.names(climate[i,]),])
  tmp <- na.omit(as.data.frame(tmp))
  
  #Define percent standard deviation for max distance from the mean (percentsd = 1 is 1 standard deviation
  percentsd = 0.25 #this might not work all the time... need a function that tries different values until it works 
                   #e.g. maybe there are no sites in an ecoregion that have all three variables within 25% SD
  
  #Get sites that are within 1*percentsd standard deviations of the mean for the ecoregion (row i)
  #tempy and precy use CV instead of SD and therefore CV column is multiplied times the mean columnn to get sd
  tmp <- tmp[tmp$precy.mean > (climate[i,5] - climate[i,6]*(climate[i,5] * percentsd)) &
               tmp$precy.mean < (climate[i,5] + climate[i,6]*(climate[i,5] * percentsd)) &
               
               tmp$ppts.mean > (climate[i,8] - (climate[i,9] * percentsd)) &
               tmp$ppts.mean < (climate[i,8] + (climate[i,9] * percentsd)) &
               
               tmp$tempy.mean > (climate[i,2] - climate[i,3]*(climate[i,2] * percentsd))
             & tmp$tempy.mean < (climate[i,2] + climate[i,3]*(climate[i,2] * percentsd)),]
 

  #Get the site whose air temperature is closest to the mean for the ecoregion
  new <- tmp[tmp[,3] == closest(xv = tmp[,3], sv = climate[i,2]),]

  #Add that ecoregion row to the data frame
  newdf[i,c(2:6)] <- new
  
} 


#Compare representative sites with ecoregion means?
# plot(newdf[,6] ~ climate[,8])
# m <- lm(newdf[,6] ~ climate[,8])
# summary(m)
# 
# plot(newdf[,5] ~ climate[,5])
# m <- lm(newdf[,5] ~ climate[,5])
# summary(m)
# 
# plot(newdf[,4] ~ climate[,2])
# m <- lm(newdf[,4] ~ climate[,2])
# summary(m)


#Rename 
repsites <- newdf


setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables")
allsites <- read.csv("representative_sites_from_climate/AllSites_AFRIInputMaster.csv")



newdf <- data.frame(ecoregion = climate[,1])

for (i in 1:length(repsites$lat)) {
  newdf[i,c(2:4)] <- allsites[allsites$X_WGS84 == repsites$lon[i] & allsites$Y_WGS84 == repsites$lat[i], ]
  
}

repsitenames <- newdf
repsitenames$ecoregion[repsitenames$ecoregion == "California"] <- "Mediterranean California"

#Save csv of representative sites
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/representative_sites_from_climate")
write.csv(repsitenames, "ecoregions_repsites.csv")
repsitenames <- read.csv("ecoregions_repsites.csv")
