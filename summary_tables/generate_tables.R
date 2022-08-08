library(sjPlot)

##################### ~~ Generate summary table for ecoregion ~~ #######################

#load metrics saved objects
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/temp.precip")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/overall.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/seasonal.variability")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/moisture.seasonality")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/extreme.drought.conditions")
load("data_preparation/saved_objects&workspaces/metrics_objects/historical_weather/recruitment.conditions")
#load mask shapefiles
load("data_preparation/saved_objects&workspaces/mapping_sf_objects/overlay_boundaries")




agg_polys_table <-
  function(stars_object, extent_ecoregions) {
    extract_poly <- 
      function(stars_object, extent_ecoregions) {
        ee <- st_transform(extent_ecoregions, st_crs(stars_object))
        ee <- st_transform(ee, st_crs(tdd_hist))
        m <- rep(list(NA), length(unique(ee$newname)))
        for (i in 1:length(unique(ee$newname))) {
          m[[i]] <- aggregate(x = stars_object, by = st_union(ee[ee$newname == unique(ee$newname)[i],]), FUN = mean, na.rm = TRUE)
        } 
        return(m)
        
      }
    
    polys <- extract_poly(stars_object, extent_ecoregions)
    new_table <- rbind(as.data.frame(polys[[1]]),
                        as.data.frame(polys[[2]]),
                        as.data.frame(polys[[3]]),
                        as.data.frame(polys[[4]]),
                        as.data.frame(polys[[5]]),
                        as.data.frame(polys[[6]]),
                        as.data.frame(polys[[7]]),
                        as.data.frame(polys[[8]]),
                       as.data.frame(polys[[9]]),
                       as.data.frame(polys[[10]]),
                       as.data.frame(polys[[11]]),
                       as.data.frame(polys[[12]]))
    
    rownames(new_table) <- c("Chihuahuan Desert",
                             "Upper Gila Mountains",
                             "Southern Plains",
                             "Mediterranean California",
                             "Western Cordillera",
                             "Eastern Plains",
                             "Great Basin",
                             "Wyoming Basin",
                             "Columbia Plateau",
                             "Colorado Plateau",
                             "Mojave Desert",
                             "Northern Plains")
    
    return(new_table)

}


#added study region (use this function not the one above)
agg_polys_table <-
  function(stars_object, extent_ecoregions) {
    extract_poly <- 
      function(stars_object, extent_ecoregions) {
        ee <- st_transform(extent_ecoregions, st_crs(stars_object))
        ee <- st_transform(ee, st_crs(tdd_hist))
        m <- rep(list(NA), length(unique(ee$newname)))
        for (i in 1:length(unique(ee$newname))) {
          m[[i]] <- aggregate(x = stars_object, by = st_union(ee[ee$newname == unique(ee$newname)[i],]), FUN = mean, na.rm = TRUE)
        } 
        return(m)
        
      }
    
    
    
    ee <- st_transform(extent_ecoregions, st_crs(tdd_hist))
    polys <- extract_poly(stars_object, ee)
    sr <- aggregate(x = stars_object, by = st_union(ee), FUN = mean, na.rm = TRUE)
    new_table <- rbind(as.data.frame(sr),
                       as.data.frame(polys[[1]]),
                       as.data.frame(polys[[2]]),
                       as.data.frame(polys[[3]]),
                       as.data.frame(polys[[4]]),
                       as.data.frame(polys[[5]]),
                       as.data.frame(polys[[6]]),
                       as.data.frame(polys[[7]]),
                       as.data.frame(polys[[8]]),
                       as.data.frame(polys[[9]]),
                       as.data.frame(polys[[10]]),
                       as.data.frame(polys[[11]]),
                       as.data.frame(polys[[12]]))
    
    rownames(new_table) <- c("A.Study Region",
                             "Chihuahuan Desert",
                             "Upper Gila Mountains",
                             "Southern Plains",
                             "Mediterranean California",
                             "Western Cordillera",
                             "Eastern Plains",
                             "Great Basin",
                             "Wyoming Basin",
                             "Columbia Plateau",
                             "Colorado Plateau",
                             "Mojave Desert",
                             "Northern Plains")
    
    return(new_table)
    
  }


sr <- aggregate(x = tdd_hist, by = st_union(ee), FUN = mean, na.rm = TRUE)

#overall conditions
tdd_polys_table <- agg_polys_table(tdd_hist, extent_ecoregions)
tddld_polys_table <- agg_polys_table(tddld_hist, extent_ecoregions)
wdd_polys_table <- agg_polys_table(wdd_hist, extent_ecoregions)
ddd_polys_table <- agg_polys_table(ddd_hist, extent_ecoregions)
swa_polys_table <- agg_polys_table(swa_hist, extent_ecoregions)
cwd_polys_table <- agg_polys_table(cwd_hist, extent_ecoregions)
first_polys_table <- agg_polys_table(first_hist, extent_ecoregions)
last_polys_table <- agg_polys_table(last_hist, extent_ecoregions)
fff_polys_table <- agg_polys_table(fff_hist, extent_ecoregions)
flf_polys_table <- agg_polys_table(flf_hist, extent_ecoregions)

tdd_polys_table <- cbind(ecoregion = rownames(tdd_polys_table), tdd_polys_table[,c(2, 4)])
tddld_polys_table <- cbind(ecoregion = rownames(tddld_polys_table), tddld_polys_table[,c(2, 4)])
wdd_polys_table <- cbind(ecoregion = rownames(wdd_polys_table), wdd_polys_table[,c(2, 4)])
ddd_polys_table <- cbind(ecoregion = rownames(ddd_polys_table), ddd_polys_table[,c(2, 4)])
swa_polys_table <- cbind(ecoregion = rownames(swa_polys_table), swa_polys_table[,c(2, 4)])
cwd_polys_table <- cbind(ecoregion = rownames(cwd_polys_table), cwd_polys_table[,c(2, 4)])
first_polys_table <- cbind(ecoregion = rownames(first_polys_table), first_polys_table[,c(2:3)])
last_polys_table <- cbind(ecoregion = rownames(last_polys_table), last_polys_table[,c(2:3)])
fff_polys_table <- cbind(ecoregion = rownames(fff_polys_table), fff_polys_table[,c(2:3)])
flf_polys_table <- cbind(ecoregion = rownames(flf_polys_table), flf_polys_table[,c(2:3)])

tdd_polys_table <- tdd_polys_table[order(tdd_polys_table$ecoregion),]
tddld_polys_table <- tddld_polys_table[order(tddld_polys_table$ecoregion),]
wdd_polys_table <- wdd_polys_table[order(wdd_polys_table$ecoregion),]
ddd_polys_table <- ddd_polys_table[order(ddd_polys_table$ecoregion),]
swa_polys_table <- swa_polys_table[order(swa_polys_table$ecoregion),]
cwd_polys_table <- cwd_polys_table[order(cwd_polys_table$ecoregion),]
first_polys_table <- first_polys_table[order(first_polys_table$ecoregion),]
last_polys_table <- last_polys_table[order(last_polys_table$ecoregion),]
fff_polys_table <- fff_polys_table[order(fff_polys_table$ecoregion),]
flf_polys_table <- flf_polys_table[order(flf_polys_table$ecoregion),]


setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")
#write.csv(overall.conditions.table, "overall.conditions.csv")

tab_dfs(list(tdd_polys_table,
             tddld_polys_table,
             wdd_polys_table,
             ddd_polys_table,
             swa_polys_table,
             cwd_polys_table,
             first_polys_table,
             last_polys_table,
             fff_polys_table,
             flf_polys_table), file = "overall_conditions.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Total Growing Degree Days (TDD) (degree days)",
                      "Table ... Variable Summary by Ecoregion: Total Growing Degree Days (TDD) Longest Duration (days)",
                      "Table ... Variable Summary by Ecoregion: Wet Degree Days (WDD) (degree days)",
                      "Table ... Variable Summary by Ecoregion: Dry Degree Days (DDD) (degree days)",
                      "Table ... Variable Summary by Ecoregion: Soil Water Availability (SWA) (mm)",
                      "Table ... Variable Summary by Ecoregion: Climatic Water Deficit (CWD) (mm)",
                      "Table ... Variable Summary by Ecoregion: First Fall Frost (day)",
                      "Table ... Variable Summary by Ecoregion: Last Spring Frost (day)",
                      "Proportion of Years with Last Spring Frost",
                      "Proportion of Years with First Fall Frost"),
        table.font.style = "normal",
        digits = 2)


#seasonal variability

tddseasvar_polys_table <- agg_polys_table(tddseasvar_hist, extent_ecoregions)
swaseasvar_polys_table <- agg_polys_table(swaseasvar_hist, extent_ecoregions)
cwdseasvar_polys_table <- agg_polys_table(cwdseasvar_hist, extent_ecoregions)

tddseasvar_polys_table <- cbind(ecoregion = rownames(tddseasvar_polys_table), tddseasvar_polys_table[,c(2, 4)])
swaseasvar_polys_table <- cbind(ecoregion = rownames(swaseasvar_polys_table), swaseasvar_polys_table[,c(2, 4)])
cwdseasvar_polys_table <- cbind(ecoregion = rownames(cwdseasvar_polys_table), cwdseasvar_polys_table[,c(2, 4)])

tddseasvar_polys_table <- tddseasvar_polys_table[order(tddseasvar_polys_table$ecoregion),]
swaseasvar_polys_table <- swaseasvar_polys_table[order(swaseasvar_polys_table$ecoregion),]
cwdseasvar_polys_table <- cwdseasvar_polys_table[order(cwdseasvar_polys_table$ecoregion),]


#write.csv(seasonal.variability.table, "seasonal.variability.csv")
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")

tab_dfs(list(tddseasvar_polys_table,
             swaseasvar_polys_table,
             cwdseasvar_polys_table), 
        file = "seasonal_variability.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Total Growing Degree Days (TDD) Seasonal Variability",
                      "Table ... Variable Summary by Ecoregion: Soil Water Availability (SWA) Seasonal Variability",
                      "Table ... Variable Summary by Ecoregion: Climatic Water Deficit (CWD) Seasonal Variability"),
        table.font.style = "normal",
        digits = 2)





#seasonality
ppts_polys_table <- agg_polys_table(ppts_hist, extent_ecoregions)
wddseas_polys_table <- agg_polys_table(wddseas_hist, extent_ecoregions)
swaseas_polys_table <- agg_polys_table(swaseas_hist, extent_ecoregions)
cwdseas_polys_table <- agg_polys_table(cwdseas_hist, extent_ecoregions)

ppts_polys_table <- cbind(ecoregion = rownames(ppts_polys_table), ppts_polys_table[,c(2:3)])
wddseas_polys_table <- cbind(ecoregion = rownames(wddseas_polys_table), wddseas_polys_table[,c(2:3)])
swaseas_polys_table <- cbind(ecoregion = rownames(swaseas_polys_table), swaseas_polys_table[,c(2:3)])
cwdseas_polys_table <- cbind(ecoregion = rownames(cwdseas_polys_table), cwdseas_polys_table[,c(2:3)])

ppts_polys_table <- ppts_polys_table[order(ppts_polys_table$ecoregion),]
wddseas_polys_table <- wddseas_polys_table[order(wddseas_polys_table$ecoregion),]
swaseas_polys_table <- swaseas_polys_table[order(swaseas_polys_table$ecoregion),]
cwdseas_polys_table <- cwdseas_polys_table[order(cwdseas_polys_table$ecoregion),]

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")
#write.csv(seasonality.table, "seasonality.csv")


tab_dfs(list(ppts_polys_table,
             wddseas_polys_table,
             swaseas_polys_table,
             cwdseas_polys_table), 
        file = "seasonality.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Precipitation (PPT) Seasonal Timing",
                      "Table ... Variable Summary by Ecoregion: Wet Degree Days (WDD) Seasonal Timing",
                      "Table ... Variable Summary by Ecoregion: Soil Water Availability (SWA) Seasonal Timing",
                      "Table ... Variable Summary by Ecoregion: Climatic Water Deficit (CWD) Seasonal Timing"),
        table.font.style = "normal",
        digits = 2)



#hot dry metrics

cwdmx_polys_table <- agg_polys_table(cwdmx_hist, extent_ecoregions)
dddsp_polys_table <- agg_polys_table(dddsp_hist, extent_ecoregions)
dsi_polys_table <- agg_polys_table(dsi_hist, extent_ecoregions)
dsic_polys_table <- agg_polys_table(dsic_hist, extent_ecoregions)

cwdmx_polys_table <- cbind(ecoregion = rownames(cwdmx_polys_table), cwdmx_polys_table[,c(2, 4)])
dddsp_polys_table <- cbind(ecoregion = rownames(dddsp_polys_table), dddsp_polys_table[,c(2, 4)])
dsi_polys_table <- cbind(ecoregion = rownames(dsi_polys_table), dsi_polys_table[,c(2, 4)])
dsic_polys_table <- cbind(ecoregion = rownames(dsic_polys_table), dsic_polys_table[,c(2, 4)])

cwdmx_polys_table <- cwdmx_polys_table[order(cwdmx_polys_table$ecoregion),]
dddsp_polys_table <- dddsp_polys_table[order(dddsp_polys_table$ecoregion),]
dsi_polys_table <- dsi_polys_table[order(dsi_polys_table$ecoregion),]
dsic_polys_table <- dsic_polys_table[order(dsic_polys_table$ecoregion),]

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")

tab_dfs(list(cwdmx_polys_table,
             dddsp_polys_table,
             dsi_polys_table,
             dsic_polys_table), 
        file = "hot-dry.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Climatic Water Deficit (CWD) 10-Day Maximum (mm)",
                      "Table ... Variable Summary by Ecoregion: Dry Degree Days (DDD) Longest Spell",
                      "Table ... Variable Summary by Ecoregion: Dry Soil Intervals (days)",
                      "Table ... Variable Summary by Ecoregion: Number of Dry Soil Intervals"),
        table.font.style = "normal",
        digits = 2)




#recruitment

sprec_polys_table <- agg_polys_table(sprec_hist, extent_ecoregions)
sprecd_polys_table <- agg_polys_table(sprecd_hist, extent_ecoregions)
frec_polys_table <- agg_polys_table(frec_hist, extent_ecoregions)
frecd_polys_table <- agg_polys_table(frecd_hist, extent_ecoregions)


rec_n <- c(spring = sprec_n[[1]], fall = frec_n[[1]])
rec_n_polys_table <- agg_polys_table(rec_n, extent_ecoregions)

sprec_polys_table <- cbind(ecoregion = rownames(sprec_polys_table), sprec_polys_table[,c(2, 4)])
sprecd_polys_table <- cbind(ecoregion = rownames(sprecd_polys_table), sprecd_polys_table[,c(2:3)])
frec_polys_table <- cbind(ecoregion = rownames(frec_polys_table), frec_polys_table[,c(2, 4)])
frecd_polys_table <- cbind(ecoregion = rownames(frecd_polys_table), frecd_polys_table[,c(2:3)])
rec_n_polys_table <- cbind(ecoregion = rownames(rec_n_polys_table), rec_n_polys_table[,c(2:3)])

sprec_polys_table <- sprec_polys_table[order(sprec_polys_table$ecoregion),]
sprecd_polys_table <- sprecd_polys_table[order(sprecd_polys_table$ecoregion),]
frec_polys_table <- frec_polys_table[order(frec_polys_table$ecoregion),]
frecd_polys_table <- frecd_polys_table[order(frecd_polys_table$ecoregion),]
rec_n_polys_table <- rec_n_polys_table[order(rec_n_polys_table$ecoregion),]

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")

tab_dfs(list(sprec_polys_table,
             sprecd_polys_table,
             frec_polys_table,
             frecd_polys_table,
             rec_n_polys_table), 
        file = "recruitment.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Spring Recruitment (degree days)",
                      "Table ... Variable Summary by Ecoregion: Timing of Spring Recruitment (day of year)",
                      "Table ... Variable Summary by Ecoregion: Fall Recruitment (degree days)",
                      "Table ... Variable Summary by Ecoregion: Timing of Fall Recruitment (day of year)",
                      "Table ... Variable Summary by Ecoregion: Number of Years with Recruitment"),
        table.font.style = "normal",
        digits = 2)









lapply(DF[is.num], round, 8)
pick_sig_digits <- function(res) {
  tmp <- c(round(min(res, na.rm = TRUE), digits = 0),round(max(res, na.rm = TRUE), digits = 0))
  tmp2 <- nchar(as.character(max(abs(tmp)))) 
  
  if (tmp2 <= 1) {
    dig = 2
  } else {
    dig = 0
  }
  return(dig)
}

#climate

tempy_polys_table <- agg_polys_table(tempy_hist, extent_ecoregions)
precy_polys_table <- agg_polys_table(precy_hist, extent_ecoregions)

tempy_polys_table <- cbind(ecoregion = rownames(tempy_polys_table), tempy_polys_table[,c(2, 4)])
precy_polys_table <- cbind(ecoregion = rownames(precy_polys_table), precy_polys_table[,c(2:3)])

tempy_polys_table <- tempy_polys_table[order(tempy_polys_table$ecoregion),]
precy_polys_table <- precy_polys_table[order(precy_polys_table$ecoregion),]

tempy_polys_table <- round(tempy_polys_table$mean, digits = pick_sig_digits(tempy_polys_table$mean))
precy_polys_table <- round(precy_polys_table$mean, digits = pick_sig_digits(precy_polys_table$mean))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/tables/summarize_ecoregions")

tab_dfs(list(tempy_polys_table,
             precy_polys_table), 
        file = "climate.doc",
        titles = list("Table ... Variable Summary by Ecoregion: Mean Annual Temperature",
                      "Table ... Variable Summary by Ecoregion: Mean Annual Precipitation"),
        table.font.style = "normal",
        digits = 2)



lapply(tempy_polys_table, round, 8)

mapply(tempy_polys_table, round, 8)
tempy_polys_table %>% mutate(across(where(is.numeric), ~ round(., 2)))


round(tempy_polys_table$mean, digits = pick_sig_digits(tempy_polys_table$mean))
version$version.string

?round


citation()

(tdd_polys_table,
  tddld_polys_table,
  wdd_polys_table,
  ddd_polys_table,
  swa_polys_table,
  cwd_polys_table,
  first_polys_table,
  last_polys_table,
  tddseasvar_polys_table,
  swaseasvar_polys_table,
  cwdseasvar_polys_table,
  ppts_polys_table,
  wddseas_polys_table,
  swaseas_polys_table,
  cwdseas_polys_table,
  cwdmx_polys_table,
  dddsp_polys_table,
  dsi_polys_table,
  dsic_polys_table,
  sprec_polys_table,
  sprecd_polys_table,
  frec_polys_table,
  frecd_polys_table,
  )
  
  rec_n_polys_table)

