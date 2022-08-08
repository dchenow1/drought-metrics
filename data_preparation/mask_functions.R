#~~~~~~~~~~~~~~~~~~~~~~~
#Load required packages:
library(stars) # package for netcdf manipulation
library(sf)
library(plyr)
library(RColorBrewer)

#~~~~~~~~~~~~~~~~~~~~~~~
#Set working directory (where droughtmetrics_source folder is saved):
setwd("/Users/dchenoweth/Dropbox/drought_metrics")

#Read in shapefiles
states <- st_read("droughtmetrics_source/mask_shapefiles/s_11au16/s_11au16.shp")
provinces <- st_read("droughtmetrics_source/mask_shapefiles/province/province.shp")
ecoregions3 <- st_read("droughtmetrics_source/mask_shapefiles/us_eco_l3/us_eco_l3.shp") #level 3 ecoregions
ecoregions2 <- st_read("droughtmetrics_source/mask_shapefiles/na_cec_eco_l2/NA_CEC_Eco_Level2.shp") #level 2 ecoregions

#Source "map_netcdf_functions.R" here to get some functions
#find_stars_max()
#find_stars_max()
#simple_mask_outline()
source("droughtmetrics_source/map_netcdf_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~
#This function creates a polygon mask from stars object
get_mask <- function(stars_object, bound = NULL) {
  sf::sf_use_s2(FALSE)
  
  stars_object <- st_apply(stars_object, MARGIN = c("lon", "lat"), FUN = mean)
  
  mask <- simple_mask_outline(stars_object, alpha = find_stars_min(stars_object), keep = 1)
  
  #mask <- sf::st_as_sfc(sf::st_bbox(tdd_hist_clim, keep = 1))
  #mask <- sf::st_as_sf(mask)
  
  #USstates <- st_transform(USstates, st_crs(mask))
  
  mask <- st_transform(mask, "+proj=eqc")
  bound <- st_transform(bound, "+proj=eqc")
  
  x <- st_intersects(bound, mask)
  
  x <- bound[lengths(x) > 0,]
  
  x <- st_transform(x, st_crs(stars_object))
  
  return(x)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~
#Load raster...
load("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/objects&workspaces/overall_conditions")
#Get polygon mask for US states based on summarized TDD historical raster:
states_mask <- get_mask(tdd_hist, bound = states)

#Get polygon mask for CA provinces based on TDD historical raster:
provinces_mask <- get_mask(tdd_hist, bound = provinces)

#Manually remove states that are bounded by the raster but not within the study region
# states_mask <- states_mask[states_mask$STATE != "MN" & 
#                              states_mask$STATE != "IA" & 
#                              states_mask$STATE != "MO" & 
#                              states_mask$STATE != "AR" & 
#                              states_mask$STATE != "LA" &
#                              states_mask$STATE != "TX",]

#Get a polygon of the study region extent based on summarized TDD historical raster
extent_mask <- simple_mask_outline(tdd_hist, alpha = find_stars_min(tdd_hist), keep = 1)

#Customizing ecoregions for HDM manuscript:
#Subsets ecoregion polygons that need to be aggregated
greatbasin <- rbind(ecoregions3[ecoregions3$US_L3NAME == "Central Basin and Range" | 
                                  ecoregions3$US_L3NAME == "Northern Basin and Range" | 
                                  ecoregions3$US_L3NAME == "Snake River Plain",])

colorado_plateau <- rbind(ecoregions3[ecoregions3$US_L3NAME == "Colorado Plateaus" | 
                                     ecoregions3$US_L3NAME == "Arizona/New Mexico Plateau", ])

mojave_desert <- rbind(ecoregions3[ecoregions3$NA_L3NAME == "Mojave Basin and Range" |
                                            ecoregions3$US_L3NAME == "Sonoran Basin and Range", ])

chihuahuan_desert <- rbind(ecoregions3[ecoregions3$NA_L3NAME == "Madrean Archipelago" |
                                         ecoregions3$US_L3NAME == "Chihuahuan Deserts", ])

#Rename level 2 and 3 ecoregions that are not new aggregated versions
uppergila <- ecoregions3[ecoregions3$NA_L3NAME == "Arizona/New Mexico Mountains",]
california <- ecoregions3[ecoregions3$NA_L2NAME == "MEDITERRANEAN CALIFORNIA",]
western_cordillera <- ecoregions3[ecoregions3$NA_L2NAME == "WESTERN CORDILLERA",]
southern_plains <- ecoregions3[ecoregions3$NA_L2NAME == "SOUTH CENTRAL SEMI-ARID PRAIRIES",]
columbia_plateau <- ecoregions3[ecoregions3$US_L3NAME == "Columbia Plateau", ]
eastern_plains <- ecoregions3[ecoregions3$NA_L2NAME == "TEMPERATE PRAIRIES",]
northern_plains <- ecoregions3[ecoregions3$NA_L2NAME == "WEST-CENTRAL SEMI-ARID PRAIRIES", ]
wyoming_basin <- ecoregions3[ecoregions3$US_L3NAME == "Wyoming Basin",]

#Assign names to new customized ecoregions
greatbasin$newname <- "Great Basin"
colorado_plateau$newname <- "Colorado Plateau"
mojave_desert$newname = "Mojave Desert"
chihuahuan_desert$newname = "Chihuahuan Desert"
uppergila$newname <- "Upper Gila Mountains"
california$newname = "Mediterranean California"
western_cordillera$newname = "Western Cordillera"
columbia_plateau$newname = "Columbia Plateau"
wyoming_basin$newname = "Wyoming Basin"
southern_plains$newname = "Southern Plains"
northern_plains$newname = "Northern Plains"
eastern_plains$newname = "Eastern Plains"

#Get all new ecoregion objects into a stars object 
extent_ecoregions <- st_as_sf(rbind.fill(greatbasin, colorado_plateau, mojave_desert, chihuahuan_desert, 
                                         uppergila, california, western_cordillera, columbia_plateau, wyoming_basin,
                                         southern_plains, northern_plains, eastern_plains))
extent_ecoregions <- st_transform(extent_ecoregions, st_crs(extent_mask))
extent_ecoregions <- st_intersection(extent_ecoregions, extent_mask)






#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot new masks for testing
#Figure that maps ecoregions within the study region
x <- ggplot() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  geom_sf(data = extent_ecoregions, col = NA, size = 0, aes(fill = newname), alpha = 0.8) +
  geom_sf(data = provinces_mask, col = "black", size = 0.2, fill = NA) +
  geom_sf(data = states_mask, col = "black", size = 0.2, fill = NA) +
  geom_sf(data = extent_mask, col = "black", size = 0.05, fill = NA) +
  geom_point(aes(y = repsitenames$Y_WGS84, x = repsitenames$X_WGS84), color = "black", size = 8, pch = 15) +
  #geom_label(mapping = aes(y = repsitenames$Y_WGS84, x = repsitenames$X_WGS84), label = repsitenames$site, nudge_x = 2.25, nudge_y = 0.5, size = 3, fill = NA) +
  coord_sf(xlim = c((st_bbox(tdd_hist)[1] + 1), 
                    (st_bbox(tdd_hist)[3] - 1)), 
           ylim = c((st_bbox(tdd_hist)[2] + 3), 
                    (st_bbox(tdd_hist)[4] + 1))) +
  theme(panel.background = element_rect(color = "black", size = 0.5, fill = NA), panel.grid = element_blank(),
        legend.title = element_blank(), legend.position = "bottom", plot.margin = unit(c(2,1,1,1), "cm"),
        legend.spacing.x = unit(c(0.5), "cm"), legend.spacing.y = unit(2,"cm"),
        legend.key.width = unit(1, "cm"), legend.key.height = unit(1.2, "cm"),
        axis.title = element_blank(), legend.justification = "left", 
        legend.text = element_text(size = 26), title = element_text(size = 28), axis.text = element_text(size = 18)) +
  guides(fill = guide_legend(nrow = 4, by_row = TRUE)) +
  #ggtitle(label = "Ecoregions within Study Region") +
  scale_fill_brewer(palette = "Paired") 
  

extent_ecoregions$newname[extent_ecoregions$newname == "California"] <- "Mediterranean California"

extent_ecoregions$newcolor[extent_ecoregions$newname == "Chihuahuan Desert"] <- brewer.pal(12, "Paired")[1]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Colorado Plateau"] <- brewer.pal(12, "Paired")[2]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Columbia Plateau"] <- brewer.pal(12, "Paired")[3]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Eastern Plains"] <- brewer.pal(12, "Paired")[4]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Great Basin"] <- brewer.pal(12, "Paired")[5]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Mediterranean California"] <- brewer.pal(12, "Paired")[6]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Mojave Desert"] <- brewer.pal(12, "Paired")[7]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Northern Plains"] <- brewer.pal(12, "Paired")[8]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Southern Plains"] <- brewer.pal(12, "Paired")[9]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Upper Gila Mountains"] <- brewer.pal(12, "Paired")[10]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Western Cordillera"] <- brewer.pal(12, "Paired")[11]
extent_ecoregions$newcolor[extent_ecoregions$newname == "Wyoming Basin"] <- brewer.pal(12, "Paired")[12]

#Save new mask objects (save again if updating polygons)
setwd("data_preparation/saved_objects&workspaces/mapping_sf_objects")
# save(states_mask,
#         extent_mask,
#         extent_ecoregions,
#         provinces_mask,
#         file = "overlay_boundaries")
load("overlay_boundaries")

#Export map of custom ecoregions
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/ecoregion_figure")
pdf("extent_ecoregion.pdf", height = 14, width = 12)
jpeg("extent_ecoregion.jpg", height = 1600, width = 1600)
print(x)
dev.off()


