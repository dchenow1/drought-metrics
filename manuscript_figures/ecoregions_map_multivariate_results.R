library(rgeos)
library(dplyr)
library(sf)
library(ggpattern)
library(plyr)
library(tidyverse)
library(cowplot)
library(magick) #?



#Ecoregions map with PCA results

#Ecoregion map with labels
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

#Export map of custom ecoregions
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/ecoregion_figure")
pdf("extent_ecoregion.pdf", height = 14, width = 12)
jpeg("extent_ecoregion.jpg", height = 1600, width = 1600)
print(x)
dev.off()





#get centroids of combined ecoregions
#first summarize sf object based on the new ecoregion name

class(extent_ecoregions)
sf::sf_use_s2(FALSE)
ecoregion_centroids <- extent_ecoregions %>% group_by(newname) %>% summarise_all(mean) %>% st_centroid()
ecoregion_centroids <- ecoregion_centroids[,c(1,9)]
ecoregion_centroids$x <- st_coordinates(ecoregion_centroids$geometry)[,1]
ecoregion_centroids$y <- st_coordinates(ecoregion_centroids$geometry)[,2]


setwd("data_preparation/saved_objects&workspaces/mapping_sf_objects")
# save(ecoregion_centroids,
#      file = "ecoregion_centroids")
load("ecoregion_centroids")




legend.e.theme <- ggplot() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.grid.major.y = element_line(size = 0.4, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        #axis.line.y = element_line(),
        axis.ticks.length=unit(.25, "cm"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 26, face = "bold"),
        legend.position = "none") 

plot.bars <- function(name) {
  legend.e.theme +
    ylim(c(0,6)) +
    #xlab(paste("σ2 = ", round(pc_results$pc.cum.var[pc_results$ecoregions == name], digits = 0))) +
    # scale_x_discrete(labels = c(round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "all"], digits = 0),
    #                             round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "clim"], digits = 0),
    #                             round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "soil"], digits = 0))) +
    geom_bar(stat = "identity", 
             color = "black",
             fill = c("gray90", "gray50", "black"),
             aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == name],
                 x = as.factor(pc_results$pc[pc_results$ecoregions == name]),
                 fill = as.factor(pc_results$pc[pc_results$ecoregions == name])))
  
}

legend.bars <- ggplot() +
  theme(panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "white", color = "black"), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(c(40),"mm"),
        legend.text = element_text(size = 32),
        legend.spacing.y = unit(0,"char"),
        legend.spacing.x = unit(2, "char"),
        legend.box.background = element_rect(color = "black", size = 2),
        legend.box.margin = margin(10,10,10,10)) +
  #ylim(0,6) +
  #xlab(paste("σ2 = ", round(pc_results$pc.cum.var[pc_results$ecoregions == name], digits = 0))) +
  scale_x_discrete(labels = c(round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "all"], digits = 0),
                              round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "clim"], digits = 0),
                              round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "soil"], digits = 0))) +
  geom_bar(stat = "identity", 
           color = "black",
           #fill = c("white, "gray90", "black"),
           aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == "Wyoming Basin"],
               x = as.factor(pc_results$pc[pc_results$ecoregions == "Wyoming Basin"]),
               fill = as.factor(pc_results$pc[pc_results$ecoregions == "Wyoming Basin"])))  +
  scale_fill_manual(values = c("gray90", "gray50", "black"), labels = c("All Metrics", "Climate Metrics", "Ecological Drought\nMetrics")) +
  guides(fill = guide_legend(label.position = "right"))

legend.bars <- plot_grid(get_legend(legend.bars))

  
  dev.off()
  x <- ggplot() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    #geom_sf(data = ecoregion_centroids$geometry, size = 2) +
    geom_sf(data = extent_ecoregions, col = "black", size = 0.1, aes(fill = newname), alpha = 0.6) +
    geom_sf(data = provinces_mask, col = "gray20", size = 0.5, fill = NA) +
    geom_sf(data = extent_mask, col = "black", size = 0.05, fill = NA) +
    geom_sf(data = states_mask, col = "gray20", size = 0.5, fill = NA) +
    coord_sf(xlim = c((st_bbox(tdd_hist)[1] + 1), 
                      (st_bbox(tdd_hist)[3] - 1)), 
             ylim = c((st_bbox(tdd_hist)[2] + 3), 
                      (st_bbox(tdd_hist)[4] + 1))) +
    #geom_point(aes(y = repsitenames$Y_WGS84, x = repsitenames$X_WGS84), color = "black", size = 8, pch = 15) +
    annotation_custom(ggplotGrob(plot.bars("Chihuahuan Desert")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Chihuahuan Desert"] + 0.5 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Chihuahuan Desert"] + 0.5 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Chihuahuan Desert"] - 2 - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Chihuahuan Desert"] - 2 + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Colorado Plateau")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Colorado Plateau"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Colorado Plateau"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Colorado Plateau"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Colorado Plateau"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Columbia Plateau")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Columbia Plateau"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Columbia Plateau"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Columbia Plateau"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Columbia Plateau"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars( "Eastern Plains")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Eastern Plains"] + 0.5 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Eastern Plains"] + 0.5 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Eastern Plains"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Eastern Plains"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Great Basin")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Great Basin"] + 0.5 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Great Basin"] + 0.5 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Great Basin"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Great Basin"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Mediterranean California")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Mediterranean California"] - 2 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Mediterranean California"] - 2 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Mediterranean California"] - 1.8 - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Mediterranean California"] - 1.8 + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Mojave Desert")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Mojave Desert"] - 0.5 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Mojave Desert"] - 0.5 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Mojave Desert"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Mojave Desert"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Northern Plains")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Northern Plains"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Northern Plains"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Northern Plains"] + 1 - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Northern Plains"] + 1 + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Southern Plains")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Southern Plains"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Southern Plains"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Southern Plains"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Southern Plains"] + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Upper Gila Mountains")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Upper Gila Mountains"] - 3 - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Upper Gila Mountains"] - 3 + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Upper Gila Mountains"] - 3.5 - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Upper Gila Mountains"] - 3.5 + 1.1) + 
    geom_segment(aes(x = ecoregion_centroids$x[ecoregion_centroids$newname == "Upper Gila Mountains"] - 1, 
                     xend = ecoregion_centroids$x[ecoregion_centroids$newname == "Upper Gila Mountains"] - 2.1, 
                     y = ecoregion_centroids$y[ecoregion_centroids$newname == "Upper Gila Mountains"] - 0.55, 
                     yend =  ecoregion_centroids$y[ecoregion_centroids$newname == "Upper Gila Mountains"] - 3.05), 
                 color = "black", size = 1.2, alpha = 0.8, lineend = "round") + 
    annotation_custom(ggplotGrob(plot.bars("Western Cordillera")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Western Cordillera"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Western Cordillera"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Western Cordillera"] + 3 - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Western Cordillera"] + 3 + 1.1) + 
    annotation_custom(ggplotGrob(plot.bars("Wyoming Basin")),
                      xmin = ecoregion_centroids$x[ecoregion_centroids$newname == "Wyoming Basin"] - 1.1,
                      xmax = ecoregion_centroids$x[ecoregion_centroids$newname == "Wyoming Basin"] + 1.1,
                      ymin = ecoregion_centroids$y[ecoregion_centroids$newname == "Wyoming Basin"] - 1.1,
                      ymax = ecoregion_centroids$y[ecoregion_centroids$newname == "Wyoming Basin"] + 1.1) + 
    annotation_custom((ggplotGrob(legend.bars)),
                      xmin = -104 - 10,
                      xmax = -104 + 20,
                      ymin = 32 - 10,
                      ymax = 32 + 10) + 
  #geom_label(mapping = aes(y = repsitenames$Y_WGS84, x = repsitenames$X_WGS84), label = repsitenames$site, nudge_x = 2.25, nudge_y = 0.5, size = 3, fill = NA) +
    theme(panel.background = element_rect(color = "transparent", size = 0.5, fill = NA), 
          panel.grid = element_blank(),
          legend.title = element_blank(), legend.position = "bottom", plot.margin = margin(0,0,0,0, "cm"),
          legend.box.margin = margin(0,0,0,0,"cm"),
          legend.spacing.x = unit(c(0.5), "cm"), legend.spacing.y = unit(2,"cm"),
          legend.key.width = unit(1.6, "cm"), legend.key.height = unit(1.6, "cm"),
          axis.title = element_blank(), legend.justification = "left", 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 34), title = element_text(size = 28), axis.text = element_text(size = 18)) +
    guides(fill = guide_legend(nrow = 4, by_row = TRUE)) +
    #ggtitle(label = "Ecoregions within Study Region") +
    scale_fill_brewer(palette = "Paired") 
  
  setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/custom_figures")
  jpeg("extent_ecoregion_pcaresults.jpeg", height = 1800, width = 1750)
  print(x)
  dev.off()
















# 
# 
# 
# plot.bars <- function(name) {
#   legend.e.theme +
#     ylim(0,6) +
#     #xlab(paste("σ2 = ", round(pc_results$pc.cum.var[pc_results$ecoregions == name], digits = 0))) +
#     scale_x_discrete(labels = c(round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "all"], digits = 0),
#                                 round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "clim"], digits = 0),
#                                 round(pc_results$pc.cum.var[pc_results$ecoregions == name & pc_results$pc == "soil"], digits = 0))) +
#     geom_bar_pattern(stat = "identity", 
#                      pattern_fill = "black",
#                      pattern_density = 0.1,
#                      color = "black",
#                      fill = "gray90",
#                      aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == name],
#                          x = as.factor(pc_results$pc[pc_results$ecoregions == name]),
#                          pattern = as.factor(pc_results$pc[pc_results$ecoregions == name])))  +
#     geom_text(aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == name],
#                   x = as.factor(pc_results$pc[pc_results$ecoregions == name]),
#                   label = pc_results$pc.n.90.var[pc_results$ecoregions == name]), vjust = -0.3, size = 7, color = "blue")
#   
# }
# 
# legend.bars <- ggplot() +
#   theme(panel.background = element_rect(fill = "transparent", color = NA),
#         plot.background = element_rect(fill = "white", color = "black"), 
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         axis.title.y = element_blank(), 
#         axis.title.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 18, margin = margin(r = 0)),
#         legend.title = element_blank(),
#         legend.key.size = unit(c(20),"mm"),
#         legend.text = element_text(size = 18),
#         legend.spacing.y = unit(c(2),"mm")) +
#   ylim(0,6) +
#   #xlab(paste("σ2 = ", round(pc_results$pc.cum.var[pc_results$ecoregions == name], digits = 0))) +
#   scale_x_discrete(labels = c(round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "all"], digits = 0),
#                               round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "clim"], digits = 0),
#                               round(pc_results$pc.cum.var[pc_results$ecoregions == "Wyoming Basin" & pc_results$pc == "soil"], digits = 0))) +
#   geom_bar_pattern(stat = "identity", 
#                    pattern_fill = "black",
#                    pattern_density = 0.1,
#                    color = "black",
#                    fill = "gray90",
#                    aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == "Wyoming Basin"],
#                        x = as.factor(pc_results$pc[pc_results$ecoregions == "Wyoming Basin"]),
#                        pattern = as.factor(pc_results$pc[pc_results$ecoregions == "Wyoming Basin"])))  +
#   geom_text(aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == "Wyoming Basin"],
#                 x = as.factor(pc_results$pc[pc_results$ecoregions == "Wyoming Basin"]),
#                 label = unique(pc_results$pc)), vjust = -0.3, size = 7, color = "blue")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot.bars <- function(name) {
#   legend.e.theme +
#     ylim(0,6) +
#     geom_bar_pattern(stat = "identity", 
#                      pattern = "gradient",
#                      aes(y = pc_results$pc.n.90.var[pc_results$ecoregions == name],
#                          x = as.factor(pc_results$pc[pc_results$ecoregions == name]),
#                          #pattern = as.factor(pc_results$pc[pc_results$ecoregions == name]),
#                      fill = y)) +
#     scale_fill_gradient2(low = "blue", mid = "gray", high =  "red")
# }
# 
# plot.bars("Wyoming Basin")
# 
# 






