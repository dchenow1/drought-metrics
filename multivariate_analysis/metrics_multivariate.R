library(ggfortify)
library(vegan)
library(stats)
library(ggrepel)


ecoregion_ls <- unique(extent_ecoregions$newname[order(extent_ecoregions$newname)])


#combine stars objects means into a single stars object
metrics_full = c(tempy = tempy_hist[1], #
                 precy = precy_hist[1], #
                 ppts = ppts_hist[1],
                 tdd = tdd_hist[1],
                 tddld =  tddld_hist[1],
                 tddseasvar = tddseasvar_hist[1],
                 first = first_hist[1],
                 last = last_hist[1],
                 
                 wdd = wdd_hist[1],
                 ddd = ddd_hist[1],
                 swa = swa_hist[1],
                 cwd = cwd_hist[1],
                 swaseasvar = swaseasvar_hist[1],
                 cwdseasvar = cwdseasvar_hist[1],
                 wddseas = wddseas_hist[1],
                 swaseas = swaseas_hist[1],
                 cwdseas = cwdseas_hist[1],
                 cwdmx = cwdmx_hist[1],
                 dddsp = dddsp_hist[1],
                 dsi = dsi_hist[1],
                 dsic = dsic_hist[1],
                 sprec = sprec_hist[1],
                 sprecd = sprecd_hist[1], #
                 frec = frec_hist[1],
                 frecd = frecd_hist[1], #
                 frecn = frec_n[[1]][1], #
                 sprecn = sprec_n[[1]][1] #
)


attributes(metrics_full)[[1]] <- c("MAT", "MAP", "PPTseas", "TDD", "WARM", "TDDmCV", "FIRST", "LAST", 
                                   "WDD", "DDD", "SWA", "CWD", "SWAmCV", "CWDmCV", "WDDseas", 
                                   "SWAseas", "CWDseas", "CWD10max", "DDDmax", "DSI", "DSIn", "SPRINGr", 
                                   "SPRINGrT", "FALLr.", "FALLrT", "FALLrN", "SPRINGrN")


#extract stars object separate stars objects (cropped by ecoregion)
extract_poly <- 
  function(stars_object, extent_ecoregions) {
    ee <- st_transform(extent_ecoregions, st_crs(stars_object))
    ee <- st_transform(ee, st_crs(tdd_hist))
    m <- rep(list(NA), length(unique(ee$newname)))
    for (i in 1:length(unique(ee$newname))) {
      #m[[i]] <- aggregate(x = stars_object, by = st_union(ee[ee$newname == unique(ee$newname)[i],]), FUN = st_crop, na.rm = TRUE)
      m[[i]] <- st_crop(metrics_full, st_union(ee[ee$newname == unique(ee$newname)[i],]), crop = FALSE)
      
    } 
    return(m)
    
  }



extent_all_polygons <- rbind(st_union(extent_ecoregions), extent_ecoregions)

#extract metrics_full
metrics_full_eco <- extract_poly(metrics_full, extent_ecoregions)

metrics_full_df <- NA

for (i in 1:12) {
  newdf <- data.frame(metrics_full_eco[[i]])
  newdf$ecoregion <- ecoregion_ls[i]
  
  metrics_full_df <- rbind(metrics_full_df, newdf)
}


#save objects the last couple objects
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/objects&workspaces")

save(metrics_full,
     metrics_full_eco,
     metrics_full_df,
     file = "combined_drought_metrics")

load("combined_drought_metrics")





##### PCA #####
#convert ecoregion column to factor (for coloring plot)
metrics_full_df$ecoregion <- as.factor(metrics_full_df$ecoregion)

#remove na's and use this object for PCA
metrics_full_df2 <- na.omit(metrics_full_df)

#make color lists for loadings arrows (should make a function that does this)
#climate variables
load.col.ls <- c(rep("black", 8), rep("blue", 19))

#PCA excluding columns: lon, lat, ecoregion
pc <- prcomp(na.omit(metrics_full_df2[,-c(1,2,30)]), scale = TRUE)

#make a biplot
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
jpeg(filename = "biplot_all.jpg", width = 8, height = 6, res = 600, units = "in")
m <- autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = alpha(load.col.ls, 0.6), loadings.label.colour = load.col.ls, 
         loadings.label.size = 3, loadings.arrow.size = 3, 
         data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
dev.off()

#make a scree plot
#convert loadings to variance
var_explained1 = pc$sdev^2 / sum(pc$sdev^2)

jpeg(filename = "scree_all.jpg", width = 6, height = 6, res = 600, units = "in")
ggplot() + 
  geom_line(mapping = aes(c(1:27), var_explained1), color = "#8DD3C7") + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
dev.off()










#PCA with all variables (by ecoregion)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/ecoregions")

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  
  jpeg(filename = paste("biplot_climate", unique(metrics_full_df2$ecoregion)[i], ".jpg", sep = ""), width = 8, height = 6, res = 600, units = "in")
  x <- autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 0.6), loadings.label.colour =  alpha(load.col.ls, 0.6), 
           loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i])
  print(x)
  dev.off()
  
  var_explained = pc$sdev^2 / sum(pc$sdev^2)
  
  jpeg(filename = paste("scree_climate", unique(metrics_full_df2$ecoregion)[i], ".jpg", sep = ""), width = 6, height = 6, res = 600, units = "in")
  x <- ggplot() + 
    geom_line(mapping = aes(c(1:length(var_explained)), var_explained)) + 
    xlab("Principal Component") + 
    ylab("Variance Explained") +
    ggtitle("Scree Plot") +
    ylim(0, 1)
  print(x)
  dev.off()
}




#for ecoregions:

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/ecoregions")

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {

pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
var_explained1 = pc$sdev^2 / sum(pc$sdev^2)
eig <- pc[["sdev"]] / sum(pc[["sdev"]])
var_cu <- summary(pc)$importance[3,]
sum(data.frame((summary(pc)$importance[1,])^2)[[1]][1])

pc2 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:10)]), scale = TRUE)
var_explained2 = pc2$sdev^2 / sum(pc2$sdev^2)
eig2 <- pc2[["sdev"]] / sum(pc2[["sdev"]])
var_cu2 <- summary(pc2)$importance[3,]
sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])

pc3 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(11:29)]), scale = TRUE)
var_explained3 = pc3$sdev^2 / sum(pc3$sdev^2)
eig3 <- pc3[["sdev"]] / sum(pc3[["sdev"]])
var_cu3 <- summary(pc3)$importance[3,]
sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])

print(unique(metrics_full_df2$ecoregion)[i])
print(cbind(var_cu, var_cu2, var_cu3))
}            
x <- ggplot() + 
  geom_line(mapping = aes(c(1:length(eig)), eig, col = "All Variables"), lwd = 0.7) + 
  geom_line(mapping = aes(c(1:length(eig2)), eig2, col = "Climate\nVariables"),  linetype = 1, lwd = 0.7) + 
  geom_line(mapping = aes(c(1:length(eig3)), eig3, col = "Soil Moisture\nVariables"),  linetype = 1, lwd = 0.7) + 
  geom_line(mapping = aes(c(1:length(var_cu)), var_cu/2, col = "All Variables"),  linetype = 2, lwd = 0.7) + 
  geom_line(mapping = aes(c(1:length(var_cu2)), var_cu2/2,  col = "Climate\nVariables"),  linetype = 2, lwd = 0.7) + 
  geom_line(mapping = aes(c(1:length(var_cu3)), var_cu3/2, col = "Soil Moisture\nVariables"),  linetype = 2, lwd = 0.7) + 
  xlab("Principal Component") + 
  ylab("Eigenvalues") +
  ggtitle(unique(metrics_full_df2$ecoregion)[i]) +
  scale_y_continuous(limits = c(0,0.5), expand = c(0,0.05), sec.axis = sec_axis(~2*., name = "Cumulative Variance Explained")) +
  scale_x_continuous(limits = c(1,10), expand = c(0,0.25)) +
  scale_colour_manual(values = c("All Variables" = alpha("orange", 0.8),
                                 "Climate\nVariables" = alpha("black", 0.8),
                                 "Soil Moisture\nVariables" = alpha("blue", 0.8)))

a <- autoplot(pc, x = 1, y = 2, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
         loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i], loadings.label.hjust = 1.1, loadings.label.vjust = 1.1, 
         colour = alpha("gray40", 0.8))

b <- autoplot(pc, x = 1, y = 3, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
         loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i], loadings.label.hjust = 1.1, loadings.label.vjust = 1.1,
         colour = alpha("gray40", 0.8))

c <- autoplot(pc, x = 2, y = 3, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
         loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i], loadings.label.hjust = 1.1, loadings.label.vjust = 1.1,
         colour = alpha("gray40", 0.8))

jpeg(filename = paste("ecoregion_pca_", unique(metrics_full_df2$ecoregion)[i], ".jpg", sep = ""), width = 12, height = 12, res = 800, units = "in")
grid.arrange(x, a, b, c)
dev.off()

}




pc_results <- data.frame(ecoregions = unique(metrics_full_df2$ecoregion), 
                         pc1.n = NA, pc1.v = NA,
                         pc2.n = NA, pc2.v = NA,
                         pc3.n = NA, pc3.v = NA)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  
  pc1 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc1)$importance[3,], n = names(summary(pc1)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc1.n"] <- as.numeric(x)
  pc_results[i,"pc1.v"] <- sum(data.frame((summary(pc1)$importance[1,])^2)[[1]][1])
  
  pc2 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:10)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc2)$importance[3,], n = names(summary(pc2)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc2.n"] <- as.numeric(x)
  pc_results[i,"pc2.v"] <- sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])
  
  pc3 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(11:29)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc3)$importance[3,], n = names(summary(pc3)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc3.n"] <- as.numeric(x)
  pc_results[i,"pc3.v"] <- sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])
  
}

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/objects&workspaces")

save(pc_results,
     file = "pca_summary_table")
load("pca_summary_table")










#long format pca results by ecoregion
pc_results <- data.frame(ecoregions = rep(unique(metrics_full_df2$ecoregion), 3), 
                         pc = rep(c("all", "clim", "soil"), each = length(unique(metrics_full_df2$ecoregion)), ),
                         pc.n.90.var = NA,
                         pc.cum.var = NA)

len <- length(unique(metrics_full_df2$ecoregion))
       
for (i in 1:len) {
  
         
  pc1 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc1)$importance[3,], n = names(summary(pc1)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc.n.90.var"] <- as.numeric(x)
  pc_results[i,"pc.cum.var"] <- sum(data.frame((summary(pc1)$importance[1,])^2)[[1]][1])
  
  pc2 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:10)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc2)$importance[3,], n = names(summary(pc2)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i + len,"pc.n.90.var"] <- as.numeric(x)
  pc_results[i + len,"pc.cum.var"] <- sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])
  
  pc3 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(11:29)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc3)$importance[3,], n = names(summary(pc3)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i + 2*len,"pc.n.90.var"] <- as.numeric(x)
  pc_results[i + 2*len,"pc.cum.var"] <- sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])
  
}










#For the entire study region:
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/without_AI")

pca_results_figure <- function(dataset_bound, filename = NULL, region = "ecoregion") {
          pc <- prcomp(na.omit(dataset_bound[,-c(1,2,30)]), scale = TRUE)
          var_explained1 = pc$sdev^2 / sum(pc$sdev^2)
          eig <- pc[["sdev"]] / sum(pc[["sdev"]])
          var_cu <- summary(pc)$importance[3,]
          sum(data.frame((summary(pc)$importance[1,])^2)[[1]][1])
          
          pc2 <- prcomp(na.omit(dataset_bound[,c(3:10)]), scale = TRUE)
          var_explained2 = pc2$sdev^2 / sum(pc2$sdev^2)
          eig2 <- pc2[["sdev"]] / sum(pc2[["sdev"]])
          var_cu2 <- summary(pc2)$importance[3,]
          sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])
          
          pc3 <- prcomp(na.omit(dataset_bound[,c(11:29)]), scale = TRUE)
          var_explained3 = pc3$sdev^2 / sum(pc3$sdev^2)
          eig3 <- pc3[["sdev"]] / sum(pc3[["sdev"]])
          var_cu3 <- summary(pc3)$importance[3,]
          sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])
          
          
          x <- ggplot() + 
            geom_line(mapping = aes(c(1:length(eig)), eig, col = "All Metrics"), lwd = 0.8) + 
            geom_line(mapping = aes(c(1:length(eig2)), eig2, col = "Climate Metrics"),  linetype = 1, lwd = 0.8) + 
            geom_line(mapping = aes(c(1:length(eig3)), eig3, col = "Ecological Drought\nMetrics"),  linetype = 1, lwd = 0.8) + 
            geom_line(mapping = aes(c(1:length(var_cu)), var_cu/2, col = "All Metrics"),  linetype = 2, lwd = 0.8) + 
            geom_line(mapping = aes(c(1:length(var_cu2)), var_cu2/2,  col = "Climate Metrics"),  linetype = 2, lwd = 0.8) + 
            geom_line(mapping = aes(c(1:length(var_cu3)), var_cu3/2, col = "Ecological Drought\nMetrics"),  linetype = 2, lwd = 0.8) + 
            xlab("Number of Principal Components") + 
            ylab("Eigenvalues") +
            scale_y_continuous(limits = c(0,0.5), expand = c(0,0.05), sec.axis = sec_axis(~2*., name = "Cumulative Variance Explained")) +
            scale_x_continuous(limits = c(1,8), expand = c(0,0.25)) +
            scale_colour_manual(values = c("All Metrics" = alpha("orange", 0.8),
                                           "Climate Metrics" = alpha("black", 0.8),
                                           "Ecological Drought\nMetrics" = alpha("blue", 0.8))) +
            theme(plot.margin = margin(1,0,1,1, "cm"),
                  axis.text = element_text(size = 14),
                  axis.title.x = element_text(size = 16, vjust = -2),
                  axis.title.y = element_text(size = 16, vjust = 4),
                  axis.title.y.right = element_text(size = 16, vjust = 4),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray80"),
                  legend.title = element_blank(),
                  legend.box.background = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.spacing.x = unit(0.5, "cm"),
                  legend.spacing.y = unit(4, "cm"),
                  legend.box.margin = margin(0,0.1,0,0.5, "cm"),
                  legend.margin = margin(0,0,0,0,"cm"),
                  legend.key.height = unit(1.25, "cm"),
                  legend.key.width = unit(1, "cm"),
                  legend.key = element_blank()) +
            guides(fill = guide_legend(byrow = FALSE))
          
          a <- autoplot(pc, x = 1, y = 2, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
                        loadings.label.size = 4, loadings.arrow.size = 3, size = 0.01, loadings.label.hjust = 0, loadings.label.vjust = 0, loadings.label.repel = TRUE, box.padding = 0.5,
                        max.overlaps = 1, point.padding = unit(0.1, "char"),
                        data = (dataset_bound), colour = "ecoregion") +
            theme(plot.margin = margin(1,10,1,5, "cm"),
                  axis.text = element_text(size = 11),
                  axis.title.x = element_text(size = 16, vjust = -4),
                  axis.title.y = element_text(size = 16, vjust = 4),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray80"),
                  legend.title = element_blank(),
                  legend.box.background = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.spacing.x = unit(0.5, "cm"),
                  legend.spacing.y = unit(4, "cm"),
                  legend.box.margin = margin(0,0,0,0, "cm"),
                  legend.margin = margin(0,0,0,0,"cm"),
                  legend.key.height = unit(0.6, "cm"),
                  legend.key.width = unit(0.1, "cm"),
                  legend.key = element_blank(),
                  legend.position = "right",
                  legend.justification = "right") +
            guides(colour = guide_legend(override.aes = list(size = 3))) 
          
          y <- get_legend(a)
          y <- plot_grid(y) +
            theme(plot.margin = margin(0,5,5,0, "cm"),
                  legend.margin = margin(0,0,0,0, "cm"))
          
          a <- autoplot(pc, x = 1, y = 2, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
                        loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, loadings.label.hjust = 0, loadings.label.vjust = 0, loadings.label.repel = TRUE, box.padding = 0.5,
                        max.overlaps = 1, point.padding = unit(0.1, "char"),
                        data = (dataset_bound), colour = "ecoregion") +
            theme(plot.margin = margin(0,0,1,1, "cm"),
                  axis.text = element_text(size = 11),
                  axis.title.x = element_text(size = 12, vjust = -4),
                  axis.title.y = element_text(size = 12, vjust = 4),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray80"),
                  legend.position = "none") +
            guides(colour = guide_legend(override.aes = list(size = 3))) 
          
          b <- autoplot(pc, x = 1, y = 3, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
                        loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, loadings.label.hjust = 0, loadings.label.vjust = 0, loadings.label.repel = TRUE, box.padding = 0.5,
                        max.overlaps = 1, point.padding = unit(0.1, "char"),
                        data = (dataset_bound), colour = "ecoregion") +
            theme(plot.margin = margin(0,0,1,1, "cm"),
                  axis.text = element_text(size = 11),
                  axis.title.x = element_text(size = 12, vjust = -4),
                  axis.title.y = element_text(size = 12, vjust = 4),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray80"),
                  legend.position = "none") +
            guides(colour = guide_legend(override.aes = list(size = 3))) 
          
          c <- autoplot(pc, x = 2, y = 3, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 1), loadings.label.colour =  alpha(load.col.ls, 1), 
                        loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, loadings.label.hjust = 0, loadings.label.vjust = 0, loadings.label.repel = TRUE, box.padding = 0.5,
                        max.overlaps = 1, point.padding = unit(0.1, "char"),
                        data = (dataset_bound), colour = "ecoregion") +
            theme(plot.margin = margin(0,0,1,1, "cm"),
                  axis.text = element_text(size = 11),
                  axis.title.x = element_text(size = 12, vjust = -4),
                  axis.title.y = element_text(size = 12, vjust = 4),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray80"),
                  legend.position = "none") +
            guides(colour = guide_legend(override.aes = list(size = 3))) 
          
          lay <- rbind(c(1,1,2),
                       c(3,4,5))
          z <- ggplot() + theme_void()
          
          jpeg(filename = paste(filename, ".jpg", sep = ""), width = 14, height = 8, res = 800, units = "in")
          if (region == "full") {
            grid.arrange(grobs = list(x, y, a, b, c), nrows = 2, layout_matrix = lay)
          } else {
            z <- ggplot() + theme_void()
            grid.arrange(grobs = list(x, z, a, b, c), nrows = 2, layout_matrix = lay)
          }
          dev.off()
        
}

pca_results_figure(dataset_bound = metrics_full_df2, "study_region", region = "full")


pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Chihuahuan Desert",], "Chihuahuan Desert", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Colorado Plateau",], "Colorado Plateau", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Columbia Plateau",], "Columbia Plateau", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Eastern Plains",], "Eastern Plains", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Great Basin",], "Great Basin", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Mediterranean California",], "Mediterranean_California", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Mojave Desert",], "Mojave Desert", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Northern Plains",], "Northern Plains", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Southern Plains",], "Southern Plains", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Upper Gila Mountains",], "Upper Gila Mountains", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Western Cordillera",], "Western Cordillera", region = "ecoregion")
pca_results_figure(dataset_bound = metrics_full_df2[metrics_full_df2$ecoregion == "Wyoming Basin",], "Wyoming Basin", region = "ecoregion")










unique(metrics_full_df2$ecoregion)



















x <- rep(list(NA), 12)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  
  x[[i]] <- autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour =  alpha(load.col.ls, 0.6), loadings.label.colour =  alpha(load.col.ls, 0.6), 
                loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i])
  
}



x <- rep(list(NA), 12)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:11)]), scale = TRUE)
  
  x[[i]] <- autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour =  "black", loadings.label.colour =  "black", 
                     loadings.label.size = 3, loadings.arrow.size = 3, size = 0.01, main = unique(metrics_full_df2$ecoregion)[i])
  
}


  
y <- rep(list(NA), 12)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
    
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  
  var_explained = pc$sdev^2 / sum(pc$sdev^2)
  
  y[[i]] <- ggplot() + 
    geom_line(mapping = aes(c(1:length(var_explained)), var_explained)) + 
    xlab("Principal Component") + 
    ylab("Variance Explained") +
    ggtitle("Scree Plot") +
    ylim(0, 1)

}




y <- rep(list(NA), 12)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  
  y[[i]] = pc$sdev^2 / sum(pc$sdev^2)
  
  
}



z <- rep(list(NA), 12)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:11)]), scale = TRUE)
  
  z[[i]] = pc$sdev^2 / sum(pc$sdev^2)
  
  
}



y <- rep(list(NA), 12)
y <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

y <- NA

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  
  pc <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,30)]), scale = TRUE)
  
  y = cbind(y, pc$sdev^2 / sum(pc$sdev^2))
  
}
y

str(y)
length(z[[1]])

m <- ggplot() + 
  geom_line(mapping = aes(c(1:27), y[[1]], color = "California")) + 
  geom_line(mapping = aes(c(1:27), y[[2]], color = "Chihuahuan Desert")) + 
  geom_line(mapping = aes(c(1:27), y[[3]], color = "Colorado Plateau")) + 
  geom_line(mapping = aes(c(1:27), y[[4]], color = "Columbia Plateau")) + 
  geom_line(mapping = aes(c(1:27), y[[5]], color = "Eastern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[6]], color = "Great Basin")) + 
  geom_line(mapping = aes(c(1:27), y[[7]], color = "Mojave Desert")) + 
  geom_line(mapping = aes(c(1:27), y[[8]], color = "Northern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[9]], color = "Southern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[10]], color = "Upper Gila Mountains")) + 
  geom_line(mapping = aes(c(1:27), y[[11]], color = "Western Cordillera")) + 
  geom_line(mapping = aes(c(1:27), y[[12]], color = "Wyoming Basin ")) + 
  geom_line(mapping = aes(c(1:9), z[[1]], color = "California")) + 
  geom_line(mapping = aes(c(1:9), z[[2]], color = "Chihuahuan Desert"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[3]], color = "Colorado Plateau"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[4]], color = "Columbia Plateau"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[5]], color = "Eastern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[6]], color = "Great Basin"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[7]], color = "Mojave Desert"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[8]], color = "Northern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[9]], color = "Southern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[10]], color = "Upper Gila Mountains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[11]], color = "Western Cordillera"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[12]], color = "Wyoming Basin "), linetype = 3) + 
  scale_color_manual(values = c("California" = brewer.pal(12, "Set3")[1],
                                "Chihuahuan Desert" = brewer.pal(12, "Set3")[2],
                                "Colorado Plateau" = brewer.pal(12, "Set3")[3],
                                "Columbia Plateau" = brewer.pal(12, "Set3")[4],
                                "Eastern Plains" = brewer.pal(12, "Set3")[5],
                                "Great Basin" = brewer.pal(12, "Set3")[6],
                                "Mojave Desert" = brewer.pal(12, "Set3")[7],
                                "Northern Plains" = brewer.pal(12, "Set3")[8],
                                "Southern Plains" = brewer.pal(12, "Set3")[9],
                                "Upper Gila Mountains" = brewer.pal(12, "Set3")[10],
                                "Western Cordillera" = brewer.pal(12, "Set3")[11],
                                "Wyoming Basin " = brewer.pal(12, "Set3")[12])) + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme(legend.position = "top") +
  ylim(0, 1) 

m1 <- ggplot() + 
  geom_line(mapping = aes(c(1:27), y[[1]], color = "California")) + 
  geom_line(mapping = aes(c(1:27), y[[2]], color = "Chihuahuan Desert")) + 
  geom_line(mapping = aes(c(1:27), y[[3]], color = "Colorado Plateau")) + 
  geom_line(mapping = aes(c(1:27), y[[4]], color = "Columbia Plateau")) + 
  geom_line(mapping = aes(c(1:27), y[[5]], color = "Eastern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[6]], color = "Great Basin")) + 
  geom_line(mapping = aes(c(1:27), y[[7]], color = "Mojave Desert")) + 
  geom_line(mapping = aes(c(1:27), y[[8]], color = "Northern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[9]], color = "Southern Plains")) + 
  geom_line(mapping = aes(c(1:27), y[[10]], color = "Upper Gila Mountains")) + 
  geom_line(mapping = aes(c(1:27), y[[11]], color = "Western Cordillera")) + 
  geom_line(mapping = aes(c(1:27), y[[12]], color = "Wyoming Basin ")) + 
  geom_line(mapping = aes(c(1:9), z[[1]], color = "California")) + 
  scale_color_manual(values = c("California" = brewer.pal(12, "Set3")[1],
                                "Chihuahuan Desert" = brewer.pal(12, "Set3")[2],
                                "Colorado Plateau" = brewer.pal(12, "Set3")[3],
                                "Columbia Plateau" = brewer.pal(12, "Set3")[4],
                                "Eastern Plains" = brewer.pal(12, "Set3")[5],
                                "Great Basin" = brewer.pal(12, "Set3")[6],
                                "Mojave Desert" = brewer.pal(12, "Set3")[7],
                                "Northern Plains" = brewer.pal(12, "Set3")[8],
                                "Southern Plains" = brewer.pal(12, "Set3")[9],
                                "Upper Gila Mountains" = brewer.pal(12, "Set3")[10],
                                "Western Cordillera" = brewer.pal(12, "Set3")[11],
                                "Wyoming Basin " = brewer.pal(12, "Set3")[12])) + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme(legend.position = "top") +
  ylim(0, 1) 


m2 <- ggplot() + 
  geom_line(mapping = aes(c(1:9), z[[2]], color = "Chihuahuan Desert"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[3]], color = "Colorado Plateau"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[4]], color = "Columbia Plateau"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[5]], color = "Eastern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[6]], color = "Great Basin"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[7]], color = "Mojave Desert"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[8]], color = "Northern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[9]], color = "Southern Plains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[10]], color = "Upper Gila Mountains"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[11]], color = "Western Cordillera"), linetype = 3) + 
  geom_line(mapping = aes(c(1:9), z[[12]], color = "Wyoming Basin "), linetype = 3) + 
  scale_color_manual(values = c("California" = brewer.pal(12, "Set3")[1],
                                "Chihuahuan Desert" = brewer.pal(12, "Set3")[2],
                                "Colorado Plateau" = brewer.pal(12, "Set3")[3],
                                "Columbia Plateau" = brewer.pal(12, "Set3")[4],
                                "Eastern Plains" = brewer.pal(12, "Set3")[5],
                                "Great Basin" = brewer.pal(12, "Set3")[6],
                                "Mojave Desert" = brewer.pal(12, "Set3")[7],
                                "Northern Plains" = brewer.pal(12, "Set3")[8],
                                "Southern Plains" = brewer.pal(12, "Set3")[9],
                                "Upper Gila Mountains" = brewer.pal(12, "Set3")[10],
                                "Western Cordillera" = brewer.pal(12, "Set3")[11],
                                "Wyoming Basin " = brewer.pal(12, "Set3")[12])) + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme(legend.position = "top") +
  ylim(0, 1) 



jpeg(filename = "scree_ecoregions_climate.jpg", width = 18, height = 15, res = 600, units = "in")
print(m2)
dev.off()





ggplot() + 
  geom_line(mapping = aes(c(1:27), y[[1]])) + 
  geom_line(mapping = aes(c(1:27), y[[2]])) + 
  geom_line(mapping = aes(c(1:27), y[[3]])) + 
  geom_line(mapping = aes(c(1:27), y[[4]])) + 
  geom_line(mapping = aes(c(1:27), y[[5]])) + 
  geom_line(mapping = aes(c(1:27), y[[6]])) + 
  geom_line(mapping = aes(c(1:27), y[[7]])) + 
  geom_line(mapping = aes(c(1:27), y[[8]])) + 
  geom_line(mapping = aes(c(1:27), y[[9]])) + 
  geom_line(mapping = aes(c(1:27), y[[10]])) + 
  geom_line(mapping = aes(c(1:27), y[[11]])) + 
  geom_line(mapping = aes(c(1:27), y[[12]])) + 
  xlab("Principal Component") + 
  scale_color_brewer(palette) + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme(legend.position = "top") +
  ylim(0, 1) 



ggplot() + 
  geom_line(mapping = aes(c(1:27), y[[1]]), color = "#8DD3C7") + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1) 



ggplot() + 
  geom_line(mapping = aes(c(1:y[1]), y), col = y) + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1) +
  colors(scale_color_brewer(palette = "Set3"))


brewer.pal(12, "Set3")[1]

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/ecoregions")

jpeg(filename = "biplot_ecoregions.jpg", width = 18, height = 24, res = 600, units = "in")
grid.arrange(x[[1]], x[[2]], x[[3]], x[[4]],
             x[[5]], x[[6]], x[[7]], x[[8]],
             x[[9]], x[[10]], x[[11]], x[[12]])
dev.off()


setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/ecoregions")

jpeg(filename = "biplot_ecoregions_climate.jpg", width = 18, height = 24, res = 600, units = "in")
grid.arrange(x[[1]], x[[2]], x[[3]], x[[4]],
             x[[5]], x[[6]], x[[7]], x[[8]],
             x[[9]], x[[10]], x[[11]], x[[12]])
dev.off()









































#PCA with only climate variables
pc <- prcomp(na.omit(metrics_full_df2[,c(3:11)]), scale = TRUE)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
jpeg(filename = "biplot_climate.jpg", width = 8, height = 6, res = 600, units = "in")
autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = alpha("black", 0.6), loadings.label.colour = "black", 
         loadings.label.size = 3, loadings.arrow.size = 3, 
         data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 

dev.off()

var_explained2 = pc$sdev^2 / sum(pc$sdev^2)

jpeg(filename = "scree_climate.jpg", width = 6, height = 6, res = 600, units = "in")
ggplot() + 
  geom_line(mapping = aes(c(1:9), var_explained2)) + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
dev.off()






# 
# 
# #Overall conditions
# pc <- prcomp(na.omit(metrics_full_df2[,c(3:10)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_overall_conditions.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# 
# #Overall moisture
# pc <- prcomp(na.omit(metrics_full_df2[,c(5:8, 27)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_overall_moisture.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# 
# 
# #Overall temperature
# pc <- prcomp(na.omit(metrics_full_df2[,c(3,4,9,10,26)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_overall_temperature.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# #seasonal variability
# pc <- prcomp(na.omit(metrics_full_df2[,c(11:13)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_seasonal_variability.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# 
# 
# 
# #seasonal timing
# pc <- prcomp(na.omit(metrics_full_df2[,c(14:17)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_seasonal_timing.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# 
# 
# #extreme drought
# pc <- prcomp(na.omit(metrics_full_df2[,c(18:21)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_extreme_drought.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 
# 
# #recruitment
# pc <- prcomp(na.omit(metrics_full_df2[,c(22:25)]), scale = TRUE)
# 
# setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis")
# jpeg(filename = "biplot_recruitment.jpg", width = 6, height = 6, res = 600, units = "in")
# autoplot(pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "black", loadings.label.colour = "black", loadings.label.size = 2.5,
#          data = (metrics_full_df2), colour = "ecoregion", size = 0.01) 
# 
# dev.off()
# 



# climate <- cbind(tempy = tempy_polys_table,
#                  precy = precy_polys_table,
#                  ppts = ppts_polys_table)
# 
# metrics_ecoregion_summ <- climate[c(1,2,5,8)]
# 
# climate <- cbind(tdd = tdd_polys_table,
#                  tddld = tddld_polys_table,
#                  wdd = wdd_polys_table,
#                  ddd = ddd_polys_table,
#                  swa = swa_polys_table,
#                  cwd = cwd_polys_table,
#                  first = first_polys_table,
#                  last = last_polys_table)
# 
# metrics_ecoregion_summ <- cbind(metrics_ecoregion_summ, climate[c(1,2,5,8,11, 14, 17, 20, 23)])
# 
# 
# climate <- cbind(tddseasvar = tddseasvar_polys_table,
#                  swaseasvar = swaseasvar_polys_table,
#                  cwdseasvar = cwdseasvar_polys_table)
# 
# metrics_ecoregion_summ <- cbind(metrics_ecoregion_summ, climate[c(1,2,5,8)])
# 
# climate <- cbind(ppts = ppts_polys_table,
#                  wddseas = wddseas_polys_table,
#                  swaseas = swaseas_polys_table,
#                  cwdseas = cwdseas_polys_table)
# 
# metrics_ecoregion_summ <- cbind(metrics_ecoregion_summ, climate[c(1,2,5,8,11)])
# 
# climate <- cbind(cwdmx = cwdmx_polys_table,
#                  dddsp = dddsp_polys_table,
#                  dsi = dsi_polys_table,
#                  dsic = dsic_polys_table)
# 
# metrics_ecoregion_summ <- cbind(metrics_ecoregion_summ, climate[c(1,2,5,8,11)])
# 
# climate <- cbind(sprec = sprec_polys_table,
#                  sprecd = sprecd_polys_table,
#                  frec = frec_polys_table,
#                  frecd = frecd_polys_table,
#                  rec = rec_n_polys_table)
# 
# metrics_ecoregion_summ <- cbind(metrics_ecoregion_summ, climate[c(1,2,5,8,11,14)])

