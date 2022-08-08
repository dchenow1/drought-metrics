library(ggfortify)
library(vegan)
library(stats)
library(ggrepel)
library(cowplot)

#combine stars objects means into a single stars object
metrics_full = c(tempy = tempy_hist[1], #
                 precy = precy_hist[1], #
                 ppts = ppts_hist[1],
                 tdd = tdd_hist[1],
                 tddld =  tddld_hist[1],
                 tddseasvar = tddseasvar_hist[1],
                 first = first_hist[1],
                 last = last_hist[1],
                 ai = ai_hist[1],
                 wdd = wdd_hist[1], #start of ecological drought metrics
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

#name  metrics (these will show up as labels for PCA loadings)
attributes(metrics_full)[[1]] <- c("MAT", "MAP", "PPTseas", "TDD", "WARM", "TDDmCV", "FIRST", "LAST", "AI",
                                   "WDD", "DDD", "SWA", "CWD", "SWAmCV", "CWDmCV", "WDDseas", 
                                   "SWAseas", "CWDseas", "CWD10max", "DDDmax", "DSI", "DSIn", "SPRINGr", 
                                   "SPRINGrT", "FALLr.", "FALLrT", "FALLrN", "SPRINGrN")


#extract stars object separate stars objects (cropped by ecoregion)
#add.full adds a row to the end wfor entire study region 
#right now the column name containing ecoregion names has to be called "newname"
extract_poly <- 
  function(stars_object, boundary.sf = extent_ecoregions, add.full = TRUE) {
    boundary.sf <- st_transform(boundary.sf, st_crs(stars_object))
    boundary.sf <- st_transform(boundary.sf, st_crs(tdd_hist))
    m <- rep(list(NA), length(unique(boundary.sf$newname)))
    for (i in 1:length(unique(boundary.sf$newname))) {
      #m[[i]] <- aggregate(x = stars_object, by = st_union(ee[ee$newname == unique(ee$newname)[i],]), FUN = st_crop, na.rm = TRUE)
      m[[i]] <- st_crop(metrics_full, st_union(boundary.sf[boundary.sf$newname == unique(boundary.sf$newname)[i],]), crop = FALSE)
      
    } 
    if (add.full == TRUE) {
      m[[length(m) + 1]] <- st_crop(metrics_full, st_union(boundary.sf), crop = FALSE)
    }
    return(m)
    
  }


#make alphabetical list of ecoregion names
ecoregion_ls <- unique(extent_ecoregions$newname[order(extent_ecoregions$newname)])

#extract metrics_full (list of stars objects to include) using extent_ecoregion 
metrics_full_eco <- extract_poly(metrics_full, extent_ecoregions)
length(metrics_full_eco)
#make empty data frame
metrics_full_df <- NA

#fill data frame with columns for metrics 
for (i in 1:length(metrics_full_eco)) {
  newdf <- data.frame(metrics_full_eco[[i]])
  newdf$ecoregion <- ecoregion_ls[i]
  
  metrics_full_df <- rbind(metrics_full_df, newdf)
}

metrics_full_df$ecoregion <- as.factor(metrics_full_df$ecoregion)

#remove na's and use this object for PCA
metrics_full_df2 <- na.omit(metrics_full_df)




#SCREE and BIPLOT layouts for study region and each ecoregion
setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/with_AI")

pca_results_figure <- function(dataset_bound, filename = NULL, region = "ecoregion") {
  load.col.ls <- c(rep("black", 9), rep("blue", 19))
  
  pc <- prcomp(na.omit(dataset_bound[,-c(1,2,31)]), scale = TRUE)
  var_explained1 = pc$sdev^2 / sum(pc$sdev^2)
  eig <- pc[["sdev"]] / sum(pc[["sdev"]])
  var_cu <- summary(pc)$importance[3,]
  sum(data.frame((summary(pc)$importance[1,])^2)[[1]][1])
  
  pc2 <- prcomp(na.omit(dataset_bound[,c(3:11)]), scale = TRUE)
  var_explained2 = pc2$sdev^2 / sum(pc2$sdev^2)
  eig2 <- pc2[["sdev"]] / sum(pc2[["sdev"]])
  var_cu2 <- summary(pc2)$importance[3,]
  sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])
  
  pc3 <- prcomp(na.omit(dataset_bound[,c(12:30)]), scale = TRUE)
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













#TABLE RESULTS
library(sjPlot)

pc_results <- data.frame(ecoregions = unique(metrics_full_df2$ecoregion), 
                         pc1.n = NA, pc1.v = NA,
                         pc2.n = NA, pc2.v = NA,
                         pc3.n = NA, pc3.v = NA)

for (i in 1:length(unique(metrics_full_df2$ecoregion))) {
  
  pc1 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],-c(1,2,31)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc1)$importance[3,], n = names(summary(pc1)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc1.n"] <- as.numeric(x)
  pc_results[i,"pc1.v"] <- sum(data.frame((summary(pc1)$importance[1,])^2)[[1]][1])
  
  pc2 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(3:11)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc2)$importance[3,], n = names(summary(pc2)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc2.n"] <- as.numeric(x)
  pc_results[i,"pc2.v"] <- sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])
  
  pc3 <- prcomp(na.omit(metrics_full_df2[metrics_full_df2$ecoregion == unique(metrics_full_df2$ecoregion)[i],c(12:30)]), scale = TRUE)
  var_cu <- data.frame(pc = summary(pc3)$importance[3,], n = names(summary(pc3)$importance[3,]))
  x <- var_cu$n[var_cu$pc > 0.90][1]
  x <- strsplit(x, split = "C")[[1]][2]
  pc_results[i,"pc3.n"] <- as.numeric(x)
  pc_results[i,"pc3.v"] <- sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])
  
}

pc1 <- prcomp(na.omit(metrics_full_df2[-c(1,2,31)]), scale = TRUE)
var_cu <- data.frame(pc = summary(pc1)$importance[3,], n = names(summary(pc1)$importance[3,]))
x <- var_cu$n[var_cu$pc > 0.90][1]
x <- strsplit(x, split = "C")[[1]][2]
pc_results[13,"pc1.n"] <- as.numeric(x)
pc_results[13,"pc1.v"] <- sum(data.frame((summary(pc1)$importance[1,])^2)[[1]][1])

pc2 <- prcomp(na.omit(metrics_full_df2[c(3:11)]), scale = TRUE)
var_cu <- data.frame(pc = summary(pc2)$importance[3,], n = names(summary(pc2)$importance[3,]))
x <- var_cu$n[var_cu$pc > 0.90][1]
x <- strsplit(x, split = "C")[[1]][2]
pc_results[13,"pc2.n"] <- as.numeric(x)
pc_results[13,"pc2.v"] <- sum(data.frame((summary(pc2)$importance[1,])^2)[[1]][1])

pc3 <- prcomp(na.omit(metrics_full_df2[c(12:30)]), scale = TRUE)
var_cu <- data.frame(pc = summary(pc3)$importance[3,], n = names(summary(pc3)$importance[3,]))
x <- var_cu$n[var_cu$pc > 0.90][1]
x <- strsplit(x, split = "C")[[1]][2]
pc_results[13,"pc3.n"] <- as.numeric(x)
pc_results[13,"pc3.v"] <- sum(data.frame((summary(pc3)$importance[1,])^2)[[1]][1])

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/multivariate_analysis/tables")

tab_df(pc_results,
     file = "pca_summary_table_with_AI.doc")
