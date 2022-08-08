#This document is for generating both the in-text and appendix figures and tables for the historical drought metrics paper

convert_to_vars_st <- function(data = vars, variability_metric = NULL) {
  
  if (is.null(variability_metric)) {
    vars_m <- vars[[1]][1] 
    names(vars_m) <- paste(names(vars[1]), "mean", sep = ".")
    
    for (i in 2:length(vars)) { 
      
      new_value <- vars[[i]][1] 
      names(new_value) <- paste(names(vars[i]), "mean", sep = ".")
      vars_m <- c(vars_m, new_value)  
    }
    return(vars_m)
    
  } else {
  
    if (variability_metric == "sd") {
      v = 2
    } else if (variability_metric == "cv") {
      v = 3
    } 
    
    #get a list of the means
    vars_m <- vars[[1]][1] 
    names(vars_m) <- paste(names(vars[1]), "mean", sep = ".")
    
    vars_v <- vars[[1]][v] 
    names(vars_v) <- paste(names(vars[1]), variability_metric, sep = ".")
    
    
    for (i in 2:length(vars)) { 
      
      new_value <- vars[[i]][1] 
      names(new_value) <- paste(names(vars[i]), "mean", sep = ".")
      vars_m <- c(vars_m, new_value)  
      
      new_value <- vars[[i]][v] 
      names(new_value) <- paste(names(vars[i]), variability_metric, sep = ".")
      vars_v <- c(vars_v, new_value)  
      
    }
    
    vars_st <- list(vars_m, vars_v)
    
    return(vars_st)
    
    }
}




#In-text figures
#function that creates a list of maps for the variables in the category
map_a_category <- function(vars_st, stars_agree = NULL, metric_name = NULL, bound = NULL, prop = FALSE, force.div = FALSE) {
  p <- list(NA)
  n = 1
  for (i in 1:length(vars)) {
    if (prop == TRUE) {
      p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
      n = n + 1
    } else {
      for (j in 1:length(vars_st)) {
        if (grepl("mean", names(vars_st[[j]][i])) == TRUE) {
          p[[n]] <- plot_cor_map(vars_st[[j]][i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, force.div = force.div)
          n = n + 1
        } else if (grepl("cv", names(vars_st[[j]][i])) == TRUE) {
          p[[n]] <- plot_cor_map(vars_st[[j]][i], stars_agree, metric_name, color.scale = p_colors, div.color.scale = v_div)
          n = n + 1
        } else {
          p[[n]] <- plot_cor_map(vars_st[[j]][i], stars_agree, metric_name, color.scale = p_colors, div.color.scale = p_div)
          n = n + 1
        }
        
      }
    }

  }
  return(p)
}




#NOT WORKING YET

# 1. get lay: matrix of which objects (vectors) should go into each row
# 2. get the heights and widths (vectors) based on lay


#function to get the layout regardless of the number of panels


title.theme <- ggplot() +
  theme(panel.background = element_rect(fill = "gray88", color = NULL, size = 3),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"),
        axis.ticks = element_blank()) 

# select_grobs <- function(lay) {
#   id <- unique(c(t(lay))) 
#   id[!is.na(id)]
# } 
# 
# grid.arrange(grobs=gs[select_grobs(hlay)], layout_matrix=hlay)
# select_grobs(lay)
# 
# 
# get_layout <- function(vars) {
#   
#   lay <- rbind(c(1,1),
#                c(2,3),
#                c(4,4),
#                c(5,6),
#                c(7,7),
#                c(8,9),
#                c(10,10),
#                c(11,12),
#                c(13,13),
#                c(14,15))
# }
# 







# plot_cor_map(vars_st.1[[1]][1], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
# plot_cor_map(precy_hist[[1]], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)



######

#Generate figures for each category

#overall conditions
vars <- list(tdd_hist,
             wdd_hist,
             ddd_hist,
             swa_hist,
             cwd_hist)
names(vars) <- list("TDD",
                    "WDD",
                    "DDD",
                    "SWA",
                    "CWD")
vars.titles <- c("Total Growing Degree Days (dd)",
                 "Wet Degree Days (dd)",
                 "Dry Degree Days (dd)",
                 "Soil Water Availability (mm)",
                 "Climatic Water Deficit (mm)")

vars_st.1 <- convert_to_vars_st(vars, variability_metric = "cv")

p <- map_a_category(vars_st.1)

z <- ggplot() + theme_void()

lay <- rbind(c(1,1,10,10),
             c(2,3,11,12),
             c(4,4,13,13),
             c(5,6,14,15),
             c(7,7,16,16),
             c(8,9,16,16))

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 22, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 22, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 22, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 22, angle = 0)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 22, angle = 0)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("overall.conditions.jpg", width = 3200, height = 2850)
grid.arrange(grobs = list(z.1,
                          p[[1]], p[[2]], 
                          z.2,
                          p[[3]], p[[4]], 
                          z.3,
                          p[[5]], p[[6]], 
                          z.4,
                          p[[7]], p[[8]], 
                          z.5,
                          p[[9]], p[[10]],
                          z),
             nrows = 6,
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             widths = c(800,800,800,800),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()







#seasonal variability
vars <- list(tddseasvar_hist,
             swaseasvar_hist,
             cwdseasvar_hist)
names(vars) <- list("TDDseasvar",
                    "SWAseasvar",
                    "CWDseasvar")
vars.titles <- c("Total Growing Degree Days\nWithin-Year Seasonal Variability",
                 "Soil Water Availability\nWithin-Year Seasonal Variability",
                 "Climatic Water Deficit\nWithin-Year Seasonal Variability")

vars_st.2 <- convert_to_vars_st(vars, variability_metric = "cv")

p <- map_a_category(vars_st.2)

z <- ggplot() + theme_void()

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 0)

lay <- rbind(c(1,1),
             c(2,3),
             c(4,4),
             c(5,6),
             c(7,7),
             c(8,9))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("seasonal.variability.jpg", width = 1600, height = 2850)
grid.arrange(grobs = list(z.1,
                          p[[1]], p[[2]], 
                          z.2,
                          p[[3]], p[[4]], 
                          z.3,
                          p[[5]], p[[6]]),
             nrows = 6,
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             widths = c(800,800),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()





#seasonality
vars <- list(ppts_hist,
             wddseas_hist,
             swaseas_hist,
             cwdseas_hist)
names(vars) <- list("PPTseas",
                    "WDDseas",
                    "SWAseas",
                    "CWDseas")
vars.titles <- c("Precipitation Seasonal Timing",
                 "Wet Degree Days Seasonal Timing",
                 "Soil Water Availability Seasonal Timing",
                 "Climatic Water Deficit Seasonal Timing")

vars_st.3 <- convert_to_vars_st(vars, variability_metric = "sd")

p <- map_a_category(vars_st.3, force.div = TRUE)

z <- ggplot() + theme_void()

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 24, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 24, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 24, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 24, angle = 0)

lay <- rbind(c(1,1),
             c(2,3),
             c(4,4),
             c(5,6),
             c(7,7),
             c(8,9),
             c(10,10),
             c(11,12))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("seasonality.jpg", width = 1600, height = 3800)
grid.arrange(grobs = list(z.1,
                          p[[1]], p[[2]], 
                          z.2,
                          p[[3]], p[[4]], 
                          z.3,
                          p[[5]], p[[6]],
                          z.4,
                          p[[7]], p[[8]]),
             nrows = 8,
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             widths = c(800,800),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()








#extreme hot dry conditions
vars <- list(cwdmx_hist,
             dddsp_hist,
             dsi_hist,
             dsic_hist)
names(vars) <- list("CWD10mx",
                    "DDDsp",
                    "DSI",
                    "DSIc")
vars.titles <- c("Climatic Water Deficit 10-Day Maximum (mm)",
                 "Maximum DDD Spell (dd)",
                 "Dry Soil Intervals (days)",
                 "Number of Dry Soil Intervals")

vars_st.4 <- convert_to_vars_st(vars, variability_metric = "cv")

p <- map_a_category(vars_st.4)

z <- ggplot() + theme_void()

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 24, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 24, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 24, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 24, angle = 0)

lay <- rbind(c(1,1),
             c(2,3),
             c(4,4),
             c(5,6),
             c(7,7),
             c(8,9),
             c(10,10),
             c(11,12))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("extreme.hot-dry.conditions.jpg", width = 1600, height = 3800)
grid.arrange(grobs = list(z.1,
                          p[[1]], p[[2]], 
                          z.2,
                          p[[3]], p[[4]], 
                          z.3,
                          p[[5]], p[[6]],
                          z.4,
                          p[[7]], p[[8]]),
             nrow = 8,
             ncol = 2,
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             widths = c(800,800),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()





#recruitment
vars <- list(sprec_hist,
             frec_hist)
names(vars) <- list("SPREC",
                    "FREC")
vars_st.5 <- convert_to_vars_st(vars, variability_metric = "cv")
p <- map_a_category(vars_st.5)

vars <- list(sprec_n[[1]],
             frec_n[[1]])
names(vars) <- list("SPRCn",
                    "FRECn")
vars_st.6 <- convert_to_vars_st(vars)
q <- map_a_category(vars_st.6, prop = TRUE)

vars.titles <- c("Spring Recruitment Index\n(WDD)",
                 "Fall Recruitment Index\n(WDD)",
                 "Proportion of Years\nwith Recruitment")

z <- ggplot() + theme_void()

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 20, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 20, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 20, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label="Spring", color="black", size = 24, fontface = "bold")
z.6 <- title.theme + annotate("text", x=-1, y=-1, label="Fall", color="black", size = 24, fontface = "bold")

lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,8,9),
             c(10,11,12))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("recruitment.jpg", width = 1800, height = 2550)
grid.arrange(grobs = list(z.1,p[[1]], p[[2]], 
                          z.2,p[[3]], p[[4]], 
                          z,z.5, z.6,
                          z.3,q[[1]], q[[2]]),
             nrows = 4,
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             widths = c(200,800,800),
             heights = c(800,
                         800,
                         150,
                         800)
)
dev.off()







#climate
vars <- list(tempy_hist,
             precy_hist,
             last_hist,
             first_hist,
             fff_hist,
             flf_hist)
names(vars) <- list("MAT",
                    "MAP", 
                    "first",
                    "last",
                    "fff",
                    "flf")
vars.titles <- c("Mean Annual Temperature",
                 "Mean Annual Precipitation",
                 "Timing of First Fall Frost",
                 "Timing of Last Spring Frost",
                 "Proportion of Years\nwith First Fall Frost",
                 "Prportion of Years\nwith Last Spring Frost")

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

vars_st.7 <- convert_to_vars_st(vars, variability_metric = "sd")
vars_st.8 <- convert_to_vars_st(vars, variability_metric = "cv")

p <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("MAT", names(vars_st[i])) == TRUE) {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
    n = n + 1
  } else {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

q <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("MAT", names(vars_st.8[[2]][i])) == TRUE) {
    q[[n]] <- plot_cor_map(vars_st.8[[2]][i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
    n = n + 1
  } else {
    q[[n]] <- plot_cor_map(vars_st.8[[2]][i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

plot_cor_map(vars_st.8[[2]][1], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)

pick

p <- map_a_category(vars_st.7)
q <- map_a_category(vars_st.8)

z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label="Mean Annual\nTemperature (°C)", color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label="Mean Annual\nPrecipitation (mm)", color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label="Timing of First Fall Frost\n(day of year)", color="black", size = 18, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label="Proportion of Years\nwith First Fall Frost", color="black", size = 18, angle = 0)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label="Timing of Last Spring Frost\n(day of year)", color="black", size = 18, angle = 0)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label="Proportion of Years\nwith Last Spring Frost", color="black", size = 18, angle = 0)


lay <- rbind(c(1,1,2),
               c(3,4,2),
               c(5,5,2),
               c(6,7,2),
               c(8,8,9),
               c(10,11,12),
               c(13,13,14),
               c(15,16,17))

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/hdm_manuscript_figures")
jpeg("climate.jpg", width = 1800, height = 2700)

grid.arrange(grobs = list(z.1, z,
                          p[[1]], q[[1]],
                          z.2,
                          q[[3]], q[[4]],
                          z.3, z.4,
                          p[[5]], p[[6]], p[[9]],
                          z.5, z.6, 
                          p[[7]], p[[8]], p[[11]]),
             nrows = 8, 
             layout_matrix = lay,
             #widths = c(1200,600,600,1200,600,600,1200,600,600,1200,600, 600,1200,600,600),
             #widths = rep(800, 3),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()



tempy_hist







#need to add back to map netcdf stuff 
cv.limits = c(min(c(vars_st.1[[2]][[1]],
                    vars_st.2[[2]][[1]],
                    vars_st.4[[2]][[1]],
                    vars_st.5[[2]][[1]]), 
                  na.rm = TRUE),
              
              max(c(vars_st.1[[2]][[1]],
                    vars_st.2[[2]][[1]],
                    vars_st.4[[2]][[1]],
                    vars_st.5[[2]][[1]]), 
                  na.rm = TRUE))

cv.limits = c(min(c(vars_st.1[[2]],
                    vars_st.2[[2]],
                    vars_st.4[[2]],
                    vars_st.5[[2]]), 
                  na.rm = TRUE),
              
              max(c(vars_st.1[[2]],
                    vars_st.2[[2]],
                    vars_st.4[[2]],
                    vars_st.5[[2]]), 
                  na.rm = TRUE))

#In-text tables?
cv.min <- min(c(min(c(min(vars_st.1[[2]][[1]], na.rm = TRUE),
      min(vars_st.1[[2]][[2]], na.rm = TRUE),
      min(vars_st.1[[2]][[3]], na.rm = TRUE),
      min(vars_st.1[[2]][[4]], na.rm = TRUE),
      min(vars_st.1[[2]][[5]], na.rm = TRUE)), 
na.rm = TRUE),

min(c(min(vars_st.2[[2]][[1]], na.rm = TRUE),
      min(vars_st.2[[2]][[2]], na.rm = TRUE),
      min(vars_st.2[[2]][[3]], na.rm = TRUE)), 
    na.rm = TRUE),

min(c(min(vars_st.3[[2]][[1]], na.rm = TRUE),
      min(vars_st.3[[2]][[2]], na.rm = TRUE),
      min(vars_st.3[[2]][[3]], na.rm = TRUE),
      min(vars_st.3[[2]][[4]], na.rm = TRUE)), 
    na.rm = TRUE),

min(c(min(vars_st.4[[2]][[1]], na.rm = TRUE),
      min(vars_st.4[[2]][[2]], na.rm = TRUE),
      min(vars_st.4[[2]][[3]], na.rm = TRUE),
      min(vars_st.4[[2]][[4]], na.rm = TRUE)), 
    na.rm = TRUE),

min(c(min(vars_st.5[[2]][[1]], na.rm = TRUE),
      min(vars_st.5[[2]][[2]], na.rm = TRUE)), 
    na.rm = TRUE)))


cv.max <- max(c(max(c(max(vars_st.1[[2]][[1]], na.rm = TRUE),
                      max(vars_st.1[[2]][[2]], na.rm = TRUE),
                      max(vars_st.1[[2]][[3]], na.rm = TRUE),
                      max(vars_st.1[[2]][[4]], na.rm = TRUE),
                      max(vars_st.1[[2]][[5]], na.rm = TRUE)), 
                    na.rm = TRUE),
                
                max(c(max(vars_st.2[[2]][[1]], na.rm = TRUE),
                      max(vars_st.2[[2]][[2]], na.rm = TRUE),
                      max(vars_st.2[[2]][[3]], na.rm = TRUE)), 
                    na.rm = TRUE),
                
                max(c(max(vars_st.3[[2]][[1]], na.rm = TRUE),
                      max(vars_st.3[[2]][[2]], na.rm = TRUE),
                      max(vars_st.3[[2]][[3]], na.rm = TRUE),
                      max(vars_st.3[[2]][[4]], na.rm = TRUE)), 
                    na.rm = TRUE),
                
                max(c(max(vars_st.4[[2]][[1]], na.rm = TRUE),
                      max(vars_st.4[[2]][[2]], na.rm = TRUE),
                      max(vars_st.4[[2]][[3]], na.rm = TRUE),
                      max(vars_st.4[[2]][[4]], na.rm = TRUE)), 
                    na.rm = TRUE),
                
                max(c(max(vars_st.5[[2]][[1]], na.rm = TRUE),
                      max(vars_st.5[[2]][[2]], na.rm = TRUE)), 
                    na.rm = TRUE)))

cv.lim = c(cv.min, cv.max)





#Appendix figures


#Appendix tables

