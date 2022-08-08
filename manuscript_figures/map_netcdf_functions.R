#~~~~~~~~~~~~~~~~~~~~~~~
#Load required packages:
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(stars) #spatiotemporal rasters
library(ncmeta)
library(rSW2st)
library(rmapshaper)
library(rgeos)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
library(ggplotify)
library(grid)
library(scales)
library(viridis)
library(scico)

#remotes::install_github("DrylandEcology/rSW2st")
######################################################################################

#Functions to get max and min values for scale limits
find_stars_max <- function(x, y) {
  my_vec <- vector(length = length(x))
  for (i in 1:length(x)) {
    my_vec[i] <- max(x[[i]], na.rm = TRUE)
  }
  return(max(my_vec))
}
find_stars_min <- function(x) {
  my_vec <- vector(length = length(x))
  for (i in 1:length(x)) {
    my_vec[i] <- min(x[[i]], na.rm = TRUE)
  }
  return(min(my_vec))
}

#Function (from DRS) to make isoline vectors  based on threshold value
#Create simplified polygon representation of mask (needs `raster::raster`)
simple_mask_outline <- function(
  grid, 
  alpha = 1, 
  keep = 0.2, 
  crs = "OGC:CRS84",
  threshold_km = NA,
  method = "ksmooth"
) {
  prev_use_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(prev_use_s2))

  tmp <- rSW2st::isoline_from_raster(grid = grid, alpha = alpha)
  tmp <- sf::st_cast(sf::st_as_sf(tmp), "POLYGON")
  sf::st_crs(tmp) <- crs
  
  pg_mask1 <- if (is.finite(keep)) {
    # simplify polygon
    rmapshaper::ms_simplify(tmp, keep = keep)
  } else {
    tmp
  }
  
  pg_mask1 <- sf::st_make_valid(pg_mask1)
  
  pg_mask2 <- if (is.finite(threshold_km)) {
    # remove small holes
    sf::st_make_valid(smoothr::fill_holes(
      pg_mask1,
      threshold = units::set_units(threshold_km, km^2)
    ))
  } else {
    pg_mask1
  }
  
  if (is.finite(method)) {
    # smooth edges
    sf::st_make_valid(smoothr::smooth(pg_mask2, method = method))
  } else {
    pg_mask2
  }
}

############################### ~~ Functions added to ggplot ~~ #############################
#assigns color scale and midpoint and limits
#based on the name of the attribute being plotted
#need to change this so that limits can be consistent across variable

#list of metric names (added as labels)
get_name <- function(stars_object) {
  if (names(stars_object) == "mean") {
    variable_name = "Mean"
    
  } else if (names(stars_object) == "sd") {
    variable_name = "Detrended Standard Deviation"
    
  } else if (names(stars_object) == "cv") {
    variable_name = "Detrended Coefficient of Variation"
    
  } else if (names(stars_object) == "slope") {
    variable_name = "Slope of Time Series"
    
  } else if (names(stars_object) == "agree") {
    variable_name = "GCM Agreement"
    
  } else if (names(stars_object) == "diff") {
    variable_name = "Absolute Mean First Differences"
    
  } else if (names(stars_object) == "diffsl") {
    variable_name = "Slope of First Differences"
    
  } else if (names(stars_object) == "change") {
    variable_name = "Change from Historical"
    
  } else if (names(stars_object) == "zscore") {
    variable_name = "Change Z-Score" }
  
}
scale1 = c("#156296", "#f5f5f5", "#8856a7") #darkblue, white, purple ##mean

scale2 = c("#e0ecf4", "#f5f5f5", "#8856a7") #? ##sum

scale3 = c("#5ab4ac", "#f5f5f5", "#7E5B11") #turquoise, white, brown ##change, sd, cv

scale4 = c("#00B5B5", "#f5f5f5", "#AC1F00") #turquoise, white, red ##diff, zscore, agree

scale5 = c("#00703B", "#f5f5f5", "#471583") #green, white, purple ##slope, diffsl


q_colors =  12 # for no particular reason
v_colors =  viridis(q_colors)
m_colors =  magma(q_colors)
p_colors =  plasma(q_colors)

v_div <- scico(21, palette = 'roma')


pick_color <- function(stars_object) {
  if (names(stars_object) == "mean") {
    colors = v_colors
  } else if (names(stars_object) == "sd") {
    colors = m_colors
  } else if (names(stars_object) == "cv") {
    colors = m_colors
  } else if (names(stars_object) == "diff") {
    colors = v_colors
  } else if (names(stars_object) == "slope") {
    colors = scale5
  } else if (names(stars_object) == "diffsl") {
    colors = scale5
  } else if (names(stars_object) == "change") {
    colors = scale3
  } else if (names(stars_object) == "agree") {
    colors = scale4
  } else if (names(stars_object) == "zscore") {
    colors = scale4
  } else if (names(stars_object) == "sum") {
    colors = i_colors
  } else {
    colors = v_colors
  }
}



pick_div_color <- function(stars_object) {
  if (names(stars_object) == "mean") {
    colors = v_div
  } else if (names(stars_object) == "sd") {
    colors = m_div
  } else if (names(stars_object) == "cv") {
    colors = m_div
  } else if (names(stars_object) == "diff") {
    colors = v_div
  } else if (names(stars_object) == "slope") {
    colors = scale5
  } else if (names(stars_object) == "diffsl") {
    colors = scale5
  } else if (names(stars_object) == "change") {
    colors = scale3
  } else if (names(stars_object) == "agree") {
    colors = scale4
  } else if (names(stars_object) == "zscore") {
    colors = scale4
  } else if (names(stars_object) == "sum") {
    colors = scale2
  } else {
    colors = v_div
  }
}


pick_scale <- function(stars_object) {
  if (names(stars_object) == "mean") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "sd") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "cv") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "diff") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "slope") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "diffsl") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "change") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "agree") {
    pb = c(0, 1)
  } else if (names(stars_object) == "zscore") {
    pb = c(0.025, 0.9975)
  } else if (names(stars_object) == "sum") {
    pb = c(0, 1)
  } else {
    pb = c(0, 1)
  }
}







#with rounding--> pick_scale() using names of variables no longer works. taking quantiles as limits (reduce outliers) for color scale
#add rounding as an option: argument--> either use rounding OR use pick scale BUT need to make pick scale work with the 3 tick colorscale
#right now only plot variables using the vars_st version of the variable so pick scale isnt triggered




#DONT DELETE
#this is the one working with hdm manuscript right now
#true as of 1.11.22
get_color_scale <- 
  function(stars_object, lim = stars_object[[1]], color.scale = v_colors, div.color.scale = v_div, ovrd.div = FALSE, probs = probs, force.div = FALSE) { 
    
    if ("TRUE" %in% grepl("agree", names(stars_object))) {
      scale_fill_gradientn(
        position = "left", 
        name = NULL,
        space = "Lab",
        na.value = "white",
        guide = "colourbar",
        aesthetics = "fill",
        colors = div.color.scale,
        oob = squish,
        
        limits = round(quantile(x = as.vector(c(7,11)),
                                probs = pick_scale(stars_object),
                                na.rm = TRUE), digits = pick_sig_digits(lim)),
        
        values = rescale(x = c(round(quantile(c(7,11), 
                                              probs = pick_scale(stars_object), 
                                              na.rm = TRUE)[1], digits = pick_sig_digits(lim)),
                               10, 
                               round(quantile(c(7,11), 
                                              probs = pick_scale(stars_object), 
                                              na.rm = TRUE)[2], digits = pick_sig_digits(lim)))),
        breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                   round(mean(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)),  
                   round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim))),
        labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                   round(mean(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                   round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)))
      )
      # print(round(quantile(x = as.vector(c(7,11)),
      #                      probs = pick_scale(stars_object),
      #                      na.rm = TRUE), digits = 2))
    } else {
      
      if (any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
                  lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] > 0, na.rm = TRUE)
          & any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
                    lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] < 0, na.rm = TRUE)
          & ovrd.div == FALSE) {
  
        scale_fill_gradientn(
          position = "left", 
          name = NULL,
          space = "Lab",
          na.value = "white",
          guide = "colourbar",
          aesthetics = "fill",
          colors = div.color.scale,
          oob = squish,
          
          limits = round(quantile(x = as.vector(lim),
                                  probs = pick_scale(stars_object),
                                  na.rm = TRUE), digits = pick_sig_digits(lim)),
          
          values = rescale(x = c(round(quantile(lim, 
                                                probs = pick_scale(stars_object), 
                                                na.rm = TRUE)[1], digits = pick_sig_digits(lim)),
                                 0, 
                                 round(quantile(lim, 
                                                probs = pick_scale(stars_object), 
                                                na.rm = TRUE)[2], digits = pick_sig_digits(lim)))),
          breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                     0, 
                     round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim))),
          labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                     0, 
                     round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)))
        )
        
      } else if (force.div == TRUE) {
        if (min(lim, na.rm = TRUE) > 0) {
         print("a")
          scale_fill_gradientn(
            position = "left", 
            name = NULL,
            space = "Lab",
            na.value = "white",
            guide = "colourbar",
            aesthetics = "fill",
            colors = div.color.scale[c(10:21)],
            #oob = squish,
            
            limits = round(quantile(x = as.vector(lim),
                                    probs = c(0,1),
                                    na.rm = TRUE), digits = pick_sig_digits(lim)),
         
            values = rescale(x = c(0, 0,
                                   round(quantile(lim, 
                                                  probs = c(0,1), 
                                                  na.rm = TRUE)[2], digits = pick_sig_digits(lim)))),
            breaks = c(round(min(lim, na.rm = TRUE), digits = pick_sig_digits(lim)),
                       round(quantile(lim,
                                      probs = c(0,1),
                                      na.rm = TRUE)[2], digits = pick_sig_digits(lim))),
            labels = c(round(min(lim, na.rm = TRUE), digits = pick_sig_digits(lim)),
                       round(quantile(lim,
                                      probs = c(0,1),
                                      na.rm = TRUE)[2], digits = pick_sig_digits(lim)))
            
          )
          
          
        } else {    
          
          scale_fill_gradientn(
            position = "left", 
            name = NULL,
            space = "Lab",
            na.value = "white",
            guide = "colourbar",
            aesthetics = "fill",
            colors = color.scale,
            oob = squish,
            
            limits = round(quantile(x = as.vector(lim),
                                    probs = pick_scale(stars_object),
                                    na.rm = TRUE), digits = pick_sig_digits(lim)),
            
            values = rescale(x = c(round(quantile(lim, 
                                                  probs = pick_scale(stars_object), 
                                                  na.rm = TRUE)[1], digits = pick_sig_digits(lim)),
                                   round(quantile(lim, 
                                                  probs = pick_scale(stars_object), 
                                                  na.rm = TRUE)[2], digits = pick_sig_digits(lim)))),
            breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                       round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = pick_sig_digits(lim)), 
                       round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim))),
            
            labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                       round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = pick_sig_digits(lim)), 
                       round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)))
          )
          
        }

      } else {
        
        scale_fill_gradientn(
          position = "left", 
          name = NULL,
          space = "Lab",
          na.value = "white",
          guide = "colourbar",
          aesthetics = "fill",
          colors = color.scale,
          oob = squish,
          
          limits = round(quantile(x = as.vector(lim),
                                  probs = pick_scale(stars_object),
                                  na.rm = TRUE), digits = pick_sig_digits(lim)),
          
          values = rescale(x = c(round(quantile(lim, 
                                                probs = pick_scale(stars_object), 
                                                na.rm = TRUE)[1], digits = pick_sig_digits(lim)),
                                 round(quantile(lim, 
                                                probs = pick_scale(stars_object), 
                                                na.rm = TRUE)[2], digits = pick_sig_digits(lim)))),
          breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                     round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = pick_sig_digits(lim)), 
                     round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim))),
        
          labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)), 
                     round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = pick_sig_digits(lim)), 
                     round(max(stars_object[[1]], na.rm = TRUE), digits = pick_sig_digits(lim)))
        )
        

    
      } 

    }

  }



plot_cor_map(cwdseas_hist[1], force.div = TRUE)

plot_cor_map(swaseas_hist[1], force.div = TRUE)


"TRUE" %in% grepl("mean", names(cwdseas_hist))

ifelse(grepl("mean", names(cwdseas_hist)))
?ifelse
get_color_scale(cwdseas_hist[1], force.div = TRUE)


rescale(x = c(round(quantile(lim, 
                             probs = c(0,1), 
                             na.rm = TRUE)[1], digits = pick_sig_digits(lim)),
              0, 
              round(quantile(lim, 
                             probs = c(0,1),
                             na.rm = TRUE)[2], digits = pick_sig_digits(lim))))

rescale(x = c(-1,0,
              round(quantile(lim, 
                             probs = c(0,1),
                             na.rm = TRUE)[2], digits = pick_sig_digits(lim))))



lim <- cwdseas_hist[[1]]

stars_object <- cwdseas_hist[1]

rescale(x = c(round(min(lim, na.rm = TRUE), digits = pick_sig_digits(lim)), 
              round(mean(lim, na.rm = TRUE), digits = pick_sig_digits(lim)),
              round(quantile(lim, 
                             probs = pick_scale(stars_object), 
                             na.rm = TRUE)[2], digits = pick_sig_digits(lim))))


c(round(min(cwdseas_hist[[1]], na.rm = TRUE), digits = pick_sig_digits(cwdseas_hist[[1]])), 
  round(mean(c(min(cwdseas_hist[[1]], na.rm = TRUE), max(cwdseas_hist[[1]], na.rm = TRUE))), digits = pick_sig_digits(cwdseas_hist[[1]])), 
  round(max(cwdseas_hist[[1]], na.rm = TRUE), digits = pick_sig_digits(cwdseas_hist[[1]])))

#Isoline for GCM agreement using tryCatch
agree_isoline <- function(stars_object, stars_agree) {
  if (names(stars_object) == "change") {
tryCatch(
  expr = {geom_sf(data = simple_mask_outline(stars_agree, alpha = 10, keep = 0.5), fill = NA, color = rgb(0,0,0, 120/255), size = 0.25)
  },
  error = function(e){ 
    geom_sf(data = NULL)
  }
)}
}


#old agree isoline function without trycatch
#agree_isoline <- function(stars_object, stars_agree) {
#  if (names(stars_object) == "change") {
#    geom_sf(data = simple_mask_outline(stars_agree, alpha = 10, keep = 0.2), fill = NA, color = "#406120", size = 1) }
#}


#Isoline function for zscore alpha level using tryCatch
alpha_isoline <- function(stars_object) {
  if (names(stars_object) == "zscore") {
    tryCatch(
      
      expr =  {if (find_stars_max(stars_object) > 2.05 & find_stars_min(stars_object > -2.05)) {
          
          geom_sf(data = simple_mask_outline(stars_object, alpha = 2.05, keep = 0.5), fill = NA, color = "#5C030D", size = 0.25)
          
      } else if (find_stars_max(stars_object) < 2.05 & find_stars_min(stars_object < -2.05)) { 
        
        geom_sf(data = simple_mask_outline(-stars_object, alpha = 2.05, keep = 0.5), fill = NA, color = "#00B5B5", size = 0.25) 
        
      } else if (find_stars_max(stars_object) > 2.05 & find_stars_min(stars_object < -2.05)) { 
        
        geom_sf(data = simple_mask_outline(stars_object, alpha = 2.05, keep = 0.5), fill = NA, color = "#5C030D", size = 0.25) +
        geom_sf(data = simple_mask_outline(-stars_object, alpha = 2.05, keep = 0.5), fill = NA, color = "#00B5B5", size = 0.25) 
      }
  },
  error = function(e){ 
    geom_sf(data = NULL)
  }
)}
}


#need something to loop over mid and end centuries if name = any of the three future variables
#still need to figure out if i can use if statements to use functions in the arguments?
plot_variables <- function(stars_object = NULL,
                          stars_agree = NULL,
                          metric_name = NULL, 
                          lim = stars_object[[1]], 
                          bound = NULL
                          ) {
    x <- ggplot() +
          geom_stars(data = stars_object) +
          scale_x_discrete(expand=c(0,0)) +
          scale_y_discrete(expand=c(0,0)) +
          geom_sf(data = states_mask, col = "black", fill = NA, size = 0.1) +
          geom_sf(data = provinces_mask, col = "black", fill = NA, size = 0.1) +
      #2.5 change to a variable that gets set before ggplot function (offsets the lower ylim of the bbox to show less white space)
          coord_sf(xlim = c(st_bbox(stars_object)[1], 
                            st_bbox(stars_object)[3]), 
                   ylim = c((st_bbox(stars_object)[2] + 3), 
                            (st_bbox(stars_object)[4] + 1))) +
          get_color_scale(stars_object, lim) +
          agree_isoline(stars_object, stars_agree) +
          alpha_isoline(stars_object) +
          theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"), 
                legend.position = "top", 
                legend.key.width = unit(2, "cm"), 
                legend.key.height = unit(0.25, "cm"), 
                legend.text = element_text(size = 8), 
                plot.title = element_text(hjust = 0.75 , size = 10), 
                axis.text = element_blank(), 
                axis.ticks = element_blank(), 
                panel.background = element_blank(), 
                panel.grid = element_blank(),
                plot.background = element_rect(color = "black", fill = NA, size = 0.5)) + 
          xlab(element_blank()) +
          ylab(element_blank()) +
          ggtitle(paste(metric_name,"\n", get_name(stars_object)))
  return(x)
}






# #backup


# get_color_scale <- 
#   function(stars_object, lim = stars_object[[1]]) {
#     
#     if (names(stars_object) == "agree") {
#       scale_fill_gradientn(
#         position = "left", 
#         name = NULL,
#         space = "Lab",
#         na.value = "white",
#         guide = "colourbar",
#         aesthetics = "fill",
#         colors = pick_color(stars_object),
#         oob = squish,
#         
#         limits = quantile(x = as.vector(c(7,11)),
#                           probs = pick_scale(stars_object),
#                           na.rm = TRUE),
#         
#         values = rescale(x = c(quantile(c(7,11), 
#                                         probs = pick_scale(stars_object), 
#                                         na.rm = TRUE)[1],
#                                10, 
#                                quantile(c(7,11), 
#                                         probs = pick_scale(stars_object), 
#                                         na.rm = TRUE)[2])))
#     } else {
#       
#       if (any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                   lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] > 0, na.rm = TRUE)
#           & any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                     lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] < 0, na.rm = TRUE)) {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = pick_color(stars_object),
#           oob = squish,
#           
#           limits = quantile(x = as.vector(lim),
#                             probs = pick_scale(stars_object),
#                             na.rm = TRUE),
#           
#           values = rescale(x = c(quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[1],
#                                  0, 
#                                  quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[2])))
#       } else {
#         scale_fill_gradient2(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           low = pick_color(stars_object)[1],
#           mid = pick_color(stars_object)[2],
#           high = pick_color(stars_object)[3],
#           oob = squish,
#           limits = quantile(x = as.vector(lim),
#                             probs = pick_scale(stars_object),
#                             na.rm = TRUE,
#                             midpoint = 0)
#         )
#       } 
#     }
#     
#   }

# get_color_scale <- function(stars_object, lim = stars_object[[1]]) {
#   
#   if (quantile(stars_object[[1]], probs = c(0.025, 0.9975), na.rm = TRUE)[1] <= 0) {
#     
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       colors = pick_color(stars_object),
#       oob = squish,
#       
#       limits = quantile(            
#           x = as.vector(lim),
#           probs = c(0.025, 0.9975),
#           na.rm = TRUE),
#       
#       values = rescale(
#           x = c(quantile(lim, 
#                          probs = c(0.025, 0.9975), 
#                          na.rm = TRUE)[1],
#                 0, 
#                 quantile(lim, 
#                          probs = c(0.025, 0.9975), 
#                          na.rm = TRUE)[2])))
#   } else {
#     
#     scale_fill_gradient2(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       low = pick_color(stars_object)[1],
#       mid = pick_color(stars_object)[2],
#       high = pick_color(stars_object)[3],
#       oob = squish,
#       limits = quantile(            
#           as.vector(lim),
#           probs = c(0.025, 0.9975),
#           na.rm = TRUE),
#       midpoint = 0
#     )
#   } 
# }
# 



# #define functions for plotting historical and future separately
# write_hist <- function(stars_object, metric_name = NULL, bound = NULL) {
#   for (i in 1:length(stars_object)) {
#     jpeg(filename = paste(names(stars_object[i]), ".jpg", sep = ''), width = 1200, height = 1400, res = 200)
#     plot_variables(stars_object[i], metric_name, bound = bound, metric_name = metric_name)
#     dev.off()
#     }
# }
# 
# 
# write_fut <- function(stars_object, metric_name = NULL, bound = NULL) {
#   for (i in 1:2) {
#     for (j in 1:length(stars_object[[i]])) {
#       jpeg(filename = paste(names(stars_object[i]), names(stars_object[[i]][j]), "jpg", sep = '.'), width = 1200, height = 1400, res = 200)
#       plot_variables(stars_object[[i]][j], stars_object[[i]][3], metric_name = metric_name, bound = bound)
#       dev.off()
#     }
#   }
# }


# 
# ############################### ~~ Functions added to ggplot ~~ #############################
# #assigns color scale and midpoint and limits
# #based on the name of the attribute being plotted
# #need to change this so that limits can be consistent across variable
# color_type <- function(stars_object, lim = NULL) { 
#   
#   if (names(stars_object) == "mean") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#8856a7",
#       #mid = "#f5f5f5",
#       #low = "#e0ecf4",
#       colors = c("#156296", "#f5f5f5", "#8856a7"),
#       values = rescale(c(quantile(lim, probs = c(0, 0.9975), na.rm = TRUE)[1],
#                          0, 
#                          quantile(lim, probs = c(0, 0.9975), na.rm = TRUE)[2])),
#       limits = quantile(lim, probs = c(0, 0.9975), na.rm = TRUE),
#       oob = squish)
#     #find_stars_max(c(stars_object[[1]][1], stars_object[[2]][1], stars_object[[3]][1]))
#     
#   } else if (names(stars_object) == "sum") {
#     scale_fill_gradient2(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       high = "#8856a7",
#       mid = "#f5f5f5",
#       low = "#e0ecf4",
#       midpoint = 0,
#       limits = quantile(as.vector(stars_object[[1]]), probs = c(0, 0.9975), na.rm = TRUE),
#       oob = squish)      
#     
#   } else if (names(stars_object) == "change") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#7E5B11",
#       #mid = "#f5f5f5",
#       #low = "#5ab4ac",
#       colors = c("#5ab4ac", "#f5f5f5", "#7E5B11"),
#       values = rescale(c(quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE)[1],
#                          0, 
#                          quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE)[2])),
#       limits = quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE),
#       oob = squish)
#     
#   } else if (names(stars_object) == "agree") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#AC1F00",
#       #mid = "#f7f7f7",
#       #low = "#00B5B5",
#       colors = c("#00B5B5", "#f7f7f7", "#AC1F00"),
#       values = rescale(c(0,9,10,11)),
#       breaks = c(0,9,10,11),
#       #midpoint = mean(c(),
#       limits = c(0,11),
#       oob = squish
#     )
#     
#   } else if (names(stars_object) == "sd") {
#     scale_fill_gradient2(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       high = "#7E5B11",
#       mid = "#f5f5f5",
#       low = "#5ab4ac",
#       midpoint = 0,
#       limits = quantile(as.vector(stars_object[[1]]), probs = c(0, 0.995), na.rm = TRUE), 
#       oob = squish)
#     
#   } else if (names(stars_object) == "cv") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#7E5B11",
#       #mid = "#f5f5f5",
#       #low = "#5ab4ac",
#       colors = c("#5ab4ac", "#f5f5f5", "#7E5B11"),
#       values = rescale(c(quantile(stars_object[[1]], probs = c(0.025, 0.9975), na.rm = TRUE)[1],
#                          0,
#                          quantile(stars_object[[1]], probs = c(0.025, 0.9975), na.rm = TRUE)[2])),
#       limits = quantile(as.vector(stars_object[[1]]), probs = c(0.025, 0.9975), na.rm = TRUE),
#       oob = squish)
#     
#   } else if (names(stars_object) == "diff") {
#     scale_fill_gradient2(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       high = "#AC1F00",
#       mid = "#f7f7f7",
#       low = "#00B5B5",
#       midpoint = 0,
#       limits = quantile(as.vector(stars_object[[1]]), probs = c(0.025, 0.9975), na.rm = TRUE),
#       oob = squish)
#     
#   } else if (names(stars_object) == "zscore") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#AC1F00",
#       #mid = "#f7f7f7",
#       #low = "#00B5B5",
#       colors= c("#00B5B5", "#f7f7f7", "#AC1F00"),
#       values = rescale(c(quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE)[1],
#                          0, 
#                          quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE)[2])),
#       limits = quantile(lim, probs = c(0.025, 0.9975), na.rm = TRUE),
#       oob = squish
#     )
#     
#   } else if (names(stars_object) == "slope" | names(stars_object) == "diffsl") {
#     scale_fill_gradientn(
#       position = "left", 
#       name = NULL,
#       space = "Lab",
#       na.value = "white",
#       guide = "colourbar",
#       aesthetics = "fill",
#       #high = "#00703B",
#       #mid = "#f5f5f5",
#       #low = "#471583",
#       colors = c("#471583", "#f5f5f5", "#00703B"),
#       values = rescale(c(quantile(stars_object[[1]], probs = c(0.025, 0.9975), na.rm = TRUE)[1],
#                          0, 
#                          quantile(stars_object[[1]], probs = c(0.025, 0.9975), na.rm = TRUE)[2])),
#       limits = quantile(as.vector(stars_object[[1]]), probs = c(0.025, 0.9975), na.rm = TRUE), 
#       oob = squish)
#   }
# }
# 


# #Quantile color scale
# ##doesnt work yet
# d = tdd_hist[2][[1]]
# 
# d = as.vector(tdd_hist[2][[1]])
# 
# d = as.vector(round(abs(10 * sapply(1:4, function(n)rnorm(20, mean=n, sd=.6)))))
# 
# ggplot(data=NULL, aes(x=1:length(d), y=d, col=cut(d,quantile(d)))) + 
#   geom_point(size=5) + scale_colour_manual(values=rainbow(5))
# 
# quantile_labels = function(d, qtls, units, round=2, minzero=F, maxplus=F, space=T){
#   labs = levels(cut(d, qtls,include.lowest=T))
#   for(i in c('[/(]', '[/[]', ']')) labs = stringr::str_replace_all(labs, i, "")
#   labs = stringr::str_split(labs, ',')
#   labs = lapply(labs, function(i) i = round(as.numeric(i), round))
#   if(minzero) labs[[1]][1] = '0'
#   labs = unlist(lapply(labs, function(i) i = paste(i, collapse='-')))
#   if(maxplus) labs[[length(labs)]] = stringr::str_replace(labs[[length(labs)]], '-[0-9]+', '+')
#   if(space) labs = paste(labs, units) else labs = paste0(labs, units)
#   labs
# }
# 
# qtls = quantile(d, probs = seq(0, 1, length.out=5), na.rm = TRUE)
# quantile_labels(d, qtls, units  = 'eggs')
# quantile_labels(d, qtls, '%', round=0, space=F)
# ?str_replace_all
# col_labs = quantile_labels(d, qtls, '%', round=0, space=F, minzero=T, maxplus=T)
# 
# ggplot(data=NULL, aes(x=1:length(d), y=d, col=cut(d,quantile(d)))) + geom_point(size=5) + 
#   scale_colour_manual(name = "Data quantile", values=rev(rainbow(4))) +
#   guides(col = guide_legend(reverse = TRUE))
# 
# ggplot() +
#   geom_stars(tdd_hist[2], aes(col = x)) +
#   scale_colour_manual(name = "Data quantile", values=rev(rainbow(4))) 
#   
# x <- cut(tdd_hist[2][[1]], quantile(tdd_hist[2][[1]], na.rm = TRUE))
# 
# #or try this instead of gradient2
# scale_fill_gradientn(
#   colours=colours,
#   values=val.remap,
#   breaks=quants,
#   guide="legend") 
# 
# 
# # 
# # 
# get_color_scale <- 
#   function(stars_object, lim = stars_object[[1]]) {
#     
#     if (names(stars_object) == "agree") {
#       scale_fill_gradientn(
#         position = "left", 
#         name = NULL,
#         space = "Lab",
#         na.value = "white",
#         guide = "colourbar",
#         aesthetics = "fill",
#         colors = pick_div_color(stars_object),
#         oob = squish,
#         
#         limits = quantile(x = as.vector(c(7,11)),
#                           probs = pick_scale(stars_object),
#                           na.rm = TRUE),
#         
#         values = rescale(x = c(quantile(c(7,11), 
#                                         probs = pick_scale(stars_object), 
#                                         na.rm = TRUE)[1],
#                                10, 
#                                quantile(c(7,11), 
#                                         probs = pick_scale(stars_object), 
#                                         na.rm = TRUE)[2])))
#     } else {
#       
#       if (any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                   lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] > 0, na.rm = TRUE)
#           & any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                     lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] < 0, na.rm = TRUE)) {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = pick_div_color(stars_object),
#           oob = squish,
#           
#           limits = quantile(x = as.vector(lim),
#                             probs = pick_scale(stars_object),
#                             na.rm = TRUE),
#           
#           values = rescale(x = c(quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[1],
#                                  0, 
#                                  quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[2])))
#       } else {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = pick_color(stars_object),
#           oob = squish,
#           
#           limits = quantile(x = as.vector(lim),
#                             probs = pick_scale(stars_object),
#                             na.rm = TRUE),
#           
#           values = rescale(x = c(quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[1],
#                                  quantile(lim, 
#                                           probs = pick_scale(stars_object), 
#                                           na.rm = TRUE)[2])))
#         
#       } 
#     }
#     
#   }
# 
# 
# 
# ##
# 
# get_color_scale <- 
#   function(stars_object, lim = stars_object[[1]]) { 
#     
#     if (names(stars_object) == "agree") {
#       scale_fill_gradientn(
#         position = "left", 
#         name = NULL,
#         space = "Lab",
#         na.value = "white",
#         guide = "colourbar",
#         aesthetics = "fill",
#         colors = pick_div_color(stars_object),
#         oob = squish,
#         
#         limits = round(quantile(x = as.vector(c(7,11)),
#                                 probs = pick_scale(stars_object),
#                                 na.rm = TRUE), digits = 2),
#         
#         values = rescale(x = c(round(quantile(c(7,11), 
#                                               probs = pick_scale(stars_object), 
#                                               na.rm = TRUE)[1], digits = 2),
#                                10, 
#                                round(quantile(c(7,11), 
#                                               probs = pick_scale(stars_object), 
#                                               na.rm = TRUE)[2], digits = 2))),
#         breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                    round(mean(stars_object[[1]], na.rm = TRUE), digits = 2),  
#                    round(max(stars_object[[1]], na.rm = TRUE), digits = 2)),
#         labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                    round(mean(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                    round(max(stars_object[[1]], na.rm = TRUE), digits = 2))
#       )
#       # print(round(quantile(x = as.vector(c(7,11)),
#       #                      probs = pick_scale(stars_object),
#       #                      na.rm = TRUE), digits = 2))
#     } else {
#       
#       if (any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                   lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] > 0, na.rm = TRUE)
#           & any(lim[lim > quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[1] &
#                     lim < quantile(lim, probs = pick_scale(stars_object), na.rm = TRUE)[2]] < 0, na.rm = TRUE)) {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = pick_div_color(stars_object),
#           oob = squish,
#           
#           limits = round(quantile(x = as.vector(lim),
#                                   probs = pick_scale(stars_object),
#                                   na.rm = TRUE), digits = 2),
#           
#           values = rescale(x = c(round(quantile(lim, 
#                                                 probs = pick_scale(stars_object), 
#                                                 na.rm = TRUE)[1], digits = 2),
#                                  0, 
#                                  round(quantile(lim, 
#                                                 probs = pick_scale(stars_object), 
#                                                 na.rm = TRUE)[2], digits = 2))),
#           breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                      0, 
#                      round(max(stars_object[[1]], na.rm = TRUE), digits = 2)),
#           labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                      0, 
#                      round(max(stars_object[[1]], na.rm = TRUE), digits = 2))
#         )
#         # print(round(quantile(x = as.vector(lim),
#         #                      probs = pick_scale(stars_object),
#         #                      na.rm = TRUE), digits = 2))
#       } else {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = pick_color(stars_object),
#           oob = squish,
#           
#           limits = round(quantile(x = as.vector(lim),
#                                   probs = pick_scale(stars_object),
#                                   na.rm = TRUE), digits = 2),
#           
#           values = rescale(x = c(round(quantile(lim, 
#                                                 probs = pick_scale(stars_object), 
#                                                 na.rm = TRUE)[1], digits = 2),
#                                  round(quantile(lim, 
#                                                 probs = pick_scale(stars_object), 
#                                                 na.rm = TRUE)[2], digits = 2))),
#           breaks = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                      round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = 2), 
#                      round(max(stars_object[[1]], na.rm = TRUE), digits = 2)),
#           labels = c(round(min(stars_object[[1]], na.rm = TRUE), digits = 2), 
#                      round(mean(c(min(stars_object[[1]], na.rm = TRUE), max(stars_object[[1]], na.rm = TRUE))), digits = 2), 
#                      round(max(stars_object[[1]], na.rm = TRUE), digits = 2))
#         )
#         # print(round(quantile(x = as.vector(lim),
#         #                      probs = pick_scale(stars_object),
#         #                      na.rm = TRUE), digits = 2))
#         
#       } 
#     }
#     
#   }
# ##
# # 
# 
# #changing limits for the cv collective limits
# 
# get_color_scale <- 
#   function(stars_object, lim = stars_object[[1]], color.scale = v_colors, div.color.scale = v_div) { 
#     
#     if (names(stars_object) == "agree") {
#       scale_fill_gradientn(
#         position = "left", 
#         name = NULL,
#         space = "Lab",
#         na.value = "white",
#         guide = "colourbar",
#         aesthetics = "fill",
#         colors = div.color.scale,
#         oob = squish,
#         
#         limits = round(quantile(x = as.vector(c(7,11)),
#                                 probs = c(0,1),
#                                 na.rm = TRUE), digits = 1),
#         
#         values = rescale(x = c(round(quantile(c(7,11), 
#                                               probs = c(0,1), 
#                                               na.rm = TRUE)[1], digits = 1),
#                                10, 
#                                round(quantile(c(7,11), 
#                                               probs = c(0,1), 
#                                               na.rm = TRUE)[2], digits = 1))),
#         breaks = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                    round(mean(lim, na.rm = TRUE), digits = 1),  
#                    round(max(lim, na.rm = TRUE), digits = 1)),
#         labels = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                    round(mean(lim, na.rm = TRUE), digits = 1), 
#                    round(max(lim, na.rm = TRUE), digits = 1))
#       )
#       # print(round(quantile(x = as.vector(c(7,11)),
#       #                      probs = pick_scale(stars_object),
#       #                      na.rm = TRUE), digits = 2))
#     } else {
#       
#       if (any(lim[lim > quantile(lim, probs = c(0,1), na.rm = TRUE)[1] &
#                   lim < quantile(lim, probs = c(0,1), na.rm = TRUE)[2]] > 0, na.rm = TRUE)
#           & any(lim[lim > quantile(lim, probs = c(0,1), na.rm = TRUE)[1] &
#                     lim < quantile(lim, probs = c(0,1), na.rm = TRUE)[2]] < 0, na.rm = TRUE)) {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = div.color.scale,
#           oob = squish,
#           
#           limits = round(quantile(x = as.vector(lim),
#                                   probs = c(0,1),
#                                   na.rm = TRUE), digits = 1),
#           
#           values = rescale(x = c(round(quantile(lim, 
#                                                 probs = c(0,1), 
#                                                 na.rm = TRUE)[1], digits = 1),
#                                  0, 
#                                  round(quantile(lim, 
#                                                 probs = c(0,1), 
#                                                 na.rm = TRUE)[2], digits = 1))),
#           breaks = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                      0, 
#                      round(max(lim, na.rm = TRUE), digits = 1)),
#           labels = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                      0, 
#                      round(max(lim, na.rm = TRUE), digits = 1))
#         )
#         # print(round(quantile(x = as.vector(lim),
#         #                      probs = pick_scale(stars_object),
#         #                      na.rm = TRUE), digits = 2))
#       } else {
#         
#         scale_fill_gradientn(
#           position = "left", 
#           name = NULL,
#           space = "Lab",
#           na.value = "white",
#           guide = "colourbar",
#           aesthetics = "fill",
#           colors = color.scale,
#           oob = squish,
#           
#           limits = round(quantile(x = as.vector(lim),
#                                   probs = c(0,1),
#                                   na.rm = TRUE), digits = 1),
#           
#           values = rescale(x = c(round(quantile(lim, 
#                                                 probs = c(0,1), 
#                                                 na.rm = TRUE)[1], digits = 1),
#                                  round(quantile(lim, 
#                                                 probs = c(0,1), 
#                                                 na.rm = TRUE)[2], digits = 1))),
#           breaks = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                      round(mean(c(lim, na.rm = TRUE), max(lim, na.rm = TRUE))), digits = 1), 
#           round(max(lim, na.rm = TRUE), digits = 1),
#           labels = c(round(min(lim, na.rm = TRUE), digits = 1), 
#                      round(mean(c(min(lim, na.rm = TRUE), max(lim, na.rm = TRUE))), digits = 1), 
#                      round(max(lim, na.rm = TRUE), digits = 1))
#         )
#         # print(round(quantile(x = as.vector(lim),
#         #                      probs = pick_scale(stars_object),
#         #                      na.rm = TRUE), digits = 2))
#         
#       } 
#     }
#     
#   }


