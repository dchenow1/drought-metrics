# START HERE --> 
# ~~~ Functions for individual maps ~~ 
#(eventually will change the code in "map_netcdf_functions" so that it can be used
#for the regular figures and correlation matrix)

# Function for plotting maps in the correlation matrix 
plot_cor_map <- function(stars_object = NULL,
                         div.color.scale = v_div,
                         color.scale = v_colors,
                         stars_agree = NULL,
                         metric_name = NULL, 
                         lim = stars_object[[1]], 
                         bound = NULL,
                         ovrd.div = FALSE,
                         probs = probs,
                         force.div = FALSE) {

  x <- ggplot() +
    geom_stars(data = stars_object) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    geom_sf(data = states_mask, col = "black", fill = NA, size = 0.25) +
    geom_sf(data = provinces_mask, col = "black", fill = NA, size = 0.25) +
    #2.5 change to a variable that gets set before ggplot function (offsets the lower ylim of the bbox to show less white space)
    coord_sf(xlim = c((st_bbox(stars_object)[1] + 1), 
                      (st_bbox(stars_object)[3] - 1)), 
             ylim = c((st_bbox(stars_object)[2] + 3), 
                      (st_bbox(stars_object)[4] + 1))) +
    get_color_scale(stars_object, lim, color.scale, div.color.scale, ovrd.div = ovrd.div, force.div = force.div, probs = probs) +
    #agree_isoline(stars_object, stars_agree) +
    #alpha_isoline(stars_object) +
    theme(aspect.ratio = 1,
          plot.margin = unit(c(0, 0, 0, 0), "cm"), 
          legend.justification = c(0,1),
          legend.text.align = 0.5,
          legend.position = c(0.05,0.1),
          legend.text = element_text(size = 30),
          legend.key.height = unit(60/.pt, "points"),
          legend.margin = margin(0,0,0.2,0.2, "mm"),
          legend.direction = "horizontal",
          #legend.position = "top", 
          legend.key.width = unit(1.4, "cm"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank(), 
          panel.grid = element_blank(),
          plot.background = element_rect(color = "black", fill = NA, size = 0.5)) +
    xlab(element_blank()) +
    ylab(element_blank()) 
  #ggtitle(paste(names(stars_object))) 
  
  return(x)
}



# Function to plot correlations with options to add or remove point colors, regression line and one to one (expected regression) line
plot_corr_color <- function(varlist.y, varlist.x, new.tmp = NULL, namex = NULL, namey = NULL, color.residuals = TRUE, add.regression.line = TRUE, add.onetoone = FALSE) {
  
  model <- lm(as.vector(varlist.y) ~ as.vector(varlist.x), na.action = na.exclude)
  res <-  lm(as.vector(varlist.y) ~ as.vector(varlist.x), na.action = na.exclude)  %>% residuals()
  cor <-  cor.test(y = as.vector(varlist.y), x = as.vector(varlist.x), na.action = na.exclude)[[4]][[1]] %>% round(digits = 2)
  
  if (add.onetoone == TRUE) {
    if (model[[1]][[2]] > 0) {
      #if slope is > 0 then force a zero intercept and set the slope == 1
      one.intercept <- 0
      one.slope <- 1
    } else {
      #if the slope is < 0 then force the x intercept to 0 and save the y-intercept from the regression, set slope == -1
      one.intercept <- model[[1]][[1]]
      one.slope <- -1
    }
  } else if (add.onetoone == FALSE) {
    one.intercept <- NULL
    one.slope <- NULL
  }
  
  if (add.regression.line == TRUE) {
    reg.intercept <- model[[1]][[1]]
    reg.slope <- model[[1]][[2]] 
   } else if (add.regression.line == FALSE) {
     reg.intercept <- NULL
     reg.slope <- NULL
   }

  if (color.residuals == TRUE) {
    color.option = res
  } else if (color.residuals == FALSE) {
    color.option = NULL
  }
  
  x <- ggplot() +
    geom_point(mapping = aes(y = (varlist.y), x = (varlist.x), color = color.option), size = 1.2, stroke = 0, alpha = 0.8) +
    geom_abline(intercept = reg.intercept, slope = reg.slope, color = "red") + #regression line
    geom_abline(intercept = one.intercept, slope = one.slope, color = "gray20", linetype = "dashed", size = 1.75) + #one to one with magnitude of regression
    xlab(label = namex) +
    ylab(label = namey) +
    xlab(label = "") +
    ylab(label = "") +
    scale_y_continuous(labels = label_number(accuracy = NULL), expand = c(0,0)) +
    scale_x_continuous(labels = label_number(accuracy = NULL), expand = c(0,0)) +
    annotate(geom = "text", 
             x  = round(max(varlist.x, na.rm = TRUE), digits = pick_sig_digits(varlist.x)) - ((round(max(varlist.x, na.rm = TRUE), digits = pick_sig_digits(varlist.x)) - round(min(varlist.x, na.rm = TRUE), digits = pick_sig_digits(varlist.x)))* 0.90), 
             y = round(max(varlist.y, na.rm = TRUE), digits = pick_sig_digits(varlist.y)) - ((round(max(varlist.y, na.rm = TRUE), digits = pick_sig_digits(varlist.y)) - round(min(varlist.y, na.rm = TRUE), digits = pick_sig_digits(varlist.y)))*0.15),
             #y = round(max(varlist.y, na.rm = TRUE), digits = pick_sig_digits(varlist.y)) -  round(min(varlist.y, na.rm = TRUE), digits = pick_sig_digits(varlist.y))*0.7,
              label = paste("r = ",cor, sep = ''), size = 16, color = "darkred", hjust = 0, vjust = 0) +
    theme(
      #aspect.ratio = 4/3,
          axis.text = element_text(size = 26),
          axis.line = element_line(),
          plot.background = element_rect(fill = "white", color = "black"),
          #axis.title = element_blank(),
         plot.margin = unit(c(0.2,0,2.2,0/.pt), "cm"),
         panel.background = element_blank(), 
         legend.justification = c(0,1),
         legend.text.align = 0.5,
         legend.position = unit(c(-12,-15), "mm"),
         legend.key.width = unit(1.4, "cm"),
         legend.text = element_text(size = 30),
         legend.direction = "horizontal",
         legend.key.height = unit(60/.pt, "points"),
         legend.margin = margin(0,0,0.2,0.2, "mm")
         ) +
    #theme(panel.background = element_rect(fill = "gray95")) +
    scale_color_gradientn(
      #na.value = rgb(1,1,1,0),
      #position = "bottom", 
      name = NULL,
      space = "Lab",
      guide = "colourbar",
      aesthetics = "color",
      colors = v_div,
      oob = squish,
      limits = c(round(min(res, na.rm = TRUE), digits = pick_sig_digits(res)),round(max(res, na.rm = TRUE), digits = pick_sig_digits(res))),
      values = rescale(x = c(round(min(res, na.rm = TRUE), digits = pick_sig_digits(res)), 0, round(max(res, na.rm = TRUE), digits = pick_sig_digits(res)))),
      breaks = c(round(min(res, na.rm = TRUE), digits = pick_sig_digits(res)), round(mean(res, na.rm = TRUE), digits = pick_sig_digits(res)), round(max(res, na.rm = TRUE), digits = pick_sig_digits(res))),
      labels = c(round(min(res, na.rm = TRUE), digits = pick_sig_digits(res)), round(mean(res, na.rm = TRUE), digits = pick_sig_digits(res)), round(max(res, na.rm = TRUE), digits = pick_sig_digits(res))))
  return(x)
}


 
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


  

# ~~ Functions to get lists of maps for the full layouts (correlation matrices) ~~
# Function to get maps that go in the diagonal of the matrix (map of metric mean)
get_diagonal_maps <- function(vars_st, ovrd.div = FALSE, force.div = FALSE) {
  
  diagonal <- rep(list(NA), length(vars_st))
  n = 1
  for (i in 1:length(vars_st)) {
    if (grepl("MAT", names(vars_st[i])) == TRUE) {
      diagonal[[n]] <- plot_cor_map(vars_st[i], div.color.scale = v_div, ovrd.div = TRUE)
      n = n + 1
    } else {
      diagonal[[n]] <- plot_cor_map(vars_st[i], div.color.scale = v_div, force.div = force.div)
      n = n + 1
    }
  }
  return(diagonal)
}





# get_diagonal_maps <- function(vars_st) {
#   
#   diagonal <- rep(list(NA), length(vars_st))
#   
#   for (i in 1:length(vars_st)) {
#     diagonal[[i]] <- plot_cor_map(vars_st[i])
#   }
#   return(diagonal)
# }

#Function to get scatterplots
get_scatters <- function(vars_st, seasonality = FALSE, color.residuals = TRUE, add.regression.line = TRUE, add.onetoone = FALSE) {
  len = 0
  
  for (i in 1:(length(vars_st)-1)) {
    len <- len + i
  } 
  
 # tmp <- data.frame(vars_st)
  tmp <- (data.frame(vars_st))
  
  
  # res <- get_residuals(vars_st)
  # res.df <- data.frame(res)
  # res.df <- res.df[ ,3:(length(res)+2)]
  # tmp <- cbind(tmp,res.df)
  
  offset_dimensions = 2
  
  tmp.list <- rep(list(NA), len)
  
  #new.tmp <-  get_regression(vars_st)
  
  n = 1
  m = 1

  
  for (i in 2:(length(vars_st))) {
    for (j in 1:m) {
      if (j == length(vars_st)) break;
      
      print(paste("vars = ", i, j))
      
      if (i != j) {
        if (color.residuals == FALSE) {
          
        tmp.list[[n]] <- plot_corr_color(varlist.y = tmp[[i + offset_dimensions]], varlist.x = tmp[[j + offset_dimensions]], #new.tmp = new.tmp[n,],
                                   namey = names(tmp[i + offset_dimensions]), namex = names(tmp[j + offset_dimensions]), color.residuals = FALSE, add.regression.line = TRUE, add.onetoone = FALSE)
        } else {
          #get data frame for residuals and variables? or change to only the residuals?
          #tmp <- (data.frame(vars_st))
          #tmp <- na.omit(data.frame(vars_st))
          # res <- get_residuals(vars_st)
          # res.df <- (data.frame(res))
          # res.df <- res.df[ ,c(3:length(res))]
          # tmp <- na.omit(cbind(tmp,res.df))

          tmp.list[[n]] <- plot_corr_color(varlist.y = tmp[[i + offset_dimensions]], varlist.x = tmp[[j + offset_dimensions]], #new.tmp = new.tmp[n,],
                                     namey = names(tmp[i + offset_dimensions]), namex = names(tmp[j + offset_dimensions]), color.residuals = TRUE, add.regression.line = TRUE, add.onetoone = TRUE)
        }
        print(n)
        print(tmp.list[[n]])
        n = n + 1
        m = m + 1

      }
      
    }
  }
  return(tmp.list)
}



get_residuals <- function(vars_st) {
  tmp <- (data.frame(vars_st))
  new.tmp <- (data.frame(vars_st))
  
  #offset for dimensions (lon lat) as columns in the new data frame
  offset_dimensions = 2
  
  n = 1 + offset_dimensions
  m = 1
  
  for (i in 1:(length(vars_st)-1)) {
    m = m + 1
    
    for (j in (m):length(vars_st)) {
      #if (j == length(vars_st) + 1) break;
      print(paste("i =", i))
      print(paste("j =", j))
      
      if (i != j) {
        new.tmp[,n] <- lm((as.vector(tmp[,j + offset_dimensions])) ~ (as.vector(tmp[,i + offset_dimensions])), na.action = na.exclude) %>% residuals()
        #print(paste(colnames(tmp)[i + offset_dimensions], colnames(tmp)[j + offset_dimensions], sep = "."))
        colnames(new.tmp)[n] <- paste(colnames(tmp)[j + offset_dimensions], colnames(tmp)[i + offset_dimensions], sep = ".")
        print(colnames(new.tmp))
        n = n + 1
        
      }
    }
  }
  
  cors.tmp <- st_as_stars(new.tmp, dims = c("lon", "lat"))
  
  return(cors.tmp)
}





get_resid_coef <- function(vars_st) {
  tmp <- (data.frame(vars_st))
  res.tmp <- data.frame("comb" = rep(NA, 10), "r2" = NA, "cor" = NA)
  
  #offset for dimensions (lon lat) as columns in the new data frame
  offset_dimensions = 2
  
  n = 1
  m = 1
  
  for (i in 1:(length(vars_st)-1)) {
    m = m + 1
    
    for (j in (m):length(vars_st)) {
      #if (j == length(vars_st) + 1) break;
      print(paste("i =", i))
      print(paste("j =", j))
      
      if (i != j) {
        mod  <- lm((as.vector(tmp[,j + offset_dimensions])) ~ (as.vector(tmp[,i + offset_dimensions])), na.action = na.exclude)
        res.tmp[n,2] <- summary(mod)[8]
        cr <- cor.test(x = (as.vector(tmp[,i + offset_dimensions])), y = (as.vector(tmp[,j + offset_dimensions])), na.action = na.exclude)
        res.tmp[n,3] <- cr$estimate 
        res.tmp[n,1] <- paste(colnames(tmp)[j + offset_dimensions], colnames(tmp)[i + offset_dimensions], sep = ".")
        
        
      
        n = n + 1
        
      }
    }
  }
  
  return(res.tmp)
}




#Function to get a list of the maps of the residuals from every metric combination 
#that fills the upper left of the matrix
get_residuals_maps <- function(vars_st) {
  
  len = 0
  
  for (i in 1:(length(vars_st)-1)) {
    len <- len + i
  } 
  
  cors.tmp <- get_residuals(vars_st)
  
  tmp.list <- rep(list(NA), len)
  
  n = 1
  
  for (i in 1:(length(vars_st)-1)) {
    
    for (j in (i + 1):(length(vars_st))) {
      
      if (i != j) {
        
        tmp.list[[n]] <- plot_cor_map(cors.tmp[n])
        
        n = n + 1
      }
    }
  }
  
  return(tmp.list)
}



#add arguments for color and regression into make grobs list function

#Function that adds the three types of plots to a list that can be used for grid.arrange 
make_grobs_list <- function(vars, add.onetoone = FALSE, ovrd.div = FALSE, force.div = FALSE) {
  
  #Create "vars_st" object from list of metrics in a category
  #vars_st is input for the following functions that make plots for the correlation matrix
  vars_st <- vars[[1]][1] 
  names(vars_st) <- names(vars[1])
  
  for (i in 2:length(vars)) { 
    
    new_value <- vars[[i]][1]
    names(new_value) <- names(vars[i])
    vars_st <- c(vars_st, new_value)  
    
  }
  
  dia <- get_diagonal_maps(vars_st, force.div = force.div, ovrd.div = ovrd.div)
  sca <- get_scatters(vars_st, add.onetoone = add.onetoone) # bottom left
  resid <- get_residuals_maps(vars_st) # top right
  
  length.diagonal <- length(dia)
  length.scatter <- length(sca)
  length.resid <- length(resid)
  
  
  
  # new empty 1 dimensional list representing a matrix
  newlist <- rep(list(NA), length.diagonal^2)
  
  for (i in 1:length(newlist)) {
    row = ceiling(i / length.diagonal)
    col = i %% length.diagonal
    if (col == 0)
      col = length.diagonal
    
    # print(paste("i=", i, " row=", row, " col=", col))
    
    # print(paste("sum", length.diagonal - row + 1))
    if (row == col) {
      # set diagonal
      # print("diagonal")
      newlist[[i]] = dia[[row]]
    } else if (col > row) {
      # set top right triangle values
      # print("resid")
      resid_index = i - (row^2 + row)/2
      # print((row^2 + row)/2)
      newlist[[i]] = resid[[ resid_index ]]
    } else {
      # set bottom left triangle
      # print("sca")
      next_item = sca[[1]]
      sca[[1]] = NULL
      #print(sca_index)
      newlist[[i]] = next_item
    }
    
  }
  return(newlist)
}


# 4 + 3 + 2 + 1

#     1  2  3  4  5     pattern (row)

# 1   D  1  2  3  4    -1  -1
# 2   x  D  5  6  7    -3  -2
# 3   x  x  D  8  9    -6  -3
# 4   x  x  x  D  10   -10 -4
# 5   x  x  x  x  D   


#     1  2  3  4  5     pattern

# 1   D  x  x  x  x     0   
# 2   1  D  x  x  x    -5  -5     6-5=1
# 3   2  3  D  x  x    -9  -4     11-9 = 2   12-9 = 3
# 4   4  5  6  D  x    -12 -3     16-12 = 4  17-12=5   18-12=6
# 5   7  8  9  10 D    -14 -2     21-14 = 7  22-14=8   23-14=9  24-14=10

# r = 1  1  1  1  1  2  2  2  2  2  3  3  3  3  3  4  4  4  4  4  5  5  5  5  5
# c = 1  2  3  4  5  1  2  3  4  5  1  2  3  4  5  1  2  3  4  5  1  2  3  4  5 
# i = 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#     D  x  x  x  x  1  D  x  x  x  2  5  D  x  x  3  6  8  D  x  4  7  9  10 D










title.theme <- ggplot() +
  theme(panel.background = element_rect(fill = "gray88", color = NULL, size = 3),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"),
        axis.ticks = element_blank()) 




#overall temperature conditions
vars <- list(tempy_hist,
             tdd_hist,
             tddld_hist,
             last_hist,
             first_hist)
names(vars) <- list("MAT",
                    "TDD",
                    "TDDLD",
                    "LAST",
                    "FIRST")
# vars.titles <- c("Mean Annual\nTemperature (°C)",
#                  "Total Growing\nDegree Days (dd)",
#                  "Warm Season\nLength (days)",
#                  "Last Frost\n(day of year)",
#                  "First Frost\n(day of year)")
vars.titles <- c("Mean Annual\nTemperature (°C)",
                 "Total Growing\nDegree Days (dd)",
                 "Warm Season\nLength (days)",
                 "Last Frost\n(day of year)",
                 "First Frost\n(day of year)")
newgrobs <- make_grobs_list(vars)

#5 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18)
z.e <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("overall.temperature.matrix.jpg", width = 3150, height = 3150)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,            z.e,        
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]], newgrobs[[5]],
                          z.2, newgrobs[[6]], newgrobs[[7]], newgrobs[[8]], newgrobs[[9]], newgrobs[[10]],
                          z.3, newgrobs[[11]], newgrobs[[12]], newgrobs[[13]], newgrobs[[14]], newgrobs[[15]],
                          z.4, newgrobs[[16]], newgrobs[[17]], newgrobs[[18]], newgrobs[[19]], newgrobs[[20]],
                          z.5, newgrobs[[21]], newgrobs[[22]], newgrobs[[23]], newgrobs[[24]], newgrobs[[25]]),
             nrows = 6,
             widths = c(150,600,600,600,600,600),
             heights = c(150,600,600,600,600,600)
)
dev.off()





#overall moisture conditions
vars <- list(precy_hist,
             wdd_hist,
             ddd_hist,
             swa_hist,
             cwd_hist)
names(vars) <- list("MAP",
                    "WDD",
                    "DDD",
                    "SWA",
                    "CWD")
vars.titles <- c("Mean Annual\nPrecipitation (mm)",
                 "Wet Degree Days (dd)",
                 "Dry Degree Days (dd)",
                 "Soil Water\nAvailability (mm)",
                 "Climatic Water\nDeficit (mm)")
newgrobs <- make_grobs_list(vars)

#5 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 14, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 14, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 14, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 14, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 14, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 14)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 14)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 14)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 14)
z.e <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 14)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("overall.moisture.matrix.jpg", width = 3150, height = 3150)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,            z.e,        
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]], newgrobs[[5]],
                          z.2, newgrobs[[6]], newgrobs[[7]], newgrobs[[8]], newgrobs[[9]], newgrobs[[10]],
                          z.3, newgrobs[[11]], newgrobs[[12]], newgrobs[[13]], newgrobs[[14]], newgrobs[[15]],
                          z.4, newgrobs[[16]], newgrobs[[17]], newgrobs[[18]], newgrobs[[19]], newgrobs[[20]],
                          z.5, newgrobs[[21]], newgrobs[[22]], newgrobs[[23]], newgrobs[[24]], newgrobs[[25]]),
             nrows = 6,
             widths = c(150,600,600,600,600,600),
             heights = c(150,600,600,600,600,600)
)
dev.off()






#seasonal variability
vars <- list(tddseasvar_hist,
             swaseasvar_hist,
             cwdseasvar_hist)
names(vars) <- list("TDDseasvar",
                    "SWAseasvar",
                    "CWDseasvar")
vars.titles <- c("TDD Seasonal\nVariability",
                 "SWA Seasonal\nVariability",
                 "CWD Seasonal\nVariability")

newgrobs <- make_grobs_list(vars)
#3 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("seasonal.variability.matrix.jpg", width = 1950, height = 1950)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,                           
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]],
                          z.2, newgrobs[[4]], newgrobs[[5]], newgrobs[[6]],
                          z.3, newgrobs[[7]], newgrobs[[8]], newgrobs[[9]]),
             nrows = 4,
             widths = c(150,600,600,600),
             heights = c(150,600,600,600)
)
dev.off()







#extreme hot dry conditions
vars <- list(tempy_hist,
             cwdmx_hist,
             dddsp_hist,
             dsi_hist,
             dsic_hist,
             precy_hist)
names(vars) <- list("MAT",
                    "CWD10mx",
                    "DDDsp",
                    "DSI",
                    "DSIc",
                    "MAP")
vars.titles <- c("Mean Annual\nTemperature (°C)",
                 "Climatic Water Deficit\n10-Day Max (mm)",
                 "Dry Degree Days\nLongest Spell (dd)",
                 "Dry Soil Intervals (days)",
                 "Number of\nDry Soil Intervals",
                 "Mean Annual\nPrecipitation (mm)")
newgrobs <- make_grobs_list(vars)

#6 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 90)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18)
z.e <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18)
z.f <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("extreme.hot-dry.conditions.jpg", width = 3750, height = 3750)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,            z.e,          z.f,
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]], newgrobs[[5]], newgrobs[[6]],
                          z.2, newgrobs[[7]], newgrobs[[8]], newgrobs[[9]], newgrobs[[10]], newgrobs[[11]], newgrobs[[12]],
                          z.3, newgrobs[[13]], newgrobs[[14]], newgrobs[[15]], newgrobs[[16]], newgrobs[[17]], newgrobs[[18]],
                          z.4, newgrobs[[19]], newgrobs[[20]], newgrobs[[21]], newgrobs[[22]], newgrobs[[23]], newgrobs[[24]],
                          z.5, newgrobs[[25]], newgrobs[[26]], newgrobs[[27]], newgrobs[[28]], newgrobs[[29]], newgrobs[[30]],
                          z.6, newgrobs[[31]], newgrobs[[32]], newgrobs[[33]], newgrobs[[34]], newgrobs[[35]], newgrobs[[36]]),
             nrows = 7,
             widths = c(150,600,600,600,600,600,600),
             heights = c(150,600,600,600,600,600,600)
)
dev.off()





#recruitment
vars <- list(sprec_hist,
             sprecd_hist,
             frec_hist,
             frecd_hist,
             sprecspl_hist,
             frecsl_hist)
names(vars) <- list("SPREC",
                    "SPRECd",
                    "FREC",
                    "FRECd",
                    "SPRCspl",
                    "FRECspl")
vars.titles <- c("Spring Recruitment\nIndex (dd)",
                 "Timing of Spring\nRecruitment (day of year)",
                 "Fall Recruitment\nIndex (dd)",
                 "Timing of Fall\nRecruitment (day of year)",
                 "Spell Length\nSpring Recruitment",
                 "Spell Length\nFall Recruitment")
newgrobs <- make_grobs_list(vars)

#6 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 90)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18)
z.e <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18)
z.f <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("recruitment.jpg", width = 3750, height = 3750)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,            z.e,          z.f,
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]], newgrobs[[5]], newgrobs[[6]],
                          z.2, newgrobs[[7]], newgrobs[[8]], newgrobs[[9]], newgrobs[[10]], newgrobs[[11]], newgrobs[[12]],
                          z.3, newgrobs[[13]], newgrobs[[14]], newgrobs[[15]], newgrobs[[16]], newgrobs[[17]], newgrobs[[18]],
                          z.4, newgrobs[[19]], newgrobs[[20]], newgrobs[[21]], newgrobs[[22]], newgrobs[[23]], newgrobs[[24]],
                          z.5, newgrobs[[25]], newgrobs[[26]], newgrobs[[27]], newgrobs[[28]], newgrobs[[29]], newgrobs[[30]],
                          z.6, newgrobs[[31]], newgrobs[[32]], newgrobs[[33]], newgrobs[[34]], newgrobs[[35]], newgrobs[[36]]),
             nrows = 7,
             widths = c(150,600,600,600,600,600,600),
             heights = c(150,600,600,600,600,600,600)
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
vars.titles <- c("Precipitation\nSeasonal Timing",
                 "WDD Seasonal Timing",
                 "SWA Seasonal Timing",
                 "CWD Seasonal Timing")
newgrobs <- make_grobs_list(vars, add.onetoone = TRUE, force.div = TRUE)


#4 variables

z <- ggplot() + theme_void()

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 90)

z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("seasonality.jpg", width = 2550, height = 2550)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,              
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]],
                          z.2, newgrobs[[5]], newgrobs[[6]], newgrobs[[7]], newgrobs[[8]],
                          z.3, newgrobs[[9]], newgrobs[[10]], newgrobs[[11]], newgrobs[[12]],
                          z.4, newgrobs[[13]], newgrobs[[14]], newgrobs[[15]], newgrobs[[16]]),
             nrows = 5,
             widths = c(150,600,600,600,600),
             heights = c(150,600,600,600,600)
)
dev.off()









#overall moisture conditions with temperature
vars <- list(tempy_hist,
             wdd_hist,
             ddd_hist,
             swa_hist,
             cwd_hist,
             precy_hist)
names(vars) <- list("MAT",
                    "WDD",
                    "DDD",
                    "SWA",
                    "CWD",
                    "MAP")
vars.titles <- c("MAT (°C)",
                 "WDD (dd)",
                 "DDD (dd)",
                 "SWA (mm)",
                 "CWD (mm)",
                 "MAP (mm)")
newgrobs <- make_grobs_list(vars)

#5 variables
z <- ggplot() + theme_void()
z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 90)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 90)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 90)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 90)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 90)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18, angle = 90)
z.a <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18)
z.b <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18)
z.c <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18)
z.d <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18)
z.e <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18)
z.f <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18)

setwd("/Users/dchenoweth/Dropbox/drought_metrics/droughtmetrics_source/correlation_matrix_figures")
jpeg("overall.moisture.wtemp.jpg", width = 3750, height = 3750)
grid.arrange(grobs = list(z,   z.a,           z.b,           z.c,           z.d,            z.e,        z.f,     
                          z.1, newgrobs[[1]], newgrobs[[2]], newgrobs[[3]], newgrobs[[4]], newgrobs[[5]], newgrobs[[6]],
                          z.2, newgrobs[[7]], newgrobs[[8]], newgrobs[[9]], newgrobs[[10]], newgrobs[[11]], newgrobs[[12]],
                          z.3, newgrobs[[13]], newgrobs[[14]], newgrobs[[15]], newgrobs[[16]], newgrobs[[17]], newgrobs[[18]],
                          z.4, newgrobs[[19]], newgrobs[[20]], newgrobs[[21]], newgrobs[[22]], newgrobs[[23]], newgrobs[[24]],
                          z.5, newgrobs[[25]], newgrobs[[26]], newgrobs[[27]], newgrobs[[28]], newgrobs[[29]], newgrobs[[30]],
                          z.6, newgrobs[[31]], newgrobs[[32]], newgrobs[[33]], newgrobs[[34]], newgrobs[[35]], newgrobs[[36]]),
             nrows = 7,
             widths = c(150,600,600,600,600,600,600),
             heights = c(150,600,600,600,600,600,600)
)
dev.off()







# plot_corr(varlist.y = tmp[[1 + offset_dimensions]], varlist.x = tmp[[2 + offset_dimensions]], new.tmp = new.tmp[1,],
#           namey = names(tmp[1 + offset_dimensions]), namex = names(tmp[2 + offset_dimensions]), residuals.df = tmp[,6])

















