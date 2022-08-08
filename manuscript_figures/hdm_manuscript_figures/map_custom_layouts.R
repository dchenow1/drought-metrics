
#set title theme for title/layout empty cells
title.theme <- ggplot() +
  theme(panel.background = element_rect(fill = "gray88", color = NULL, size = 3),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"),
        axis.ticks = element_blank()) 




setwd("manuscript_figures/custom_figures")


#overall conditions
vars <- list(tempy_hist[1],
             precy_hist[1],
             ddd_hist[1],
             wdd_hist[1],
             cwd_hist[1],
             swa_hist[1]
             )
names(vars) <- list("tempy",
                    "precy",
                    "ddd",
                    "wdd",
                    "cwd",
                    "swa")
vars.titles <- c("Mean Annual Temperature (°C)",
                 "Mean Annual Precipitation (mm)",
                 "Dry Degree Days (dd)",
                 "Wet Degree Days (dd)",
                 "Climatic Water Deficit (mm)",
                 "Soil Water Availability (mm)"
                 )

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

p <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("tempy", names(vars_st[i])) == TRUE) {
  p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
  n = n + 1
  } else {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

z <- ggplot() + theme_void()

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12)
             )

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 0)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 0)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18, angle = 0)

jpeg("overall.conditions.jpg", width = 1600, height = 2850)
grid.arrange(grobs = list(z.1,z.2,
                          p[[1]],p[[2]], 
                          z.3,z.4,
                          p[[3]],p[[4]],
                          z.5,z.6,
                          p[[5]],p[[6]]),
             nrows = 6,
             layout_matrix = lay,
             widths = c(800,800),
             heights = c(150,
                         800,
                         150,
                         800,
                         150,
                         800)
)
dev.off()






#moisture variability
vars <- list(swaseasvar_hist[1],
             cwdseasvar_hist[1],
             swa_hist[3],
             cwd_hist[3]
             )
names(vars) <- list("swaseasvar",
                    "cwdseasvar",
                    "swa.cv",
                    "cwd.cv"
                    )
vars.titles <- c("Soil Water Availability\nWithin-Year Seasonal Variability",
                 "Climatic Water Deficit\nWithin-Year Seasonal Variability",
                 "Soil Water Availability\nAmong-Year Interannual Variability",
                 "Climatic Water Deficit\nAmong-Year Interannual Variability"
)

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

p <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("tempy", names(vars_st[i])) == TRUE) {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
    n = n + 1
  } else {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

z <- ggplot() + theme_void()

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8)
             )

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 17, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 17, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 17, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 17, angle = 0)

jpeg("moisture.variability.jpg", width = 1600, height = 1900)
grid.arrange(grobs = list(z.1,z.2,p[[1]],p[[2]], 
                          z.3,z.4,p[[3]],p[[4]]
                          ),
             nrows = 4,
             layout_matrix = lay,
             widths = c(800,800),
             heights = c(150,
                         800,
                         150,
                         800
                         )
)
dev.off()









#hot drought
vars <- list(cwdmx_hist[1],
             dsi_hist[1],
             dsic_hist[1]
)
names(vars) <- list("cwdmx",
                    "dsi",
                    "dsic"
)
vars.titles <- c("Climatic Water Deficit\n10-Day Maximum (mm)",
                 "Dry Soil Intervals\n Length (days)",
                 "Number of\nDry Soil Intervals"
)

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

p <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("tempy", names(vars_st[i])) == TRUE) {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
    n = n + 1
  } else {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

z <- ggplot() + theme_void()

lay <- rbind(c(1,2,3),
             c(4,5,6)
)

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 0)

jpeg("hot.drought.jpg", width = 2400, height = 950)
grid.arrange(grobs = list(z.1,z.2,z.3,p[[1]],p[[2]],p[[3]]
                          ),
                        nrows = 2,
                        layout_matrix = lay,
                        widths = c(800,800,800),
                        heights = c(150,
                                    800
                        )
)
dev.off()








#recruitment
vars <- list(sprec_hist[1],
             frec_hist[1],
             sprec_n[[1]][1],
             frec_n[[1]][1]
)
names(vars) <- list("sprec",
                    "frec",
                    "sprec.n",
                    "frec.n"
)
vars.titles <- c("Spring Recruitment (dd)",
                 "Fall Recruitment (dd)",
                 "Proportion of Years\nwith Spring Recruitment",
                 "Proportion of Years\nwith Fall Recruitment"
)

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

p <- list(NA)
n = 1
for (i in 1:length(vars)) {
  if (grepl("tempy", names(vars_st[i])) == TRUE) {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div, ovrd.div = TRUE)
    n = n + 1
  } else {
    p[[n]] <- plot_cor_map(vars_st[i], stars_agree, metric_name, color.scale = v_colors, div.color.scale = v_div)
    n = n + 1
  }
}

z <- ggplot() + theme_void()

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8)
)

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 0)

jpeg("recruitment.jpg", width = 1600, height = 1900)
grid.arrange(grobs = list(z.1,z.2,
                          p[[1]],p[[2]], 
                          z.3,z.4,
                          p[[3]],p[[4]]
          ),
          nrows = 2,
          layout_matrix = lay,
          widths = c(800,800),
          heights = c(150,
                      800,
                      150,
                      800
)
)
dev.off()







#climate and frost
vars <- list(tempy_hist[1],
             precy_hist[1],
             last_hist[1],
             first_hist[1],
             flf_hist[1],
             fff_hist[1]
)
names(vars) <- list("MAT",
                    "MAP",
                    "LAST",
                    "FIRST",
                    "FLF",
                    "FFF"
)
vars.titles <- c("Mean Annual Temperature (°C)",
                 "Mean Annual Precipitation (mm)",
                 "Last Frost (day of year)",
                 "First Frost (day of year)",
                 "Proportion of Years\nwith Last Frost",
                 "Proportion of Years\nwith First Frost"
                
)

vars_st <- vars[[1]][1] 
names(vars_st) <- names(vars[1])

for (i in 2:length(vars)) { 
  
  new_value <- vars[[i]][1]
  names(new_value) <- names(vars[i])
  vars_st <- c(vars_st, new_value)  
  
}

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

z <- ggplot() + theme_void()

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12)
)

z.1 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[1], color="black", size = 18, angle = 0)
z.2 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[2], color="black", size = 18, angle = 0)
z.3 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[3], color="black", size = 18, angle = 0)
z.4 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[4], color="black", size = 18, angle = 0)
z.5 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[5], color="black", size = 18, angle = 0)
z.6 <- title.theme + annotate("text", x=-1, y=-1, label=vars.titles[6], color="black", size = 18, angle = 0)

jpeg("climate.jpg", width = 1600, height = 2850)
grid.arrange(grobs = list(z.1,z.2,
                          p[[1]],p[[2]], 
                          z.3,z.4,
                          p[[3]],p[[4]],
                          z.5,z.6,
                          p[[5]],p[[6]]
      ),
      nrow = 6,
      layout_matrix = lay,
      widths = c(800,800),
      heights = c(150,
                  800,
                  150,
                  800,
                  150,
                  800
      )
)
dev.off()



