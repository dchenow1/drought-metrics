


#aggregate sites by simple feature polygons
#stars object is the stars object that has the full study region extent

#add the column with the names to categorize by as an argument?

agg_polys_table <-
  function(stars_object, polys) {
    extract_poly <- 
      function(stars_object, polys) {
        polys <- st_transform(polys, st_crs(stars_object))
        polys <- st_transform(polys, st_crs(tdd_hist))
        m <- rep(list(NA), length(unique(polys$)))
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





hi <- function(stars, column) {
  #mean(stars[column])
  print(stars[column])
}

hi(ecoregion_centroids,column = "newname")





