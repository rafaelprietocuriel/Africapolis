
# project created to compute the pairwise distance between agglomerations in Africa
# It uses Africapolis dataset. 
# The data is available here: http://africapolis.org/
# It uses the package 'geosphere' for computing spatial distances

# package
  require(geosphere)

# load the database
  DB <- read.csv("Africapolis_agglomeration_2015.csv")

# distance between pair of cities in kilometres
  DistM <- distm(cbind(x = DB$Longitude, y = DB$Latitude))/1000

# remove the diagonal, by considering a large distance between two equal cities
  DistM[upper.tri(DistM)] <- NA

# How many cities are at a distance of 5 kilometres?
  CommutingCities <- sum(DistM < 5)
# note that it is divided by 2, as every pair of cities is counted twice

# plot cities and nearby cities
# it creates a png file, as the display is a bit slow
# Note that it is possible to adjust the distance, by changing the parameter below
  MaxDistance <- 120
  w <- which(DistM < MaxDistance, arr.ind = TRUE)
  png(filename = "CitiesMap.png",
    width = 980, height = 980)
  par(mar = c(0,0,0,0), bg = "black")
  plot(-1000, 
     xlim = c(-17, 45), ylim = c(-33, 38),
     asp = 1)
  colCities <- rgb(.1, .4, .7, .1)
  colEdges <- rgb(.8, .1, .1, .1)
  for (k in 1:dim(w)[1]){
    or <- w[k, 1]
    de <- w[k, 2]
    points(c(DB$Longitude[or], DB$Longitude[de]), 
         c(DB$Latitude[or], DB$Latitude[de]),
         type = "l", 
         lwd = 1,
         col = colEdges )
  } 
  points(DB$Longitude, DB$Latitude,
       pch = 20,
       cex = .1,
       col = colCities)

dev.off()

