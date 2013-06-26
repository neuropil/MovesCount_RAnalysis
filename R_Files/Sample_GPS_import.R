library(rgdal)
library(maptools)
library(ggmap)
library(ggplot2)

# Programmatically load in data



# Load in gpx data
gpx.raw <- readOGR(dsn = "Move_2013_05_31_19_14_34_ACT_RUNNING.gpx", layer = "tracks")

# Extract coordinates from gpx data
cords <- coordinates(gpx.raw)
reduce.cords <- lapply(cords, function(x) do.call("rbind", x))
reduce.cords = reduce.cords[[1]]


output <- matrix(unlist(reduce.cords ), ncol = 2, byrow = TRUE)

# Load in kml data with altitude values
kml.raw <- getKMLcoordinates('Move_2013_05_31_19_14_34_ACT_RUNNING.kml', ignoreAltitude = FALSE)

# FOR TROUBLE SHOOTING
# murder <- subset(crime, offense == "murder")
# 
# kmlTest = reduce.cords[1:157,]
# 
# murder$lon = kmlTest[,1]
# murder$lat = kmlTest[,2]
# 
# qmplot(lon, lat, data = murder,
#        colour = I('red'), size = I(3), darken = .3)

