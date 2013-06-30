rankall <- function(csvFile, gpxFile = FALSE, kmlFile = FALSE, gender = "M", weight = 146, age = 33, HR.mode = "calculate"){

library(rgdal)
library(maptools)
gpclibPermit()
library(ggmap)
library(ggplot2)
library(plyr)
library(stringr)


# Use temporary GPX file while creating

gpxFile = "Move_2013_05_31_19_14_34_ACT_RUNNING.gpx"
kmlFile = "Move_2013_05_31_19_14_34_ACT_RUNNING.kml"


########### CHECK INPUTS #################################

# Get File Names for KML file and/or GPS file if requested

fileTitle = unlist(strsplit(file,split=".csv"))

if (gpxFile = TRUE){
  gpxFile = str_join(fileTitle,".gpx")
  
  # Load in gpx data
  gpx.raw <- readOGR(dsn = gpxFile, layer = "tracks")
  
  # Extract coordinates from gpx data
  cords <- coordinates(gpx.raw)
  reduce.cords <- lapply(cords, function(x) do.call("rbind", x))
  reduce.cords = reduce.cords[[1]]
  # Generate GPS matrix
  gpsMat <- reduce.cords
}

if (kmlFile = TRUE){
  kmlFle = str_join(fileTitle,".kml")
  
  # Load in kml data with altitude values
  kml.raw <- getKMLcoordinates(kmlFile, ignoreAltitude = FALSE)
  # Generate KML matrix
  kmlMat = matrix(unlist(kml.raw), ncol = 3, byrow = FALSE)
}

# Check Gender Data

if (gender == "M" | gender == "m") {
  g = 1
} else if (gender == "F" | gender == "f") {
  g = 0
} else {
  print("incorrect selection")
}


# Load in CSV data
# Remove lots of extraneous values
file = "Move_2013_05_31_19_14_34_ACT_RUNNING.xlsx"

# Convert to CSV and reduce columns
excelToCsv(file, target_dir = date_dir)

# Consider trying this code
moveData <- read.csv("Move_2013_05_31_19_14_34_ACT_RUNNING.csv") #

# Remove extraneous columns
moveData$SeaLevelPressure <- NULL

# Distance is in meters
# Speed is miles per hour

# 1. Get Minute Averages
minute.Indices = list()

mdd.names = names(moveData)

minutes = levels(moveData$LocalTime)

minute.MoveData <- matrix(0, length(minutes), length(mdd.names)-1)

for(i in seq(along = levels(moveData$LocalTime))) {
  # Get name of first minute (level of LocalTime)
  minute = levels(moveData$LocalTime)[i]
  # Append each list of indices to Index List of lists
  minute.Indices = c(minute.Indices, list(which(moveData$LocalTime == minute))) 
  # for loop columns and add average data for index of minute; row value is index
  count = 1;
  for(ni in 2:length(mdd.names)) {

      minute.MoveData[i,count] = round(mean(moveData[minute.Indices[[i]],mdd.names[ni]]), digits = 2)
      count = count + 1
  }   
}

minute.MoveData <- data.frame(minute.MoveData)
minute.MoveData <- data.frame(LocalTime = minutes, minute.MoveData)
names(minute.MoveData)[c(2:6)] <- mdd.names[2:6] 
minute.MoveData <- rename(minute.MoveData, c("Distance" = "Distance.meters"))

# 2. Get Index of non-zero rows

nonzero.Indices <- which(minute.MoveData$Speed != 0 & minute.MoveData$HeartRate != 0)
# Remove zero rows
minute.MoveData <- minute.MoveData[nonzero.Indices,]
# 3. Combine non-zero rows with gps data

# Get GPS or KML row indices
gps.rows = round(seq(1,nrow(kmlMat),length = nrow(minute.MoveData)))

# Add gps rows to Dataframe: longitude is column 1 and latitude is column 2
minute.MoveData <- data.frame(Long = gpsMat[gps.rows,1], Lat = gpsMat[gps.rows, 2], minute.MoveData)

# 4. Calculate new columns
#    a. Speed (miles per hour) speed multiplied by miles per hour 
#    b. Distance (in miles)
#    c. Pace (minute for mile)
#    d. VO2max (ml/kg)

Speed.miles = NULL
Distance.miles = NULL
Pace.timeformat = NULL
Pace.decformat = NULL
VO2max = NULL
for(fi in 1:nrow(minute.MoveData)){
  #    a. Speed (miles per hour) speed multiplied by miles per hour 
  Speed.miles[fi] = minute.MoveData$Speed[fi] * 2.2369 # meters per hour
  #    b. Distance (in miles)
  Distance.miles[fi] = minute.MoveData$Distance.meters[fi] / 1609.34 # meters per hour
  
  tempPace.raw = 60/Speed.miles[fi]
  tempPace.minute = as.integer(60/Speed.miles[fi])
  tempPace.second = as.integer((tempPace.raw %% tempPace.minute) * 60)
  #    c. Pace (minute for mile)
  Pace.timeformat[fi] = paste(as.character(tempPace.minute), ":", as.character(tempPace.second), sep = "")
  Pace.decformat[fi] = tempPace.minute + (tempPace.second / 100)
  #    d. VO2max (ml/kg)
  VO2_max[fi] = 132.853 - (0.0769 * weight) - (0.3877 * age) + (6.315 * g) - (3.2649 * Pace.decformat[fi]) - (0.1565 * minute.MoveData$HeartRate[fi])
}

# Add new features to DATAFRAME






# Extract values of interest

MAX_HR = max(moveData$HeartRate)

Rest_HR = mean(moveData$HeartRate[1:10])

Ave_HR = mean(moveData$HeartRate)

# find heart-rate zones

# Males 214 - Your age = MAx HR

# MAx_HR - Rest_HR = DIFFERENCE

# Difference * (60% range) 0.6
# Add Rest_HR

# FAT-Buring 70-80%
# Glycogen buring 80-90%
# REd Line zone 90-100%




VO2_max = 132.853 - (0.0769 * weight) - (0.3877 * age) + (6.315 * g) - (3.2649 * 14.54) - (0.1565 * 133)

# EPOC (an estimated measure of oxygen debt), Respiration, and R-R.


# VO2max = 15.3 * (max HR/rest HR) 
# MHR = beats/min 208 - (0.7*age)
# RHR = number of heart beats in 20 seconds * 3
# VO2 = 132.853 - (0.0769 * Wieght(pounds)) - (0.3877 * Age) + (6.315 * Gender(1 for male)) - (3.2649 * Time (to complete a mile in minutes)) - (0.1565 * Heart Rate)
# VO2 value (in ml/kg/min)

}

# source("excelToCsv.R")


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

######
#C:\Program Files\Java\jre7\bin\server