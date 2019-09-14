# SET WORKING DIRECTORY
setwd("E:/Documents Storage/Text and Data Mining/Final Project")

# LOAD LIBRARIES
library(dplyr)
library(tm)
library(rtweet)


register_google(key = "AIzaSyDgjxFP4g-f_MZM1lbAXUn2NVgjGlE8Hrk", write = TRUE)


# DATA COLLECTION
myData <- search_tweets("#Marvel", type = "recent", geocode = lookup_coords("usa"), "lang::en", timeout = 60 * 60 * 24 * 7, n = 10000, include_rts = FALSE)
marvelData <- myData[,c(1:7, 13:14, 17, 32, 73:75, 84)]

Coords <- as.data.frame(marvelData$location)
names(Coords) <- "Location"
Coords$Location <- as.character(Coords$Location)


for (i in 1:nrow(Coords)) {
  latlon = geocode(Coords[i,1])
  Coords$lon[i] = as.numeric(latlon[1])
  Coords$lat[i] = as.numeric(latlon[2])
}


us <- map_data("state")

goodLon <- between(Coords$lon, min(us$long), max(us$long))
goodLat <- between(Coords$lat, min(us$lat), max(us$lat))

remove <- c(which(goodLon == "FALSE"), which(goodLat == "FALSE"))
remove <- unique(remove)

marvelData<- marvelData[-remove,]
Coords <- Coords[-remove,]


save_as_csv(marvelData, file_name = "Marvel.csv")
save_as_csv(Coords, file_name = "MarvelCoords.csv")
