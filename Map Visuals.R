# SET WORKING DIRECTORY
setwd("E:/Documents Storage/Text and Data Mining/Final Project")

# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(ggmap)

# LOAD DATA
marvelData <- read.csv("Marvel.csv")
MarvelCoords <- read.csv("MarvelCoords.csv")

MarvelCoords <- na.omit(MarvelCoords)

badCoords <- c(which(MarvelCoords$Location == "Earth"), which(MarvelCoords$Location == "MontrÃ©al, QuÃ©bec"),
               which(MarvelCoords$Location == "yeehaw"), which(MarvelCoords$Location == "King's Landing"),
               which(MarvelCoords$Location == "Monterrey, MÃ©xico"), which(MarvelCoords$Location == "Hoy"),
               which(MarvelCoords$Location == "MontrÃ©al, Qc"), which(MarvelCoords$Location == "LOCATION:"),
               which(MarvelCoords$Location == "<U+0413><U+043E><U+043B><U+043B><U+0438><U+0432><U+0443><U+0434>"),
               which(MarvelCoords$Location == "mars"), which(MarvelCoords$Location == "Atlantis"),
               which(MarvelCoords$Location == "The Former USA"), which(MarvelCoords$Location == "Florida & California"),
               which(MarvelCoords$Location == "STL"), which(MarvelCoords$Location == "Nuevo LeÃ³n, MÃ©xico"),
               which(MarvelCoords$Location == "Van NUys"), which(MarvelCoords$Location == "Monterrey, Nuevo LeÃ³n"),
               which(MarvelCoords$Location == "A Van down by the river"), which(MarvelCoords$Location == "LOVELY"),
               which(MarvelCoords$Location == "Los Angeles, CA by way of MI"), which(MarvelCoords$Location == "Niagara Region Ontario"),
               which(MarvelCoords$Location == "Home, sweet home"), which(MarvelCoords$Location == "cbe"),
               which(MarvelCoords$Location == "SWVA"), which(MarvelCoords$Location == "In the gym and garden"),
               which(MarvelCoords$Location == "The New world"), which(MarvelCoords$Location == "lovely"),
               which(MarvelCoords$Location == "Midwest"), which(MarvelCoords$Location == "united"),
               which(MarvelCoords$Location == "Two steps ahead"), which(MarvelCoords$Location == "Chihuahua, MÃ©xico"),
               which(MarvelCoords$Location == "Novo MÃ©xico"), which(MarvelCoords$Location == "Wonderland"),
               which(MarvelCoords$Location == "USS Enterprise"), which(MarvelCoords$Location == "Mx"),
               which(MarvelCoords$Location == "Mars"), which(MarvelCoords$Location == "Coahuila de Zaragoza, MÃ©xico"),
               which(MarvelCoords$Location == "Dreamland"), which(MarvelCoords$Location == "New"),
               which(MarvelCoords$Location == "Hogwarts"), which(MarvelCoords$Location == "JuÃ¡rez, Chihuahua"),
               which(MarvelCoords$Location == "QuÃ©bec, Canada"), which(MarvelCoords$Location == "U.S.A."),
               which(MarvelCoords$Location == "Pluto"), which(MarvelCoords$Location == "Chub"),
               which(MarvelCoords$Location == "home"), which(MarvelCoords$Location == "NOVA"),
               which(MarvelCoords$Location == "Etats-Unis"), which(MarvelCoords$Location == "D.F."),
               which(MarvelCoords$Location == "Bliss"), which(MarvelCoords$Location == "Saltillo, Coahuila de Zaragoza"),
               which(MarvelCoords$Location == "Matamoros, Tamaulipas"), which(MarvelCoords$Location == "Pe"),
               which(MarvelCoords$Location == "SC: joeydoomsday519"), which(MarvelCoords$Location == "TorreÃ³n, Coahuila de Zaragoza"),
               which(MarvelCoords$Location == "Clock Tower"), which(MarvelCoords$Location == "Central Islip"),
               which(MarvelCoords$Location == "Pub"), which(MarvelCoords$Location == "nowhere"),
               which(MarvelCoords$Location == "sea"), which(MarvelCoords$Location == "Saltillo, Coahuila, MÃ©xico"),
               which(MarvelCoords$Location == "Art"), which(MarvelCoords$Location == "Estados Unidos"),
               which(MarvelCoords$Location == "At the movies"), which(MarvelCoords$Location == "MontrÃ©al, QC, Canada"),
               which(MarvelCoords$Location == "Everywhere")
               )
MarvelCoords <- MarvelCoords[-badCoords,]


# PROJECT GOALS
USA <- geocode("Kansas")
USA.map <-get_map(location = USA, zoom = 4, maptype = "toner-background" , color = "bw", source = "google")
mapSimple <- ggmap(USA.map)
mapSimple

Marvel.Map <- mapSimple +
  geom_point(data = MarvelCoords, mapping = aes(x = lon, y = lat), size = 1.25, shape = 21, color = "black", fill = "#f0131e") +
  geom_density2d(data = MarvelCoords, mapping = aes(x = lon, y = lat), color = "#f0131e", lineend = "round", linejoin = "bevel") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Tweets about Marvel")

Marvel.Map

save.image(file = "Map_Visuals.RData")
