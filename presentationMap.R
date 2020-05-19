library(tidyverse)
library(sf)
library(leaflet)
library(leafem)
library(mapview)
library(RPostgres)
library(raster)


connectU <- dbConnect(drv = Postgres(), 
                      host = 'localhost', dbname = 'hikingDatabase', 
                      user = 'CDuncan',   password = 'Cheviot')
# Format summit positions ----
summitNames <- 
  read_csv('raw/coordsOfExtendedWainwrights.csv') %>%
  transmute(hillnumber = as.character(hillnumber), hillname, metres, NameAndNum = paste0(hillname, ' (', hillnumber, ')'), Elevation = paste0(metres, ' m'))

summitPositions <- 
  read_sf(connectU, Id(schema = 'clean_data', table = 'summitPositions'), crs = 27700) %>%
  mutate(hillNumber = as.character(hillNumber)) %>%
  left_join(summitNames, by = c('hillNumber' = 'hillnumber')) %>%
  transmute(Hill = hillname, Number = hillNumber, NameAndNum, Elevation , Wainwright = if_else(W==1, 'Inner', 'Outer'), geom)

# Format paths ----
formatPath <- function(path, nameLookup) {
  
  pathWithName <- path %>%
    left_join(nameLookup, by = c('from' = 'hillnumber')) %>%
    transmute(order, from = NameAndNum, to, cost, geom) %>%
    left_join(nameLookup, by = c('to' = 'hillnumber')) %>%
    transmute(Number = order, From = from, To = NameAndNum, Cost = round(as.numeric(cost)), geom) 
}


C1OW <- read_sf('raw/paths/minimumOW0067.gpkg') %>% formatPath(nameLookup = summitNames)
C1WW <- read_sf('raw/paths/minimumWW0067.gpkg') %>% formatPath(nameLookup = summitNames)
C2OW <- read_sf('raw/paths/minimumOW0133.gpkg') %>% formatPath(nameLookup = summitNames)
C2WW <- read_sf('raw/paths/minimumWW0133.gpkg') %>% formatPath(nameLookup = summitNames)
LWW <- read_sf('raw/paths/originalMin.gpkg') %>% rename(cost = weight) %>% formatPath(nameLookup = summitNames)

# Input raster data ----
airMap <- stack('raw/testCrop/aerialTriQuintRes.tif')
satMap <- stack('raw/testCrop/sen2TriBand.tif')
mlMap <- stack('raw/testCrop/reclassifiedLC5mTri.tif')
frictMap <- stack('raw/testCrop/frictionMapTri.tif')

# Plot maps ----



map1 <- 
  mapview(C1WW, layer.name = 'Wainwrights: Campbell 0.067', color = 'mediumblue', hide = TRUE, homebutton = FALSE)+
  mapview(C2WW, layer.name = 'Wainwrights: Campbell 0.133', color = 'darkgreen', hide = TRUE, homebutton = FALSE)+
  mapview(LWW, layer.name = 'Wainwrights: Langmuir', color = 'purple', hide = TRUE,homebutton = FALSE)+
  mapview(C1OW, layer.name = 'Greater Wainwrights: Campbell 0.067', color = 'dodgerblue1', hide = TRUE, homebutton = FALSE)+
  mapview(C2OW, layer.name = 'Greater Wainwrights: Campbell 0.133', color = 'green1', hide = TRUE, homebutton = FALSE)+
  
  
  mapview(summitPositions, layer.name = "Wainwright",
          zcol = "Wainwright", label = summitPositions$NameAndNum,
          cex = 3.5, lwd = 1, col.regions = c('red', 'yellow'),
          homebutton = TRUE, hide = FALSE,
          popup = popupTable(summitPositions, zcol = c("Hill", "Number", "Elevation", 'Wainwright')))+
          
  viewRGB(airMap, r=1,g=2,b=3, layer.name = '<RASTER> Aerial map')+
  viewRGB(satMap, r=1,g=2,b=3, layer.name = '<RASTER> Sentinel-2 natural colour')+
  viewRGB(mlMap, r=1,g=2,b=3, layer.name = '<RASTER> Bespoke land cover map')+
  viewRGB(frictMap, r=1,g=2,b=3, layer.name = '<RASTER> Friction map')
  
  
  #map1
  mapshot(map1, url = paste0(getwd(),'/index.html'))
  
  