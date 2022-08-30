# library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(htmlwidgets)

# Quick notes on source data
# Data fetched once a day is downloaded in a separate script + action
# Data fetched hourly or similar is downloaded in this script to be as "live" as possible
# Currently, smoke polygons is once daily and CA fire, federal fire, satellite hotspots and AQ are "live"

# Overview of steps
# SECTION 1. Fetch all data.
# SECTION 2. Read in Air Quality and Smoke data.
# SECTION 3. Read in and reshape satellite hot spots data.
# SECTION 4. Read in and reshape Federal fire data into points and polygons.
# SECTION 5. Read in and reshape California fire data.
# SECTION 6. Merge federal and California fire points.
# SECTION 7. Script popup and icons for fire layer(s).
# SECTION 8. Script color palettes for maps.
# SECTION 9. Script national leaflet map.
# SECTION 10. Script California leaflet map.
# SECTION 11. Write leaflet maps to html.

### SECTION 1. Fetch all data ###
# We use try function throughout so that if a file is down temporarily
# this script won't stop; the map will be made with the last/newest data

# Get active California fires data from Calfire
try(download.file("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=false",
                  "data/calfire_activefires.geojson"))

# Get active wildfire perimeters from NFIS, which we use for both perimeters and points
# Separate point file if we ever need again is here: https://opendata.arcgis.com/datasets/51192330d3f14664bd69b6faed0fdf05_0.geojson
try(download.file("https://opendata.arcgis.com/datasets/2191f997056547bd9dc530ab9866ab61_0.geojson",
                  "data/active_perimeters.geojson"))

# Latest AQ geofile from AirNow
try(download.file("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowLatestContoursCombined/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson",
                  "data/airnow_aq.geojson"))

# NASA series of wildfires hotspots data
# Alaska is a separate file if we need/want it
# last 24 hours from VIIRS SUOMI NPP satellite
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/suomi-npp-viirs-c2/csv/SUOMI_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
                  "data/hotspots_npp.csv"))
# last 24 hours from VIIRS NOAA-20 satellite
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/noaa-20-viirs-c2/csv/J1_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
                  "data/hotspots_noaa20.csv"))
# last 24 hours from MODIS satellite
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/modis-c6.1/csv/MODIS_C6_1_USA_contiguous_and_Hawaii_24h.csv",
                  "data/hotspots_modis.csv"))

### SECTION 2. Read in Air Quality and Smoke data. ###
# Load/Read Air Quality geojson
air_quality <- st_read("data/airnow_aq.geojson")
# Load/Read NOAA satellite smoke shapefile fetched daily in separate script/action
noaa_latest_smoke <- st_read("data/satellite/smoke/noaa_latest_smoke.shp")

### SECTION 3. Read in and reshape satellite hot spots data. ###

# Load/read three satellites shapefiles
hotspots_modis <- read_csv("data/hotspots_modis.csv", 
                           col_types = cols(satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                            acq_time = col_time(format = "%H%M")))
hotspots_noaa20 <- read_csv("data/hotspots_noaa20.csv", 
                            col_types = cols(satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                             acq_time = col_time(format = "%H%M")))
hotspots_npp <- read_csv("data/hotspots_npp.csv", 
                         col_types = cols(bright_ti4 = col_double(), bright_ti5 = col_double(), frp = col_double(), scan = col_double(), track = col_double(), latitude = col_double(), longitude = col_double(), satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                          acq_time = col_time(format = "%H%M")))

# Combine those three hotspots files into a single geo layer, then clean up
hotspots <- bind_rows(hotspots_modis,hotspots_noaa20,hotspots_npp)
rm(hotspots_modis,hotspots_noaa20,hotspots_npp)


### SECTION 4. Read in and reshape Federal fire data into points and polygons. ###

# Load/read federal fire perimeters and downsize to just what we need for project
nfis_perimeters <- st_read("data/active_perimeters.geojson") %>%
  select(1,2,6,7,9,10,17,18,19,20,24,28,33,48,49,50,52,53,64,65,67,68,70,84,85,90,91,109)

# Create tighter federal fire points file from current perimeters file
fed_fires <- nfis_perimeters %>%
  select(14,25,22,15,16,17,18,13,4,7,20,27,11,12) %>%
  st_drop_geometry() %>%
  mutate(source="NFIS")
# Rename fields to match the California fire points file
names(fed_fires) <- c("name", "state", "county", 
                      "location", "type", "latitude", "longitude", 
                      "started", "updated", "acres_burned", "percent_contained",
                      "fed_fire_id","fire_behavior", "fire_cause","source")
# Clean numeric fields, round for days burning and days since update for later filtering
fed_fires$days_burning <- floor(difftime(Sys.Date(),fed_fires$started, units="days"))+1
fed_fires$days_sinceupdate <- difftime(Sys.Date(),fed_fires$updated, units="days")
# filter out small fires and old fires not updated for more than a week
# except for leaving in very new fires
fed_fires <- fed_fires %>%
  filter(acres_burned>99 & days_sinceupdate<8 |
           days_sinceupdate<3)

# Fix fire name field so it's consistent as possible across all data we're using
fed_fires$name <- str_to_title(fed_fires$name)
fed_fires$name <- paste0(fed_fires$name," Fire")
fed_fires$name <- trimws(fed_fires$name)
# standardize state column
fed_fires$state <- gsub("US-", "", fed_fires$state)

# Leave in, but comment out until/unless needed for MANUAL FIRE EDITS
# Manual fixes of a couple fires with mistaken geocoordinates for point origin
# fed_fires$latitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", 35.69468,fed_fires$latitude)
# fed_fires$longitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", -105.335,fed_fires$longitude)

# filter, shrink perimeters file to include only fields we need for layer
# and only leaves in those fires that remain in points file
nfis_perimeters <- nfis_perimeters %>%
  select(2,27) %>%
  filter(nfis_perimeters$irwin_UniqueFireIdentifier %in% fed_fires$fed_fire_id)
# simplifies and standardizes col names
names(nfis_perimeters) <- c("name", "fed_fire_id", "geometry")
# Fix fire name field so it's consistent as possible across all data we're using
nfis_perimeters$name <- str_to_title(nfis_perimeters$name)
nfis_perimeters$name <- paste0(nfis_perimeters$name," Fire")


### SECTION 5. Read in and reshape California fire data. ###

# Load/read in Calfire geojson; transform to sf format
calfire_activefires <- st_read("data/calfire_activefires.geojson")

# Simplify, standardize version of California Fires from CalFire's active list
cal_fires <- calfire_activefires %>%
  mutate(state="CA") %>%
  select(1,25,7,8,15,14,13,4,3,9,10,17) %>%
  st_drop_geometry() %>%
  mutate(source="Cal Fire")
names(cal_fires) <- c("name", "state", "county", 
                      "location", "type", "latitude", "longitude", 
                      "started", "updated", "acres_burned", "percent_contained",
                      "info_url","source")
# clean numeric fields
cal_fires$acres_burned <- round(as.numeric(cal_fires$acres_burned),0)
cal_fires$percent_contained <- as.numeric(cal_fires$percent_contained)
# calculating fields for time passed elements in popups and for filtering old fires
cal_fires$days_burning <- floor(difftime(Sys.Date(),cal_fires$started, units="days"))+1
cal_fires$days_sinceupdate <- difftime(Sys.Date(),cal_fires$updated, units="days")
# filter out small fires and old fires not updated for more than a week
# except for leaving in very new fires
cal_fires <- cal_fires %>%
  filter(acres_burned>99 & days_sinceupdate<8 |
           days_sinceupdate<3)

### SECTION 6. Merge federal and California fire points. ###

# Temporarily reduce California file
cal_fires_unique <- cal_fires %>%
  filter(!cal_fires$name %in% fed_fires$name)

# Combine into one file and clean up
fires <- bind_rows(fed_fires,cal_fires_unique)
rm(cal_fires_unique)

# Drop duplicates 
# my_data %>% distinct(Sepal.Length, Petal.Width, .keep_all = TRUE)

# Create flag for active vs. not for map icons
fires$active <- if_else(fires$days_sinceupdate<4,"Yes","No")

# Save latest merged fire points file as csv
write_csv(fires,"data/wildfires_working.csv")

# set values for dynamic zoom in feature in map
max_lat <- fires$latitude[which.max(fires$acres_burned)]
max_lon <- fires$longitude[which.max(fires$acres_burned)]

# remove fires without lat longs yet so they can be mapped
# validation shows these are tiny almost all <1ac and all <10ac
fires <- fires %>% filter(!is.na(latitude) & !is.na(longitude))
# transform to properly projected spatial points data
fires <- st_as_sf(fires, coords = c("longitude", "latitude"), 
                  crs = 4326)

### SECTION 7. Script popup and icons for fire layer(s). ###

fireLabel <- paste(sep = "<br/>",
                   paste("<font size='3'><b>",fires$name,"</font size></b>"),
                   paste("In ",fires$county," County</font size>,",fires$state,"</b>"),
                   paste(""),
                   paste("Started ",round(difftime(Sys.time(),fires$started,units='days'),0), "days ago"),
                   paste(prettyNum(fires$acres_burned,big.mark=","),"acres burned"),
                   paste(fires$percent_contained," percent contained"),
                   paste("<font size='1'>Updated ", paste(as.character(as.POSIXct(fires$updated, format = "%Y-%m-%d %H:%M"), format = "%b %d, %Y at %I:%M %p")),"</font size>")
)

# Create temporary perimeter label
perimeterLabel <- paste(nfis_perimeters$name)

# Create the fire icons
fireIcons <- icons(
  iconUrl = ifelse(fires$percent_contained == "100" | fires$active == "No",
                   "firegrey.png",
                   "fireorange.png"),
  iconWidth = 18, iconHeight = 18)

my_icons <- awesomeIcons(
  icon = "fire",
  iconColor = "white",
  library = 'glyphicon',
  squareMarker = TRUE,
  markerColor = "orange")

### SECTION 8. Script color palettes for maps. ###

# Create color palette for air quality
airpal <- colorFactor(palette = c("#b1dbad", "#ffffb8", "#ffcc80","#ff8280","#957aa3","#a18f7f"), levels = c("1", "2", "3", "4","5","6"), na.color = "#ff8280")
# Create color palette smokepal for the varying levels of intensity of smoke
smokepal <- colorFactor(palette = c("#99a0a5", "#51585f", "#2d343a"), levels = c("Light","Medium","Heavy"))

### SECTION 9. Script national leaflet map. ###

# New wildfire map include fires, smoke and hotspots
wildfire_map <- leaflet(hotspots) %>%
  setView(max_lon, max_lat, zoom = 7) %>% 
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircleMarkers(radius = 1.5,
                   color = "#be0000",
                   weight = 1,
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   group="Hot Spots") %>%
  addPolygons(data = nfis_perimeters, 
              color = "#00318b",
              popup = perimeterLabel,
              weight = 1.5,
              group="Perimeters") %>%
  addAwesomeMarkers(data = fires,
                    popup = fireLabel,
                    icon = my_icons,
                    group="Fires") %>%
  addPolygons(data = noaa_latest_smoke, 
              color = ~smokepal(Density),
              fillOpacity = 0.6,
              weight = 0,
              group="Smoke") %>%
  addPolygons(data = air_quality, 
              color = ~airpal(gridcode),
              weight = 0,
              fillOpacity = 0.6,
              group = "Air Quality") %>%
  addLegend(values = values(air_quality$gridcode), title = "Air Quality Index<br><a href='https://www.airnow.gov/aqi/aqi-basics/' target='blank'>What AQI ratings mean</a>", 
            group = "Air Quality", 
            colors = c("#b1dbad", "#ffffb8", "#ffcc80","#ff8280","#957aa3","#a18f7f","#dde4f0"),
            labels=c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous","No AQ Data"),
            position = 'bottomleft') %>%
  addLayersControl(
    overlayGroups = c("Fires","Perimeters","Hot Spots","Smoke","Air Quality"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomright') %>% hideGroup(c("Smoke","Air Quality")) 


### SECTION 10. Script California leaflet map. ###

california_map <- wildfire_map %>%
  setView(-122.5, 37.5, zoom = 6) %>% 
  addProviderTiles(provider = "Stamen.Toner")

### SECTION 11. Write leaflet maps to html. ###
saveWidget(california_map, 'docs/map_california.html', title = "ABC Owned Television Stations California Wildfire Tracker", selfcontained = TRUE)
saveWidget(wildfire_map, 'docs/wildfire_map.html', title = "ABC Owned Television Stations Wildfire Tracker", selfcontained = TRUE)

