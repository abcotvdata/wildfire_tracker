# library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(htmlwidgets)

# California fires data
try(download.file("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=false",
              "data/calfire_activefires.geojson"))
# Read in geojson and then transform to sf format
calfire_activefires <- st_read("data/calfire_activefires.geojson")

# NOAA satellite fires
try(download.file("https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/latesthms.txt",
              "data/noaa_latest_fires.csv"))
noaa_latest_fires <- read_csv("data/noaa_latest_fires.csv")
# convert to geo file next

# NOAA satellite smoke sourced as shapefile, then read in as sf
try(download.file("https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/latest_smoke.shp","data/latest_smoke.shp"))
try(download.file("https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/latest_smoke.dbf","data/latest_smoke.dbf"))
try(download.file("https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/latest_smoke.shx","data/latest_smoke.shx"))
latest_smoke <- st_read("data/latest_smoke.shp")

# Active wildfire perimeters from NFIS
try(download.file("https://opendata.arcgis.com/datasets/2191f997056547bd9dc530ab9866ab61_0.geojson",
              "data/active_perimeters.geojson"))
# Read in and downsize to just what we need for project
nfis_perimeters <- st_read("data/active_perimeters.geojson") %>%
  select(1,2,6,7,9,10,17,18,19,20,24,28,33,48,49,50,52,53,64,65,67,68,70,84,85,90,91,109)

# NASA series of wildfires hotspots data
# Alaska is a separate file if we need/want it
# last 24 hours from VIIRS SUOMI NPP SATELLITE
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/suomi-npp-viirs-c2/csv/SUOMI_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
              "data/hotspots_npp.csv"))
# last 24 hours from VIIRS NOAA-20 satellite
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/noaa-20-viirs-c2/csv/J1_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
              "data/hotspots_noaa20.csv"))
# last 24 hours from MODIS satellite
try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/modis-c6.1/csv/MODIS_C6_1_USA_contiguous_and_Hawaii_24h.csv",
              "data/hotspots_modis.csv"))
# read-in satellites' data
hotspots_modis <- read_csv("data/hotspots_modis.csv", 
                           col_types = cols(satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                            acq_time = col_time(format = "%H%M")))
hotspots_noaa20 <- read_csv("data/hotspots_noaa20.csv", 
                            col_types = cols(satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                             acq_time = col_time(format = "%H%M")))
hotspots_npp <- read_csv("data/hotspots_npp.csv", 
                         col_types = cols(bright_ti4 = col_double(), bright_ti5 = col_double(), frp = col_double(), scan = col_double(), track = col_double(), latitude = col_double(), longitude = col_double(), satellite = col_character(), confidence = col_character(), acq_date = col_date(format = "%Y-%m-%d"), 
                                          acq_time = col_time(format = "%H%M")))
# combine those hotspots files into a single geo layer, clean up
hotspots <- bind_rows(hotspots_modis,hotspots_noaa20,hotspots_npp)
rm(hotspots_modis,hotspots_noaa20,hotspots_npp)

# Latest AQ geofile from AirNow
try(download.file("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowLatestContoursCombined/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson",
              "data/airnow_aq.geojson"))
air_quality <- st_read("data/airnow_aq.geojson")

# Create temporary perimeter label
perimeterLabel <- paste("<b><font size='4'>",nfis_perimeters$irwin_IncidentName," Fire</b></font size><br/><font size='3'>",
                        "Perimeter updated ",nfis_perimeters$poly_DateCurrent,"<font size>")
# Create color palette for air quality
airpal <- colorFactor(palette = c("#b1dbad", "#ffffb8", "#ffcc80","#ff8280","#957aa3","#a18f7f"), levels = c("1", "2", "3", "4","5","6"), na.color = "#ff8280")

# Make a common, simple point file that merges, appends, dedupes cal/federal
# Start by create a simplified, standard version of fed fire POINTS from current perimeters file
fed_fires <- nfis_perimeters %>%
  select(14,25,22,15,16,17,18,13,4,7,20,27,11,12) %>%
  filter(irwin_IncidentTypeCategory=="WF") %>%
  st_drop_geometry() %>%
  mutate(source="NFIS")
names(fed_fires) <- c("name", "state", "county", 
                      "location", "type", "latitude", "longitude", 
                      "started", "updated", "acres_burned", "percent_contained",
                      "fed_fire_id","fire_behavior", "fire_cause","source")

# Manual fixes of a couple fires with mistaken geocoordinates
fed_fires$latitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", 35.69468,fed_fires$latitude)
fed_fires$longitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", -105.335,fed_fires$longitude)
fed_fires$latitude <- ifelse(fed_fires$fed_fire_id=="2022-NMN4S-000034", 36.243,fed_fires$latitude)
fed_fires$longitude <- ifelse(fed_fires$fed_fire_id=="2022-NMN4S-000034", -105.038,fed_fires$longitude)

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
cal_fires$acres_burned <- as.numeric(cal_fires$acres_burned)
cal_fires$percent_contained <- as.numeric(cal_fires$percent_contained)

# merge into one file
fires <- bind_rows(fed_fires,cal_fires)

# fix fire name field so it's consistent across all data we're going to use
fires$name <- str_to_title(fires$name)
fires$name <- paste0(fires$name," Fire")
fires$name <- gsub("Fire Fire", "Fire", fires$name)
fires$name <- gsub("  ", " ", fires$name)
# standardize state column
fires$state <- gsub("US-", "", fires$state)
# save latest merged fire points file as csv
write_csv(fires,"wildfires_working.csv")
# set values for dynamic zoom in feature in map
max_lat <- fires$latitude[which.max(fires$acres_burned)]
max_lon <- fires$longitude[which.max(fires$acres_burned)]

# remove fires without lat longs yet
# validation shows these are tiny almost all <1ac and all <10ac
fires <- fires %>% filter(!is.na(latitude) & !is.na(longitude))
# transform to properly projected spatial points data
fires <- st_as_sf(fires, coords = c("longitude", "latitude"), 
                  crs = 4326)

# make the wildfireIcon
fireIcons <- icons(
  iconUrl = ifelse(fires$percent_contained != "100" | is.na(fires$percent_contained),
                   "fireorange.png",
                   "firegrey.png"),
  iconWidth = 18, iconHeight = 18)

# setup the popup box content for fire POINTS and assign to a value 
fireLabel <- paste(sep = "<br/>",
                   paste("<font size='4'><b>",fires$name,"</font size></b>"),
                   paste("<font size='2'>In <b>",fires$county," County</font size></b>"),
                   paste(" "),
                   paste("<b>Acres burned: </b>",prettyNum(fires$acres_burned,big.mark=",")),
                   paste("<b>Percent contained: </b>",fires$percent_contained),
                   paste(" "),
                   paste("<b>Started: </b>", paste(as.character(as.POSIXct(fires$started, format = "%Y-%m-%d %H:%M"), format = "%b %d, %Y at %I:%M %p"))),
                   paste("<b>Updated: </b>", paste(as.character(as.POSIXct(fires$updated, format = "%Y-%m-%d %H:%M"), format = "%b %d, %Y at %I:%M %p")),"</font size>"),
                   paste(" "),
                   paste("<font size='1'>","* Acres burned and containment are updated as state or federal governments release information.","</font size>")
)

# New wildfire map include fires, smoke and hotspots
wildfire_map <- leaflet(noaa_latest_fires) %>%
  setView(max_lon, max_lat, zoom = 5) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Street Map") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(radius = 1.5,
                   color = "orange",
                   stroke = FALSE,
                   fillOpacity = 0.9,
                   group="Hot Spots") %>%
  addCircleMarkers(data = hotspots,
                   radius = 1.5,
                   color = "orange",
                   stroke = FALSE,
                   fillOpacity = 0.9,
                   group="Hot Spots") %>%
  addMarkers(data = fires,
             popup = fireLabel,
             icon = fireIcons,
             group="Fires") %>%
  addPolygons(data = nfis_perimeters, 
              color = "#be0000",
              popup = perimeterLabel,
              weight = 1.5,
              group="Perimeters") %>%
  addPolygons(data = latest_smoke, 
              color = "#916d61",
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
    baseGroups = c("Street Map", "Dark", "Satellite"),
    overlayGroups = c("Fires","Perimeters", "Hot Spots","Smoke","Air Quality"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomright') %>% hideGroup(c("Smoke","Air Quality")) 
# %>%
#  addSearchFeatures(options =
#                    searchFeaturesOptions(zoom = 10),
#                  targetGroups = 'Fires')
# wildfire_map

# Temporary replacement for existing Cali-only
# wildfire map include fires and perimeters only
california_map <- leaflet(noaa_latest_fires) %>%
  setView(-122.5, 37.5, zoom = 6) %>% 
  addProviderTiles(provider = "Stamen.Toner") %>%
  addCircleMarkers(radius = 1.5,
                   color = "orange",
                   stroke = FALSE,
                   fillOpacity = 0.9,
                   group="Hot Spots") %>%
  addCircleMarkers(data = hotspots,
                   radius = 1.5,
                   color = "orange",
                   stroke = FALSE,
                   fillOpacity = 0.9,
                   group="Hot Spots") %>%
  addMarkers(data = fires,
             popup = fireLabel,
             icon = fireIcons,
             group="Fires") %>%
  addPolygons(data = nfis_perimeters, 
              color = "#be0000",
              popup = perimeterLabel,
              weight = 1.5,
              group="Perimeters") %>%
  addPolygons(data = latest_smoke, 
              color = "#916d61",
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
    overlayGroups = c("Fires","Perimeters", "Hot Spots","Smoke","Air Quality"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomright') %>% hideGroup(c("Hot Spots","Smoke","Air Quality"))
# california_map

# Export as HTML file
saveWidget(california_map, 'docs/map_california.html', title = "ABC Owned Television Stations Wildfire Tracker", selfcontained = TRUE)
saveWidget(wildfire_map, 'docs/wildfire_map.html', title = "ABC Owned Television Stations Wildfire Tracker", selfcontained = TRUE)

