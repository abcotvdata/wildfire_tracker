library(dplyr)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(htmlwidgets)
library(htmltools)
library(janitor)

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
# SECTION 9. Script a base wildfire map.
# SECTION 10. Script national map variant(s).
# SECTION 11. Script California map variant(s).
# SECTION 12. Write all leaflet maps to html.

### SECTION 1. Fetch all data ###
# We use try function throughout so that if a file is down temporarily
# this script won't stop; the map will be made with the last/newest data

# Get active CALIFORNIA FIRES data from Calfire
temp_file <- "data/temp.geojson"
original_file <- "data/calfire_activefires.geojson"

# Try downloading the file to a temporary file
download_status <- try(download.file("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true", temp_file))

# If download is successful, move the temporary file to replace the original file
if (!inherits(download_status, "try-error")) {
  file.rename(temp_file, original_file)
}

# Get active FEDERAL WILDFIRE PERIMETERS from NFIS, which we use for both perimeters and points
# Separate point file if we ever need again is here: https://opendata.arcgis.com/datasets/51192330d3f14664bd69b6faed0fdf05_0.geojson
# try(download.file("https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/Current_WildlandFire_Locations/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
#                  "data/active_points.geojson"))
#try(download.file("https://opendata.arcgis.com/datasets/2191f997056547bd9dc530ab9866ab61_0.geojson",
#                 "data/active_perimeters2.geojson"))
# try(download.file("https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/Current_WildlandFire_Perimeters/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
#                  "data/active_perimeters.geojson"))
try(download.file("https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
                  "data/active_perimeters.geojson"))

# Get latest AIR QUALITY geojson polygon data from government's AirNow
try(download.file("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowLatestContoursCombined/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson",
                  "data/airnow_aq.geojson"))

# Get SATELLITE HOTSPOTS data from NASA
# Alaska is a separate file if we need/want it
# last 24 hours from VIIRS SUOMI NPP satellite
# try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/suomi-npp-viirs-c2/csv/SUOMI_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
#                  "data/hotspots_npp.csv"))
# last 24 hours from VIIRS NOAA-20 satellite
#try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/noaa-20-viirs-c2/csv/J1_VIIRS_C2_USA_contiguous_and_Hawaii_24h.csv",
#                  "data/hotspots_noaa20.csv"))
# last 24 hours from MODIS satellite
#try(download.file("https://firms.modaps.eosdis.nasa.gov/data/active_fire/modis-c6.1/csv/MODIS_C6_1_USA_contiguous_and_Hawaii_24h.csv",
#                  "data/hotspots_modis.csv"))


# Get FIRE DANGER FORECASTS from U.S. Forest Service's Wildland Fire Assessment System
try(download.file("https://www.wfas.net/images/firedanger/fdr_fcst.txt","data/wfas_forecast.txt"))
# OPEN WORK: Move this to the separate once-a-day download script + action

### SECTION 2. Read in Air Quality and Smoke data. ###

# Load/read Air Quality geojson
air_quality <- st_read("data/airnow_aq.geojson")
# Load/read NOAA satellite smoke shapefile fetched daily in separate script/action
noaa_latest_smoke <- st_read("data/satellite/smoke/noaa_latest_smoke.shp")
# Load, read and clean portion of USFS fire forecast station file with forecast adjectives
usfs_forecast <- read_fwf("data/wfas_forecast.txt", skip = 7,
                          fwf_cols(station_id = c(1, 7), 
                                   station_name = c(8, 26),
                                   latitude = c(32, 36),
                                   longitude = c(37, 42),
                                   fdc_adj = c(112, 114)))
usfs_forecast <- usfs_forecast %>% filter(!is.na(latitude) & !is.na(longitude) & !is.na(fdc_adj))
usfs_forecast <- usfs_forecast %>% filter(latitude != "lat" & longitude != "long" & fdc_adj != "ADJ")
usfs_forecast$longitude <- paste(sep="","-",usfs_forecast$longitude)
usfs_forecast$longitude <- gsub("--", "-", usfs_forecast$longitude)
usfs_forecast$latitude <- as.numeric(usfs_forecast$latitude)
usfs_forecast$longitude <- as.numeric(usfs_forecast$longitude)

### SECTION 3. Read in and reshape satellite hot spots data. ###

# Load/read three satellites shapefiles
#hotspots_modis <- read_csv("data/hotspots_modis.csv", 
#                           col_types = cols(satellite = col_character(),
#                                            latitude = col_number(),
#                                            longitude = col_number(),
#                                            scan = col_number(),
#                                            track = col_number(),
#                                            frp = col_number(),
#                                            bright_t31 = col_number(),
#                                            brightness = col_number(),
#                                            confidence = col_character(),
#                                            version = col_character(),
#                                            daynight = col_character(),
#                                            acq_date = col_date(format = "%Y-%m-%d"), 
#                                            acq_time = col_time(format = "%H%M")
#                                            ))
#hotspots_noaa20 <- read_csv("data/hotspots_noaa20.csv", 
#                            col_types = cols(satellite = col_character(),
#                                             latitude = col_number(),
#                                             longitude = col_number(),
#                                            scan = col_number(),
#                                             track = col_number(),
#                                             frp = col_number(),
#                                             bright_ti5 = col_number(),
#                                             bright_ti4 = col_number(),
#                                             confidence = col_character(),
#                                             version = col_character(),
#                                             daynight = col_character(),
#                                             acq_date = col_date(format = "%Y-%m-%d"), 
#                                             acq_time = col_time(format = "%H%M")
#                            ))
#hotspots_npp <- read_csv("data/hotspots_npp.csv", 
#                         col_types = cols(satellite = col_character(),
#                                          latitude = col_number(),
#                                          longitude = col_number(),
#                                          scan = col_number(),
#                                          track = col_number(),
#                                          frp = col_number(),
#                                          bright_ti5 = col_number(),
#                                          bright_ti4 = col_number(),
#                                          confidence = col_character(),
#                                          version = col_character(),
#                                          daynight = col_character(),
#                                          acq_date = col_date(format = "%Y-%m-%d"), 
#                                          acq_time = col_time(format = "%H%M")
#                         ))

# Combine those three hotspots files into a single geo layer, then clean up
# hotspots <- bind_rows(hotspots_modis,hotspots_noaa20,hotspots_npp)
# rm(hotspots_modis,hotspots_noaa20,hotspots_npp)

# Read in hotspots 
hotspots <- read_csv("data/hotspots.csv")

# saved function to convert the milliseconds from UTC 
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

### SECTION 4. Read in and reshape Federal fire data into points and polygons. ###

# Read in federal fire perimeters and select relevant columns
nfis_perimeters <- st_read("data/active_perimeters.geojson") %>% 
  select(attr_UniqueFireIdentifier, poly_IncidentName, attr_POOState, attr_POOCounty, 
         attr_FireDiscoveryDateTime, poly_DateCurrent, attr_IncidentSize,attr_PercentContained,
         attr_IncidentTypeCategory, attr_FireBehaviorGeneral, attr_FireCause,
         attr_InitialLatitude,attr_InitialLongitude) %>% 
  rename(fed_fire_id = attr_UniqueFireIdentifier, 
         name = poly_IncidentName, 
         state = attr_POOState,
         county = attr_POOCounty,
         started = attr_FireDiscoveryDateTime, 
         updated = poly_DateCurrent, 
         acres_burned = attr_IncidentSize,
         percent_contained = attr_PercentContained,
         type = attr_IncidentTypeCategory,
         fire_behavior = attr_FireBehaviorGeneral, 
         fire_cause = attr_FireCause,
         latitude = attr_InitialLatitude,
         longitude = attr_InitialLongitude)

# Convert milliseconds to dates and clean numeric fields
fed_fires <- nfis_perimeters %>% 
  st_drop_geometry() %>%
  mutate(source="NFIS") %>%
  mutate_at(vars(started, updated), ms_to_date, t0 = "1970-01-01", timezone = "America/Los_Angeles") %>% 
  mutate(days_burning = floor(difftime(Sys.time(), started, units = "days")), 
         days_sinceupdate = round(difftime(Sys.time(), updated, units = "days"), 1)) %>% 
  filter(acres_burned > 99 & days_sinceupdate < 120 | days_sinceupdate < 120)

# Standardize fire name and state columns
fed_fires <- fed_fires %>% 
  mutate(name = str_to_title(name),
         name = paste0(name, " Fire"),
         name = gsub("  ", " ", name),
         name = trimws(name),
         state = gsub("US-", "", state))

# Select relevant columns and filter to only include fires that remain in points file
nfis_perimeters <- nfis_perimeters %>% 
  select(name, fed_fire_id, geometry) %>% 
  filter(fed_fire_id %in% fed_fires$fed_fire_id)

# Standardize fire name column
nfis_perimeters$name <- str_to_title(nfis_perimeters$name)
nfis_perimeters$name <- paste0(nfis_perimeters$name, " Fire")

# Leave in, but comment out until/unless needed for MANUAL FIRE EDITS
# Manual fixes of a couple fires with mistaken geocoordinates for point origin
# fed_fires$latitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", 35.69468,fed_fires$latitude)
# fed_fires$longitude <- ifelse(fed_fires$fed_fire_id=="2022-NMSNF-000027", -105.335,fed_fires$longitude)

### SECTION 5. Read in and reshape California fire data. ###

# Load/read in Calfire geojson; transform to sf format
calfire_activefires <- st_read("data/calfire_activefires.geojson")

# Simplify, standardize version of California Fires from CalFire's active list
try(
  cal_fires <- calfire_activefires %>%
  mutate(state="CA") %>%
  select(1,25,7,8,15,14,13,4,3,9,10,17) %>%
  st_drop_geometry() %>%
  mutate(source="Cal Fire")
)
try(
names(cal_fires) <- c("name", "state", "county", 
                      "location", "type", "latitude", "longitude", 
                      "started", "updated", "acres_burned", "percent_contained",
                      "info_url","source")
)

# clean numeric fields
try(
cal_fires$acres_burned <- round(as.numeric(cal_fires$acres_burned),0)
)
try(
cal_fires$percent_contained <- as.numeric(cal_fires$percent_contained)
)
# calculating fields for time passed elements in popups and for filtering old fires
try(
cal_fires$days_burning <- floor(difftime(Sys.Date(),cal_fires$started, units="days"))+1
)
try(
cal_fires$days_sinceupdate <- floor(difftime(Sys.Date(),cal_fires$updated, units="days"))+1
)
# OPEN WORK: Verify and solve the time zones for the math for
# both the California and federal files' time stamps
try(
cal_fires$name <- trimws(cal_fires$name)
)
# filter out small fires and old fires not updated for more than a week
# except for leaving in very new fires
try(
  cal_fires <- cal_fires %>%
  filter(acres_burned>99 & days_sinceupdate<8 |
           days_sinceupdate<3)
)

### SECTION 6. Merge federal and California fire points. ###

# Temporarily reduce California file
try(
  cal_fires_unique <- cal_fires %>%
  filter(!cal_fires$name %in% fed_fires$name)
)

# Combine into one file and clean up
fires <- fed_fires
try(fires <- bind_rows(fires,cal_fires_unique))
try(rm(cal_fires_unique))
# Add full state name; pulling states from R built in reference data
states <- as.data.frame(cbind(state.abb,state.name)) %>% janitor::clean_names()
fires <- left_join(fires,states,by=c("state"="state_abb"))

# Save latest merged fire points file as csv
write_csv(fires,"data/wildfires_working.csv")

# set values for dynamic zoom in feature in map
# max_lat <- fires$latitude[which.max(fires$acres_burned)]
# max_lon <- fires$longitude[which.max(fires$acres_burned)]
top_states <- fires %>%
  group_by(state_name) %>%
  summarise(acres=sum(acres_burned,na.rm = TRUE),count=n()) %>%
  arrange(desc(acres)) %>%
  head(1)
top_calfires <- fires %>%
  filter(state=="CA") %>%
  arrange(desc(acres_burned)) %>% st_drop_geometry()

# remove fires without lat longs yet so they can be mapped
# validation shows these are tiny almost all <1ac and all <10ac
fires <- fires %>% filter(!is.na(latitude) & !is.na(longitude))
# Create flag for active vs. not for map icons
fires$active <- if_else(fires$days_sinceupdate<4,"Yes","No")
# Create variables for header
fires_count <- fires %>% st_drop_geometry() %>% count()
fires_topstate <- top_states$state_name
fires_topstatecount <- top_states$count
fires_topstateacres <- prettyNum(top_states$acres,big.mark=",")



# transform to properly projected spatial points data
fires <- st_as_sf(fires, coords = c("longitude", "latitude"), 
                  crs = 4326)


### SECTION 7. Script popups, buttons and icons ###

fireLabel <- paste(sep = "",
                   paste("<font size='3'><b>",fires$name,"</font size></b><hr style='margin-top:0px; margin-bottom:0px;'><font size='2'>",fires$county," County<b>,",fires$state_name,"</b>"),
                   paste("<hr style='margin-top:0px; margin-bottom:0px;'>Burning for ",ifelse(fires$days_burning<2,"about <b>1</b> day",paste(sep="","<b>",fires$days_burning,"</b> days"))),
                   paste("<hr style='margin-top:0px; margin-bottom:0px;'><b>",prettyNum(fires$acres_burned,big.mark=","),"</b> acres burned"),
                   paste("<hr style='margin-top:0px; margin-bottom:0px;'><b>",ifelse(is.na(fires$percent_contained),"</b>Percent contained not available",paste(sep="",fires$percent_contained,"</b>","% contained"))),
                   paste("<hr style='margin-top:0px; margin-bottom:2px;'>"),
                   paste("<i>Updated ", paste(as.character(as.POSIXct(fires$updated, format = "%Y-%m-%d %H:%M"), format = "%b %d at %I:%M %p")),"</font size>")
)

# Create temporary perimeter label
perimeterLabel <- paste(nfis_perimeters$name)

# Create the fire icons
fireIcons <- awesomeIcons(
  icon = "fire",
  iconColor = "white",
  library = 'glyphicon',
  squareMarker = TRUE,
  markerColor = "orange")
# options include ion-flame, ion-fireball, fa-fire

# Set values for EasyButtonBar controls here
fire_button <- "glyphicon-fire"
fire_buttontitle <- "Active wildfires"
hotspot_button <- "glyphicon-certificate"
hotspot_buttontitle <- "Satellite-detected hot spots"
aq_button <- "glyphicon-scale"
aq_buttontitle <- "Air Quality Index"
smoke_button <- "fa-cloud"
smoke_buttontitle <- "Wildfire smoke levels"
forecast_button <- "glyphicon-flag"
forecast_buttontitle <- "Fire Danger Forecast"

### SECTION 8. Script color palettes for maps. ###

# Create color palette for air quality
airpal <- colorFactor(palette = c("#b1dbad", "#ffffb8", "#ffcc80","#ff8280","#957aa3","#a18f7f"), levels = c("1", "2", "3", "4","5","6"), na.color = "#ff8280")
# Create color palette smokepal for the varying levels of intensity of smoke
smokepal <- colorFactor(palette = c("#99a0a5", "#51585f", "#2d343a"), levels = c("Light","Medium","Heavy"))
# Create color palette for air quality
riskpal <- colorFactor(palette = c("#006400", "green", "yellow","orange","red"), levels = c("L", "M", "H", "V","E"), na.color = "#ff8280")

# SECTION 9. Script a base wildfire map.

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    left: 0.5%;
    top: 0.8%;
    text-align: left;
    background-color: rgba(255, 255, 255, 0);
    width: 90%;
    border-radius: 4px 4px 4px 4px;
  }
  .leaflet-control.map-title .headline{
    font-weight: bold;
    font-size: 28px;
    color: white;
    padding: 0px 5px;
    background-color: #F98C00;
    background: linear-gradient(90deg, rgba(190,0,0,1) 0%, rgba(249,140,0,1) 43%, rgba(255,186,0,1) 90%, rgba(255,186,0,0) 100%);
    border-radius: 4px 4px 0px 0px;
  }
  .leaflet-control.map-title .subheadline {
    font-size: 14px;
    color: black;
    padding: 5px 30px 5px 10px;
    background: linear-gradient(90deg, rgba(255,255,255,1) 90%, rgba(255,255,255,0) 100%);
    border-radius: 0px 0px 4px 4px;
  }
  .leaflet-control.map-title .subheadline a {
    color: #BE0000;
    font-weight: bold;
  }
  
  @media only screen and (max-width: 550px) {
    .leaflet-control.map-title .headline {
      font-size: 20px;
    border-radius: 4px 4px 0px 0px;
    }
    .leaflet-control.map-title .subheadline {
      font-size: 10px;
    border-radius: 0px 0px 4px 4px;
    }
  @media only screen and (max-width: 420px) {
    .leaflet-control.map-title .headline {
      font-size: 18px;
    border-radius: 4px 4px 0px 0px;
    }
    .leaflet-control.map-title .subheadline {
      font-size: 9px;
    border-radius: 0px 0px 4px 4px;
    }
"))

headerhtml <- tags$div(
  tag.map.title, HTML(paste(sep="",
  "<div class='headline'>Wildfire Tracker</div>
  <div class='subheadline'>ABC News is tracking data about ",fires_count," wildfires nationwide. 
  The most active state is ",
  fires_topstate,", with ",
  fires_topstatecount," fires that have burned ",
  fires_topstateacres," acres.
  Click on a fire for live status details. The buttons below add or remove more data about air quality, smoke and fire risk forecast. <div>")
  )
)

caliheaderhtml <- tags$div(
  tag.map.title, HTML(paste(sep="",
                            "<div class='headline'>Wildfire Tracker</div>
  <div class='subheadline'>We're tracking ",count(top_calfires)," wildfires statewide. ",  
                       #     The largest is the <a href='https://abcotvdata.github.io/wildfire_tracker/largest_calfire_map.html'>",
                        #    top_calfires[1,1],"</a>, burning ",
                        #    prettyNum(round(top_calfires[1,10],0),big.mark=",")," acres. 
                            "Click on a fire for live status details. The buttons below add or remove data about air quality, smoke and the fire risk forecast.<div>")
  )
)

hawaiiheaderhtml <- tags$div(
  tag.map.title, HTML(paste(sep="",
                            "<div class='headline'>Hawaii Wildfire Tracker</div>
  <div class='subheadline'>We're tracking wildfire hot spots detected by U.S. satellites on the islands of Hawaii. ",  
                            "The buttons below add or remove data about air quality, smoke and the fire risk forecast, and allow you to zoom in and out.<div>")
  )
)


# New wildfire base map include fires, smoke and hotspots
base_map <- leaflet(hotspots, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(-116, 43.5, zoom = 5) %>% 
#  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
#  addProviderTiles(providers$Stamen.TonerLines) %>%
#  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircleMarkers(radius = 2.5,
                   color = "#be0000",
                   weight = 1,
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   group="Hot spots") %>%
  addPolygons(data = nfis_perimeters, 
              color = "#00318b",
              popup = perimeterLabel,
              weight = 1.5,
              group="Wildfires") %>%
  addAwesomeMarkers(data = fires,
                    popup = fireLabel,
                    popupOptions = popupOptions(keepInView = T, 
                                                autoPanPaddingTopLeft=c(100,120)),
                    icon = fireIcons,
                    group="Wildfires") %>%
  addPolygons(data = noaa_latest_smoke, 
              color = ~smokepal(Density),
              fillOpacity = 0.6,
              weight = 0,
              group="Fire smoke") %>%
  addPolygons(data = air_quality, 
              color = ~airpal(gridcode),
              weight = 0,
              fillOpacity = 0.6,
              group = "Air quality") %>%
  addCircleMarkers(data = usfs_forecast,
                   radius = 5,
                   color = ~riskpal(fdc_adj),
                   weight = 1,
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   group="Fire forecast") %>%
  addLegend(values = values(air_quality$gridcode), title = "Air Quality Index<br><a href='https://www.airnow.gov/aqi/aqi-basics/' target='blank'><small>What AQI ratings mean</a>", 
            group = "Air quality", 
            colors = c("#b1dbad", "#ffffb8", "#ffcc80","#ff8280","#957aa3","#a18f7f","#dde4f0"),
            labels=c("Good", "Moderate", "Unhealthy/Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous","No AQ Data"),
            position = 'bottomright') %>%
  addLegend(values = values(usfs_forecast$fdc_adj), title = "Wildland Fire Danger Rating<br><a href='https://www.wfas.net/index.php/fire-danger-rating-fire-potential--danger-32/class-rating-fire-potential-danger-51?task=view' target='blank'><small>More detailed on these risk ratings</a>", 
            group = "Fire forecast", 
            colors = c("#006400", "green", "yellow","orange","red"),
            labels=c("Low", "Moderate", "High", "Very High", "Extreme"),
            position = 'bottomright') %>%
  addLayersControl(
    overlayGroups = c("Wildfires","Hot spots","Fire smoke","Air quality","Fire forecast"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomleft') %>% hideGroup(c("Fire smoke","Air quality","Fire forecast"))
# base_map

### SECTION 10. Script national map + variant(s). ###

# National wildfire map include fires, smoke and hotspots
# Adding the customized national bounding box, map header and EasyButtonsBar in order
# Which must be added separately 
fed_fires2 <- fed_fires %>% filter(state!="AK")
wildfire_map <- base_map %>% 
  addControl(position = "topleft", html = headerhtml, className="map-title") %>%
  fitBounds(lng1 = min(fed_fires2$longitude,na.rm = TRUE) - 12, 
            lat1 = min(fed_fires2$latitude,na.rm = TRUE), 
            lng2 = max(fed_fires2$longitude,na.rm = TRUE) - 5, 
            lat2 = max(fed_fires2$latitude,na.rm = TRUE) + 2) %>% 
  addEasyButtonBar(easyButton(icon = fire_button, title = fire_buttontitle,
                              onClick = JS("function(btn, map) {
                     
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[0].click();

                }")), 
                   easyButton(icon = hotspot_button, title = hotspot_buttontitle,
                              onClick = JS("function(btn, map) {

                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[1].click();

                }")),
                   
                   easyButton(icon = smoke_button, title = smoke_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[2].click();
                              }")),
                   
                   easyButton(icon = aq_button, title = aq_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[3].click();
                              }")),
                   
                   easyButton(icon = forecast_button, title = forecast_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[4].click();

                }"))) %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft'}).addTo(this)
    }") %>%
  htmlwidgets::onRender("
    function(el, x) {
        document.getElementsByClassName('leaflet-control-layers')[0].style.display = 'none';
    }")
#wildfire_map

# Create customized versions zoomed to center of states with frequent fires
# to create a nav ability from the header to zoom to states with most activity
#idaho_map <- wildfire_map %>% setView(-114.4, 45.3, zoom = 6)
#colorado_map <- wildfire_map %>% setView(-105.3, 39, zoom = 6)
#arizona_map <- wildfire_map %>% setView(-111.5, 34.4, zoom = 6)
#nevada_map <- wildfire_map %>% setView(-117.22, 39.87, zoom = 6)
#oregon_map <- wildfire_map %>% setView(-120.5, 44, zoom = 6)
#washington_map <- wildfire_map %>% setView(-120.74, 47.75, zoom = 6)
#montana_map <- wildfire_map %>% setView(-112, 46.96, zoom = 6)
#utah_map <- wildfire_map %>% setView(-111.95, 39.41, zoom = 6)
#newmexico_map <- wildfire_map %>% setView(-106.01, 34.3, zoom = 6)
#texas_map <- wildfire_map %>% setView(-99, 31, zoom = 6)
#wyoming_map <- wildfire_map %>% setView(-107.29, 43.07, zoom = 6)

### SECTION 11. Script California map + variant(s). ###

california_map <- base_map %>%
  addControl(position = "topleft", html = caliheaderhtml, className="map-title") %>%
  setView(-122.5, 37.5, zoom = 6) %>%
  addEasyButtonBar(easyButton(icon = fire_button, title = fire_buttontitle,
                              onClick = JS("function(btn, map) {
                     
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[0].click();

                }")), 
                   easyButton(icon = hotspot_button, title = hotspot_buttontitle,
                              onClick = JS("function(btn, map) {

                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[1].click();

                }")),
                   
                   easyButton(icon = smoke_button, title = smoke_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[2].click();
                              }")),
                   
                   easyButton(icon = aq_button, title = aq_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[3].click();
                              }")),
                   
                   easyButton(icon = forecast_button, title = forecast_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[4].click();

                }"))) %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft'}).addTo(this)
    }") %>%
  htmlwidgets::onRender("
    function(el, x) {
        document.getElementsByClassName('leaflet-control-layers')[0].style.display = 'none';
    }")

### SECTION 11. Script California map + variant(s). ###

hawaii_map <- base_map %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addControl(position = "topleft", html = hawaiiheaderhtml, className="map-title") %>%
  setView(-156.4, 20.798, zoom = 10) %>%
  addEasyButtonBar(easyButton(icon = fire_button, title = fire_buttontitle,
                              onClick = JS("function(btn, map) {
                     
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[0].click();

                }")), 
                   easyButton(icon = hotspot_button, title = hotspot_buttontitle,
                              onClick = JS("function(btn, map) {

                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[1].click();

                }")),
                   
                   easyButton(icon = smoke_button, title = smoke_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[2].click();
                              }")),
                   
                   easyButton(icon = aq_button, title = aq_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[3].click();
                              }")),
                   
                   easyButton(icon = forecast_button, title = forecast_buttontitle,
                              onClick = JS("function(btn, map) {
                              
                             let layerControlElement = document.getElementsByClassName('leaflet-control-layers')[0];
                             layerControlElement.getElementsByTagName('input')[4].click();

                }"))) %>% 
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft'}).addTo(this)
    }") %>%
  htmlwidgets::onRender("
    function(el, x) {
        document.getElementsByClassName('leaflet-control-layers')[0].style.display = 'none';
    }")


#largest_calfire_map <- california_map %>%
#  setView(top_calfires[1,7], top_calfires[1,6], zoom = 10)

# Create customized versions zoomed to our stations' regions of the state
bayarea_map <- california_map %>% fitBounds(-123.5,36,-120.5,41)
fresno_map <- california_map %>% fitBounds(-121.1052,36.1837,-118.4987,37.5551)
socal_map <- california_map %>% fitBounds(-120.8358,32.5566,-114.5195,35.5286)

### SECTION 12. Write all leaflet maps to html. ###
saveWidget(california_map, 'docs/california_map.html', title = "ABC Owned Television Stations California Wildfire Tracker")
saveWidget(wildfire_map, 'docs/wildfire_map.html', title = "ABC Owned Television Stations and ABC News U.S. Wildfire Tracker")
# saveWidget(largest_calfire_map, 'docs/largest_calfire_map.html', title = "ABC Owned Television Stations and ABC News U.S. Wildfire Tracker")

saveWidget(bayarea_map, 'docs/bayarea_map.html', title = "ABC7 Bay Area Wildfire Tracker")
saveWidget(fresno_map, 'docs/fresno_map.html', title = "ABC30 Central Valley Wildfire Tracker")
saveWidget(socal_map, 'docs/socal_map.html', title = "ABC7 Southern California Wildfire Tracker")

saveWidget(hawaii_map, 'docs/hawaii_map.html', title = "ABC Owned Television Stations Hawaii Wildfire Tracker")

#saveWidget(idaho_map, 'docs/idaho_map.html', title = "ABC Owned Television Stations and ABC News Idaho Wildfire Tracker", selfcontained = TRUE)
#saveWidget(colorado_map, 'docs/colorado_map.html', title = "ABC Owned Television Stations and ABC News Colorado Wildfire Tracker", selfcontained = TRUE)
#saveWidget(nevada_map, 'docs/nevada_map.html', title = "ABC Owned Television Stations and ABC News Nevada Wildfire Tracker", selfcontained = TRUE)
#saveWidget(arizona_map, 'docs/arizona_map.html', title = "ABC Owned Television Stations and ABC News Arizona Wildfire Tracker", selfcontained = TRUE)
#saveWidget(newmexico_map, 'docs/newmexico_map.html', title = "ABC Owned Television Stations and ABC News New Mexico Wildfire Tracker", selfcontained = TRUE)
# saveWidget(oregon_map, 'docs/oregon_map.html', title = "ABC Owned Television Stations and ABC News Oregon Wildfire Tracker", selfcontained = TRUE)
#saveWidget(washington_map, 'docs/washington_map.html', title = "ABC Owned Television Stations and ABC News Wyoming Wildfire Tracker", selfcontained = TRUE)
#saveWidget(wyoming_map, 'docs/wyoming_map.html', title = "ABC Owned Television Stations and ABC News Wyoming Wildfire Tracker", selfcontained = TRUE)
#saveWidget(utah_map, 'docs/utah_map.html', title = "ABC Owned Television Stations and ABC News Utah Wildfire Tracker", selfcontained = TRUE)
#saveWidget(montana_map, 'docs/montana_map.html', title = "ABC Owned Television Stations and ABC News Montana Wildfire Tracker", selfcontained = TRUE)
#saveWidget(texas_map, 'docs/texas_map.html', title = "ABC13 Texas Wildfire Tracker", selfcontained = TRUE)
