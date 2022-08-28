# library(tidyverse)
# library(dplyr)
# library(readr)
# library(stringr)
# library(sf)

# Every morning, by 1130UTC/730EDST, NOAA moves the latest
# fire hot spot and wildfire smoke files from satellites
# We are setting a script to fetch those every morning

# Formulate url for today's fire file
# There is a WFS version with simpler URL but moves hours later
noaafireurl <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Shapefile/",
                     format(Sys.Date(), "%Y"),"/",
                     format(Sys.Date(), "%m"),"/",
                     "hms_fire",format(Sys.Date(), "%Y%m%d"),".zip")
# Tries to download the file; if not there, script won't fail
try(download.file(noaafireurl,"data/satellite/fire/noaa_latest_fires.zip"))
unzip("data/satellite/fire/noaa_latest_fires.zip", exdir = "data/satellite/fire/")
# noaa_latest_fires <- st_read(paste(sep="","data/satellite/fire/","hms_fire",format(Sys.Date(), "%Y%m%d"),".shp"))

# Rename the file to simplify map-building script
try(file.rename(paste(sep="",
                    "data/satellite/fire/",
                    "hms_fire",
                    format(Sys.Date(), "%Y%m%d"),
                    ".shp"), "data/satellite/fire/noaa_latest_fire.shp"))
try(file.rename(paste(sep="",
                    "data/satellite/fire/",
                    "hms_fire",
                    format(Sys.Date(), "%Y%m%d"),
                    ".shx"), "data/satellite/fire/noaa_latest_fire.shx"))
try(file.rename(paste(sep="",
                    "data/satellite/fire/",
                    "hms_fire",
                    format(Sys.Date(), "%Y%m%d"),
                    ".prj"), "data/satellite/fire/noaa_latest_fire.prj"))
try(file.rename(paste(sep="",
                    "data/satellite/fire/",
                    "hms_fire",
                    format(Sys.Date(), "%Y%m%d"),
                    ".dbf"), "data/satellite/fire/noaa_latest_fire.dbf"))



# Formulate url for today's smoke file
smokeurl <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                  format(Sys.Date(), "%Y"),"/",
                  format(Sys.Date(), "%m"),"/",
                  "hms_smoke",format(Sys.Date(), "%Y%m%d"),".zip")
try(download.file(smokeurl,"data/satellite/smoke/noaa_latest_smoke.zip"))
unzip("data/satellite/smoke/noaa_latest_smoke.zip", exdir = "data/satellite/smoke/")
# latest_smoke <- st_read(paste(sep="","data/satellite/smoke/","hms_smoke",format(Sys.Date(), "%Y%m%d"),".shp"))

# Rename the smoke file to simplify map-building script
try(file.rename(paste(sep="",
                      "data/satellite/smoke/",
                      "hms_smoke",
                      format(Sys.Date(), "%Y%m%d"),
                      ".shp"), "data/satellite/smoke/noaa_latest_smoke.shp"))
try(file.rename(paste(sep="",
                      "data/satellite/smoke/",
                      "hms_smoke",
                      format(Sys.Date(), "%Y%m%d"),
                      ".shx"), "data/satellite/smoke/noaa_latest_smoke.shx"))
try(file.rename(paste(sep="",
                      "data/satellite/smoke/",
                      "hms_smoke",
                      format(Sys.Date(), "%Y%m%d"),
                      ".prj"), "data/satellite/smoke/noaa_latest_smoke.prj"))
try(file.rename(paste(sep="",
                      "data/satellite/smoke/",
                      "hms_smoke",
                      format(Sys.Date(), "%Y%m%d"),
                      ".dbf"), "data/satellite/smoke/noaa_latest_smoke.dbf"))
