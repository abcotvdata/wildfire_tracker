library(lubridate)
# Every morning, by 1130UTC/730EDST, NOAA moves the latest
# wildfire smoke files from satellites
# We are setting a script to fetch every morning
# Fire file is below, not used anymore; saving code temp

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

# create a pair of urls based on yesterday's date and today's
# idea is to make sure we have the latest
today <- Sys.Date()
yesterday <- today - 1
hotspot_url2 <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/",
                      year(yesterday),"/",
                      month(yesterday),"/",
                      "hms_fire",format(yesterday, "%Y%m%d"),".txt")
hotspot_url1 <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/",
                      year(today),"/",
                      month(today),"/",
                      "hms_fire",format(today, "%Y%m%d"),".txt")

## download the yesterday file and then download the today file
# which will replace if today file exists and won't if not
result <- try(download.file(hotspot_url1,"data/hotspots.csv"))
if (inherits(result, "try-error")) {
  try(download.file(hotspot_url2,"data/hotspots.csv"))
}
