## NEED TO STREAMLINE THIS TEST-and-DOWNLOAD METHOD
# create a pair of urls based on yesterday's date and today's
# idea is to make sure we have the latest
hotspot_url2 <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/",
                  format(Sys.Date(), "%Y"),"/",
                  format(Sys.Date(), "%m"),"/",
                  "hms_fire",format(Sys.Date()-1, "%Y%m%d"),".txt")
hotspot_url1 <- paste(sep="","https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/",
                 format(Sys.Date(), "%Y"),"/",
                 format(Sys.Date(), "%m"),"/",
                 "hms_fire",format(Sys.Date(), "%Y%m%d"),".txt")

## download the yesterday file and then download the today file
# which will replace if today file exists and won't if not
download.file(hotspot_url2,"hotspots.csv")
download.file(hotspot_url1,"hotspots.csv")
