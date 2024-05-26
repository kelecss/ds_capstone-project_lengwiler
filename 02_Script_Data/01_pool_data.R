# Script Name: pool_data_01.R
# Description: This script taps into the Zurich Open Data API to extract data on the status of all public outdoor pools in Zurich.
# Author: Kerim Lengwiler
# Date Created: 2024_05_04
# Date Updated: 2024_05_04



packages <- c("shiny", "httr", "XML", "bslib", "leaflet", "tidyverse", "tidygeocoder","sf")
sapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

url <- "https://www.stadt-zuerich.ch/stzh/bathdatadownload"

# Make the request
pool_response <- GET(url = url, query = list()) 
http_status(pool_response)

xml_data <- xmlParse(rawToChar(pool_response$content))

pool <- getNodeSet(xml_data, "//bath")

# Extract data for each bath
pool_details <- lapply(pool, function(pool) {
  title <- xmlValue(getNodeSet(pool, "./title")[[1]])
  temperatureWater <- xmlValue(getNodeSet(pool, "./temperatureWater")[[1]])
  poiid <- xmlValue(getNodeSet(pool, "./poiid")[[1]])
  dateModified <- xmlValue(getNodeSet(pool, "./dateModified")[[1]])
  openClosedTextPlain <- xmlValue(getNodeSet(pool, "./openClosedTextPlain")[[1]])
  urlPage <- xmlValue(getNodeSet(pool, "./urlPage")[[1]])
  pathPage <- xmlValue(getNodeSet(pool, "./pathPage")[[1]])
  
  # Return a list of bath details
  list(Title = title, Wassertemperatur = temperatureWater, ID = poiid, 
       Update = dateModified, Status = openClosedTextPlain, 
       URL_Page = urlPage, Path_Page = pathPage)
})

# Convert the list to a data frame for easier manipulation and viewing
df_pools <- do.call(rbind.data.frame, pool_details)


## Getting the Locations
df_pools$Address <- NA

pool_addresses <- c("Werdinsel 2, 8049 Zürich", "Lettensteg 10, 8037 Zürich", "Wasserwerkstrasse 141, 8037 Zürich", "Wasserwerkstrasse 142, 8037 Zürich",
                   "Stadthausquai 12, 8001 Zürich", "Ringstrasse 79,8057 Zürich", "Luegislandstrasse 160, 8051 Zürich", "Adlisbergstrasse 36, 8044 Zürich",
                   "Wasserschöpfi 71, 8055 Zürich", "Edelweissstrasse 5, 8048 Zürich", "Glatttalstrasse 43, 8052 Zürich", "Zwischen den Hölzern, 8102 Oberengstringen",
                   "Limmattalstrasse 154, 8049 Zürich", "Hofstrasse 56, 8032 Zürich", "Sihlstrasse 71, 8001 Zürich", "Klebestrasse 3, 8041 Zürich",
                   "Wallisellenstrasse 100, 8050 Zürich", "Badweg 10, 8001 Zürich", "Mythenquai 9, 8002 Zürich", "Katzenseestrasse, 8046 Zürich",
                   "Utoquai 50, 8008 Zürich", "Mythenquai 95, 8002 Zürich", "Bellerivestrasse 200, 8008 Zürich", "Seestrasse 451, 8038 Zürich",
                   "Emil-Klöti-Strasse 17, 8037 Zürich") 
 
df_pools$Address <- pool_addresses

df_pools_coord <- geocode(df_pools, address = "Address", lat = "Latitude", lon = "Longitude", method = "osm")

# Merge data
df_pools$Latitude <- NA
df_pools$Longitude <- NA

df_pools$Latitude <- df_pools_coord$Latitude
df_pools$Longitude <- df_pools_coord$Longitude


# Print the data frame
View(df_pools)


