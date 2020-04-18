### GPS.R


### SET UP ENVIRONMENT ###

library(dplyr)
library(lubridate)
library(ggmap)

# Source required files
source("GPS-R-API-key.R")

# Set Google API Key, enabled for Maps Static API
register_google(key = get_google_key())
has_google_key()


### READ AND CLEAN DATA ###

# Set csv data column classes to avoid data loss (e.g. non-delimited time parsed as numeric)
csv_column_classes = c("character", "character", "factor", "character", "factor", "character", "factor", "numeric", "numeric",
               "character", "numeric", "factor", "character")

# Read csv data
rawdata <- read.csv("Västerby.csv", colClasses = csv_column_classes)

# Set variable names
colnames(rawdata) <- c("gprmc", "utc", "status", "lat1", "lat2", "lon1", "lon2", "speed", "track",
                       "date", "mag_var1", "mag_var2", "checksum")

# Convert times and dates to correct data types
rawdata <- rawdata %>%
  mutate(date = dmy(date), utc = as.times(paste(substr(utc, 1, 2),':', substr(utc, 3, 4),':',substr(utc, 5, 6), sep = "")))

# Convert position data to numeric
rawdata <- rawdata %>%
  mutate(lat = as.numeric(sp::char2dms(paste(substr(lat1, 1, 2),"d", substr(lat1, 3, nchar(lat1)),"'", lat2, sep = "")))) %>%
  mutate(lon = as.numeric(sp::char2dms(paste(substr(lon1, 1, 3),"d", substr(lon1, 4, nchar(lon1)),"'", lon2, sep = ""))))


str(rawdata)
rawdata



### PLOT POSITION DATA ON MAP ###


# Set start location
myLocation <- c(lon = rawdata$lon[1], lat = rawdata$lat[1])

# Set up map
myMap <- get_map(myLocation,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 16)
# Plot map
map_plot <- ggmap(myMap)
map_plot

# Save map as png image
ggsave(filename = "map_plot.png", map_plot,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')




# Resources
# https://developers.google.com/maps/documentation/javascript/get-api-key
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://www.jessesadler.com/post/geocoding-with-r/ 