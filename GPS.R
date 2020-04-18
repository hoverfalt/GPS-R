### GPS.R - Playground for manipulating and plotting GPS data


##########################
### SET UP ENVIRONMENT ###
##########################

# Remove all objects from workspace
rm(list = ls())

# Load required libraries
library(dplyr)
library(lubridate)
library(ggmap)
library(chron)
library(geosphere)

# Source required files
source("GPS-R-API-key.R")

# Set Google API Key (Maps Static API)
register_google(key = get_google_key())
has_google_key()


###########################
### READ AND CLEAN DATA ###
###########################

# Set csv data column classes to avoid data loss (e.g. non-delimited time parsed as numeric)
csv_column_classes = c("character", "character", "factor", "character", "factor", "character", "factor", "numeric", "numeric",
               "character", "numeric", "factor", "character")

# Read csv data
gpsdata <- read.csv("Västerby.csv", colClasses = csv_column_classes)

# Set variable names
colnames(gpsdata) <- c("gprmc", "utc", "status", "lat1", "lat2", "lon1", "lon2", "speed", "track",
                       "date", "mag_var1", "mag_var2", "checksum")

# Convert times and dates to correct data types
gpsdata <- gpsdata %>%
  mutate(date = dmy(date), utc = as.times(paste(substr(utc, 1, 2),':', substr(utc, 3, 4),':',substr(utc, 5, 6), sep = "")))

# Combine date and time into POSIXct (datetime)
gpsdata$timestamp <- as.POSIXct(paste(gpsdata$date, gpsdata$utc), origin = "1970-01-01")

# Convert position data to numeric
gpsdata <- gpsdata %>%
  mutate(lat = as.numeric(sp::char2dms(paste(substr(lat1, 1, 2),"d", substr(lat1, 3, nchar(lat1)),"'", lat2, sep = "")))) %>%
  mutate(lon = as.numeric(sp::char2dms(paste(substr(lon1, 1, 3),"d", substr(lon1, 4, nchar(lon1)),"'", lon2, sep = ""))))

# Drop redundant variables
gpsdata <- gpsdata %>% select(-gprmc, -lat1, -lat2, -lon1, -lon2, -date, -utc)

# Rearrange variables
gpsdata <- gpsdata[c("timestamp", "status", "lat", "lon", "speed", "track", "mag_var1", "mag_var2", "checksum")]

# Show structure of rawdata
str(gpsdata)
head(gpsdata, 10)


#################################
### PLOT POSITION DATA ON MAP ###
#################################

# Calculate the center location of the data points
centerLocation <- c(lon = mean(c(min(gpsdata$lon), max(gpsdata$lon))),
                    lat = mean(c(min(gpsdata$lat), max(gpsdata$lat))))

# Set up map (TO-DO: the zoom level should be set to fit the data)
map <- get_googlemap(center = centerLocation,
                     maptype = "terrain",
                     zoom = 13)

# Setup plot - points
map_plot <- ggmap(map) +
  geom_point(data = gpsdata, aes(x = lon, y = lat, color = speed), size = 1, lineend = "round") +
  scale_colour_gradient(low = "green", high = "red")

# Setup plot - path
map_plot <- ggmap(map) +
  geom_path(data = gpsdata, aes(x = lon, y = lat, color = speed), size = 1, lineend = "round") +
  scale_colour_gradient(low = "green", high = "red")

# Plot map
map_plot

# Save map as png image
ggsave(filename = "Plots/map_plot.png", map_plot,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')


###########################
### MANIPULTE GPS DATA  ###
###########################

# Function to calculate the distance between two data points
calculate_distance <- function(lon1, lat1, lon2, lat2){
  return(distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine))
}

# Calculate distance between data ponts n and n-1
gpsdata <- gpsdata %>%
  mutate(lat_prev = lag(lat), lon_prev = lag(lon)) %>%
  mutate(dist = purrr::pmap_dbl(list(lon, lat, lon_prev, lat_prev), calculate_distance)) %>%
  select(-lat_prev, -lon_prev)

# Set first distance data point from NA to 0
gpsdata$dist[1] <- 0

# Calculate cumulative distance
gpsdata <- gpsdata %>% mutate(cum_dist = cumsum(dist))

# Print toal distance
sum(gpsdata$dist)
gpsdata$cum_dist[nrow(gpsdata)]

# Plot cumulative distance over time
p <- ggplot(gpsdata, aes(x = timestamp, y = cum_dist)) +
  geom_line() +
  scale_x_datetime() +
  labs(x = "Time", y = "Distance (m)")
p

# Save plot as png
ggsave(filename = "Plots/distance_over_time.png", p,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')




# Resources
# https://developers.google.com/maps/documentation/javascript/get-api-key
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://www.jessesadler.com/post/geocoding-with-r/ 

