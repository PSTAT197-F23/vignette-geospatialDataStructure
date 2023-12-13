# Load in libraries
library(sf)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(ggmap)
library(leaflet)

# defining 2 lines
lnstr_sfg1 <- st_linestring(matrix(runif(10), ncol=2)) # a line with 10 points
lnstr_sfg2 <- st_linestring(matrix(runif(10), ncol=2)) # ncol = 2 specifies the dimension

# Combining the the lines into a dataframe
(lnstr_sfc <- st_sfc(lnstr_sfg1, lnstr_sfg2)) # just one feature here

# Creating the sf object with attributes
dfr <- data.frame(id = c("hwy1", "hwy2"),
                  cars_per_hour = c(78, 22)) 
(lnstr_sf <- st_sf(dfr , lnstr_sfc)) # 2 lines each with an attribute cars_per_hour

# ggplot on the sf object
ggplot() +
  geom_sf(data = lnstr_sf["id"]) +
  theme_minimal()

# Generating a cluster of points in 2 dimensions

a_lat <- c(72.117, 71.05, 71.717, 71.167, 71.817, 72.333, 71.25, 70.917, 
           70.85, 70.933)
a_lon <- c(33.217, 30.333, 26.3, 22.333, 22.333, 22.333, 18.517, 20.133, 
           21.333, 19.917)
b_lat <- c(41.283333, 41, 40.95, 40.95, 38.683333, 40.783333, 40.733333, 
           40.516667, 38.566667, 41.266667)
b_lon <- c(-71.116667, -70.733333, -70.583333, -70.483333, -74.816667, 
           -70.5, -70.566667, -70.266667, -74.85, -71.366667)
c_lat <- c(58.015, 57.67167, 57.33833, 56.685, 56.99333, 57.31667, 57.65, 
           57.99667, 58.32167, 58.33333)
c_lon <- c(-158.31667, -158.36, -158.41, -159.76, -159.72333, -159.66333, 
           -159.64167, -159.605, -159.54, -160.72167)
a_dat <- data.frame(lat=a_lat, lon=a_lon)
b_dat <- data.frame(lat=b_lat, lon=b_lon)
c_dat <- data.frame(lat=c_lat, lon=c_lon)
a_dat$cluster <- "a"
b_dat$cluster <- "b"
c_dat$cluster <- "c"
dat <- rbind(a_dat, b_dat, c_dat)

# Visualizing the Clusters
ggplot(dat, aes(x = lon, y = lat, color = cluster)) +
  geom_point() +
  labs(title = "Scatter Plot of Clusters",
       x = "Lon",
       y = "Lat")

# Creating the shapes for the Cluster

shapes <- dat %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by(cluster) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull()
plot(shapes)

# Loading in a vector dataframe of the states of US
us_geo <- read_csv("data/dat_spatial.csv")
us_geo <- us_geo %>% 
  filter(name != "Hawaii" & name != "Alaska")

# Creating a "sf" object for the US vector locations
SQ_example <- st_as_sf(us_geo, wkt = "geometry")
str(SQ_example)

# using the techniques above; plot the US sf object
ggplot() +
  geom_sf(data = SQ_example["name"]) +
  theme_minimal() +
  labs(title = "Map of the U.S. Using sf Polygons")

# load in our attributes data

us_species <- read_csv("data/dat_species.csv")
us_species <- us_species %>%  # filtering out everything we don't need.
  filter(year >= "1950") %>% 
  filter(group == "Mammals"| group == "Reptiles" | group == "Amphibians")

# merge the sf data with their attributes

colnames(us_geo)[1] <- "state"
us_geo_merged <- merge(us_geo,us_species,by="state", all=T)
names(us_geo_merged) 

us_geo_merged <- us_geo_merged[is.na(us_geo_merged$geometry) == FALSE, ]

# Create an sf object (SQ) from the WKT geometries
SQ <- st_as_sf(us_geo_merged, wkt = "geometry")
SQ1 <- st_as_sf(us_geo, wkt = "geometry")

# Check the structure of SQ
str(SQ)
unique(us_geo_merged$sciname)

# Filter on the merged data to find attributes about Capybaras
SQ_f <- SQ %>%
  filter(sciname == "Hydrochoerus hydrochaeris") %>% 
  filter(year <= 2015) %>% 
  group_by(state) %>% 
  summarise(occurrence = sum(occurrence))

# Create a ggplot object
ggplot() +
  # Add the filtered sf object as a layer
  geom_sf(data = SQ1["state"]) +
  geom_sf(data = SQ_f, aes(fill = occurrence)) +
  
  # Customize the plot as needed
  theme_minimal() +
  labs(title = "Occurrences Plot")

# Centroid points

# Generate some centroids
centroids <- st_make_grid(what="centers") %>% st_sf()

# Make a new grid from them
cellSize <- 10
grid <- (st_bbox(centroids) + cellSize/2*c(-1,-1,1,1)) %>%
  st_make_grid(cellsize=c(cellSize, cellSize)) %>% st_sf()

ggplot() + geom_sf(data=grid) + geom_sf(data=centroids)
