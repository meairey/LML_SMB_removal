# Replace "your_shapefile.shp" with the path to your shapefile
library(ggplot2)
library(sf)
library(geosphere)


## Set up LML shape file ---------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/LML_shape")
LML_shape <- st_read("World_Lakes.shp")

## Set up FBL shape file
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/FBL_shape")

FBL_shape <- st_read("World_Lakes.shp")



ggplot()  +
  geom_sf(data = LML_shape) + theme_minimal() + 
  geom_sf(data = FBL_shape) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot() +
  geom_sf(data = LML_shape) + theme_minimal()
  
ggplot() + 
  geom_sf(data = FBL_shape) + 
  theme(axis.text.x = element_text(angle = 90, hjust = .5)) + 
  theme_minimal()


FBL_shape$geometry



# Transform the geometry column to lat/long (EPSG:4326)
FBL_shape <- st_transform(FBL_shape, crs = 4326)
LML_shape = st_transform(LML_shape, crs = 4326)

# Extract the transformed coordinates
coords.fbl <- st_coordinates(FBL_shape$geometry)
coords.lml = st_coordinates(LML_shape$geometry)

# Convert the coordinates to a data frame
coords_df <- as.data.frame(coords.fbl) %>% 
  rbind(as.data.frame(coords.lml))
colnames(coords_df) <- c("Longitude", "Latitude")

# Plot the map with latitude and longitude of both lakes
ggplot(coords_df, aes(x = Longitude, y = Latitude)) +
  theme_minimal() +
  geom_polygon(fill = "lightblue") + 
  xlim(-74.95, -74.89)


# Plot the map
ggplot(coords_df, aes(x = Longitude, y = Latitude)) +
  geom_path() 

# Assuming your dataframe is called df and it contains X and Y columns
# Create an sf dataframe
df <- as.data.frame(coords)

df$group = "FBL_shape"
# Mark the segment between row 4 and row 5 as red
df$color <- ifelse(df$segment == 4, "red", "black")

ggplot(df, aes(x = X, y = Y, color = color, group = group)) +
  
  geom_polygon(aes(group = group),alpha = 0.3)+ 
  geom_path(aes(group = group), size = 1) +
  theme_minimal()

## Loading in FBL sites ---------------------

## Map of sites in FBL
library(tidyverse)
utm_df  = read.csv("../BEF SITES 05.19.05.csv") %>%
  select(SITE.N, HAB, E.UTM1, N.UTM1, E.UTM2, N.UTM2) %>% 
  na.omit() %>%
  filter(grepl("FBL",SITE.N)) 


# Example UTM coordinates (Zone 18N, for New York City)

utm_zone <- 18
utm_band <- "T"


# Convert the data frame to a sf object with the correct UTM CRS
utm_sf <- st_as_sf(utm_df, coords = c("E.UTM1", "N.UTM1"), crs = paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))


coords <- st_coordinates(utm_sf$geometry) %>% as.data.frame() %>%
  mutate(HAB = utm_df$HAB) %>%
  mutate(group = 'FBL')

#------------------------------------------------------------
## Lat Long Plot

coords = coords %>% mutate(X = X %% 1000000)
# Convert the UTM coordinates to an sf object
utm_sf <- st_as_sf(coords %>% select(X,Y), coords = c("X", "Y"), crs = 32618)

# Transform the UTM coordinates to latitude and longitude
latlong_sf <- st_transform(utm_sf, crs = 4326)

# Extract the latitude and longitude values
latlong_coords <- st_coordinates(latlong_sf)
latlong_df <- data.frame(
  latitude = latlong_coords[, 2],
  longitude = latlong_coords[, 1]
) %>% 
  mutate(HAB = coords$HAB) %>%
  mutate(group = "FBL")


# Print the latitude and longitude values


ggplot(latlong_df, aes(x = longitude, y = latitude, group = group, col = HAB)) + 
  geom_polygon(fill = "light blue") + 
  geom_path(size = 2, key_glyph = "rect") + 
  labs(col = "Habitat") + 
  theme_minimal()


## Habitat lengths 

setwd('C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/')
hab_length = read.csv("Data/BEFsites_LengthAndHabitat.csv") %>% filter(Water %in% c("FBL","LML")) %>%
  select(Water, SITE, SITE_N, Shape_Length, Habitat) %>%
  na.omit()

hab_length %>% filter(Water == "FBL") %>%
  group_by(Water, Habitat) %>%
  summarize(Shape_Length = sum(Shape_Length)) %>% 
  ggplot(., aes(x = "", y = Shape_Length, fill = Habitat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, gridlines, and axis text
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Pie Chart") +
  scale_fill_brewer(palette = "Set3") 

hab_length %>% filter(Water == "LML") %>%
  group_by(Water, Habitat) %>%
  summarize(Shape_Length = sum(Shape_Length)) %>% 
  ggplot(., aes(x = "", y = Shape_Length, fill = Habitat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, gridlines, and axis text
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Pie Chart") +
  scale_fill_brewer(palette = "Set3") 


hab_length  %>%
  group_by(Water) %>%
  summarize(Shape_Length = sum(Shape_Length))  %>% 
  ggplot(., aes(x = "", y = Shape_Length, fill = Water)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, gridlines, and axis text
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Pie Chart") +
  scale_fill_brewer(palette = "Set3") 



df <- hab_length %>% 
  group_by(Water, Habitat) %>%
  summarize(Shape_Length = sum(Shape_Length)) %>%
  unite("ID", c(Water, Habitat)) %>%
  arrange(desc(ID)) %>%
  mutate(
    fraction = Shape_Length / sum(Shape_Length),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_position = (ymax + ymin) / 2,
    label = paste0(ID)
  )




pie_chart <- df %>%
  separate(label, into = c("Water", "Habitat")) %>%
  ggplot(., aes(x = 1, y = fraction, fill = Water)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Set color to add outlines
  coord_polar(theta = "y", start = 0) +
  theme_void() +  # Remove background, gridlines, and axis text
  labs(title = "Proportion of shoreline length") +
  scale_fill_brewer(palette = "Set3") +  # Change color palette
  geom_text(aes(y = label_position, label = Habitat), color = "black", size = 4)  # Add text labels


## Hab data classifications -----------------------


setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal")

gps = read.csv("Data/garmin_lml_hab_day1.csv") %>%
  select(ID, lat, lon, ele, name) %>%
  filter(ele < 500)

points = data.frame(ID1 = rep(1:250, each = 250), 
                    ID2 = rep(1:250, by = 250))



distance_pairs = points %>% 
  left_join(gps, by = c("ID1" = "ID")) %>%
  rename("lat1" = "lat", 
         "lon1" = "lon", 
         "ele1" = "ele",
         "name1" = "name") %>%
  left_join(gps, by = c("ID2" = "ID")) %>% 
  rename("lat2" = "lat", 
         "lon2" = "lon", 
         "ele2" = "ele",
         "name2" = "name")%>%
  mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lon2, lat2)))



substrate = read.csv("Data/habitat_class.csv") %>% 
  filter(is.na(start) == F) %>%
  mutate(feature = str_replace(feature, "A", "SV")) %>%
  mutate(feature = str_replace(feature, "SBR","B")) %>%
  mutate(feature = str_replace(feature, "SCS", "SC")) %>%
  mutate(feature = str_replace(feature, "SB", "B")) %>%
  mutate(feature = str_replace(feature, "SCD", "SC"))

substrate

whole = substrate %>%
  left_join(distance_pairs, by = c("start" = "ID1", "end" = "ID2")) 

## Site 28 is different because it loops
point.155 = cbind((gps %>% filter(name == 155))$lat, (gps %>% filter(name == 155))$lat)

point.144 = cbind((gps %>% filter(name == 144))$lat, (gps %>% filter(name == 144))$lat)

point.171 = cbind((gps %>% filter(name == 171))$lat, (gps %>% filter(name == 171))$lat)


site_lengths = substrate %>% group_by(site) %>%
  summarize(ID1 = min(start),
            ID2= max(end)) %>%
  left_join(distance_pairs, by = c("ID1", "ID2")) %>%
  select(site, ID1, ID2, dist) %>%
  rename("shoreline" = "dist") %>% 
  mutate(shoreline = case_when(site == "BEF.028" ~ distHaversine(point.144, 
                                                                 point.155) +
                                 distHaversine(point.155, point.171),
                               site != "BEF.028"~ shoreline))

whole %>% left_join(site_lengths, by = "site") %>%
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  print(n = 100) %>%
  mutate(site = as.factor(parse_number(site) * 1000)) %>%
  print(n = 40) %>%
  ggplot(aes(x = site, y = percent_shoreline, fill =  feature)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~feature) 
