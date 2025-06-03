#Ranking volcanoes by city populations <100 km
# Load required libraries
library(sf)
library(exactextractr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(writexl)
library(terra)

#set working directory
setwd("C:/Users/MeredithES/OneDrive - University of Twente/Documents/Cities/Submission/Github/")

#making dataset
volcano_points_path <- "Volcano_Rank/Data/Volcanoes2025.shp"
city_polygons_path <- "Volcano_Rank/Data/2020.shp"
city_shapefile_path <- "City_Rank/Results/Results_2020.shp"

#Define the paths to the data
buffer_10km_shapefile <- "Volcano_Rank/Data/10km.shp"
buffer_30km_shapefile <- "Volcano_Rank/Data/30km.shp"
buffer_100km_shapefile <- "Volcano_Rank/Data/100km.shp"
population_raster_path <- "City_Rank/Data/2020.tif"

#Read in the shapefiles
cities <- st_read(city_shapefile_path)
buffer_10km <- st_read(buffer_10km_shapefile)
buffer_30km <- st_read(buffer_30km_shapefile)
buffer_100km <- st_read(buffer_100km_shapefile)

#Read in the population raster
population_raster <- rast(population_raster_path)

raster_crs <- st_crs(population_raster)
buffer_100km_crs <- st_crs(buffer_100km)

#Check and align the CRS
if (st_crs(cities) != raster_crs) {
  cities <- st_transform(cities, raster_crs)
}
if (st_crs(buffer_100km) != raster_crs) {
  buffer_100km <- st_transform(buffer_100km, raster_crs)
}
if (st_crs(buffer_30km) != raster_crs) {
  buffer_30km <- st_transform(buffer_30km, raster_crs)
}
if (st_crs(buffer_10km) != raster_crs) {
  buffer_10km <- st_transform(buffer_10km, raster_crs)
}
#Create a function to extract population
extract_population_per_volcano <- function(cities, buffers, population_raster) {
  population_results <- list()

  for (i in seq_len(nrow(buffers))) {
    volcano_buffer <- buffers[i, ]
    volcano_name <- volcano_buffer$Volcano_Na
    country_name <- volcano_buffer$Country
    volcano_number <- volcano_buffer$Volcano_Nu

    intersected_cities <- st_intersection(cities, volcano_buffer)

    population <- exact_extract(population_raster, intersected_cities, "sum")

    population_results[[i]] <- data.frame(
      Volcano_No = volcano_number,
      Volcano_Na = volcano_name,
      Country    = country_name,
      Population = sum(population, na.rm = TRUE)
    )
  }

  return(do.call(rbind, population_results))
}

pop_10km <- extract_population_per_volcano(cities, buffer_10km, population_raster)
pop_30km <- extract_population_per_volcano(cities, buffer_30km, population_raster)
pop_100km <- extract_population_per_volcano(cities, buffer_100km, population_raster)

#Adjust population for overlapping rings
pop_10_30km <- pop_30km
pop_10_30km$Population <- pop_30km$Population - pop_10km$Population

pop_30_100km <- pop_100km
pop_30_100km$Population <- pop_100km$Population - pop_30km$Population

#Combine results
pop_combined <- bind_rows(
  pop_10km %>% mutate(BufferType = "10km"),
  pop_10_30km %>% mutate(BufferType = "10-30km"),
  pop_30_100km %>% mutate(BufferType = "30-100km")
)

pop_summary <- pop_combined %>%
  group_by(Volcano_No, Volcano_Na, BufferType) %>%
  summarise(Population = sum(Population, na.rm = TRUE))
pop_summary_wide <- pivot_wider(pop_summary,names_from = BufferType,values_from = Population)

pop_summary_long <- pop_summary %>%
  mutate(Buffer = factor(BufferType, levels = c("30-100km", "10-30km","10km"  )))



#100km rank
top_10_names <- pop_100km %>%
  arrange(desc(Population)) %>% 
  slice(1:10) %>%         
  pull(Volcano_Na)    

filtered_pop_summary <- pop_summary_long %>%
  filter(Volcano_Na %in% top_10_names) %>%
  mutate(Volcano_Na = factor(Volcano_Na, levels = top_10_names)) %>%
  arrange(Volcano_Na)     

ggplot(filtered_pop_summary, aes(y = Volcano_Na, x = Population, fill = Buffer)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("10km" = "yellow", "10-30km" = "#21908C", "30-100km" = "#440154")) +
  labs(
    title = "Population Exposure by Buffer Zone (Top 10 Volcanoes)",
    x = "Population",
    y = "Volcano Name",
    fill = "Buffer Zone"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

#30km
top_10_names <- pop_30km %>%
  arrange(desc(Population)) %>%
  slice(1:10) %>%   
  pull(Volcano_Na)    

filtered_pop_summary <- pop_summary_long %>%
  filter(Volcano_Na %in% top_10_names) %>%
  mutate(Volcano_Na = factor(Volcano_Na, levels = top_10_names)) %>%
  arrange(Volcano_Na)     

ggplot(filtered_pop_summary, aes(y = Volcano_Na, x = Population, fill = Buffer)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_fill_manual(values = c("10km" = "blue", "10-30km" = "green", "30-100km" = "red")) +
  #scale_x_continuous(limits = c(0, 10000000), labels = scales::comma) + 
  labs(
    title = "Population Exposure by Buffer Zone (Top 10 Volcanoes)",
    x = "Population",
    y = "Volcano Name",
    fill = "Buffer Zone"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

#10km
top_10_names <- pop_10km %>%
  arrange(desc(Population)) %>%  
  slice(1:10) %>%  
  pull(Volcano_Na)        

filtered_pop_summary <- pop_summary_long %>%
  filter(Volcano_Na %in% top_10_names) %>%
  mutate(Volcano_Na = factor(Volcano_Na, levels = top_10_names)) %>%
  arrange(Volcano_Na)     

ggplot(filtered_pop_summary, aes(y = Volcano_Na, x = Population, fill = Buffer)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_manual(values = c("10km" = "yellow", "10-30km" = "#21908C", "30-100km" = "#440154")) +
  scale_x_continuous(limits = c(0, 1000000), labels = scales::comma) +  
  labs(
    title = "Population Exposure by Buffer Zone (Top 10 Volcanoes)",
    x = "Population",
    y = "Volcano Name",
    fill = "Buffer Zone"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )



volcano_points_path <- "City_Rank/Data/volcanoes2025.shp"
city_polygons_path <- "City_Rank/Results/Results_2020.shp"

volcano_points <- st_read(volcano_points_path)
city_polygons <- st_read(city_polygons_path)

if (st_crs(volcano_points) != st_crs(city_polygons)) {
  city_polygons <- st_transform(city_polygons, st_crs(volcano_points))
}

volcano_points <- volcano_points %>%
  mutate(
    Lat = st_coordinates(.)[, 2],
    Lon = st_coordinates(.)[, 1]
  )

city_polygons <- st_make_valid(city_polygons)
volcano_points <- st_make_valid(volcano_points)

volcano_points <- volcano_points %>%
  rename(Volcano_Na = Volcano.Na, Volcano_No = Volcano.Nu)

# Calculate nearest city and distance
nearest_city_info <- st_nearest_feature(volcano_points, city_polygons)
nearest_city_geom <- city_polygons[nearest_city_info, ]
distance_to_city <- st_distance(volcano_points, nearest_city_geom, by_element = TRUE)

volcano_points_with_city <- volcano_points %>%
  mutate(
    Nearest_City = nearest_city_geom$City,
    Distance_to_Nearest_City_km = as.numeric(distance_to_city) / 1000
  )

# Create 100km buffer and count cities
buffer_100km <- st_read(buffer_100km_shapefile)
buffer_30km <- st_read(buffer_30km_shapefile)
buffer_10km <- st_read(buffer_10km_shapefile)

city_counts100 <- sapply(seq_len(nrow(buffer_100km)), function(i) {
  sum(st_intersects(buffer_100km[i, ], city_polygons, sparse = FALSE))
})
city_counts30 <- sapply(seq_len(nrow(buffer_30km)), function(i) {
  sum(st_intersects(buffer_30km[i, ], city_polygons, sparse = FALSE))
})
city_counts10 <- sapply(seq_len(nrow(buffer_10km)), function(i) {
  sum(st_intersects(buffer_10km[i, ], city_polygons, sparse = FALSE))
})

# Add city counts
volcano_points_with_city <- volcano_points_with_city %>%
  mutate(Num_Cities_100_km = city_counts100)
volcano_points_with_city <- volcano_points_with_city %>%
  mutate(Num_Cities_30_km = city_counts30)
volcano_points_with_city <- volcano_points_with_city %>%
  mutate(Num_Cities_10_km = city_counts10)

# Ensure population fields are present before ranking (assume Pop_100_km exists)
if (!"Pop_100_km" %in% colnames(volcano_points_with_city)) {
  volcano_points_with_city$Pop_100_km <- 0  # or handle appropriately
}

volcano_points_with_city2 <- volcano_points_with_city %>%
  left_join(pop_summary_wide, by = "Volcano_No")


# Calculate ranks
volcano_points_with_city <- volcano_points_with_city2 %>%
  mutate(
    City_count_rank = rank(-Num_Cities_100_km, ties.method = "min"),
    Distance_rank = rank(Distance_to_Nearest_City_km, ties.method = "min")
  )

volcano_points_with_city <- volcano_points_with_city %>%
  rename(
    `Volcano Name` = Volcano_Na.x,
    `<10km` = `10km`
  ) %>%
  mutate(
    `<30km` = `<10km` + `10-30km`,
    `<100km` = `<10km` + `10-30km` + `30-100km`
  )

volcano_points_with_city <- volcano_points_with_city %>%
  mutate(
    Population_rank = rank(-.data[["<100km"]], ties.method = "min"),
    Composite_Rank = (Population_rank + City_count_rank + Distance_rank) / 3
  )

volcano_points_with_city <- volcano_points_with_city %>%
select(
    `Volcano Name`, Lat, Lon, Country,
    Nearest_City, Distance_to_Nearest_City_km,
    Distance_rank, Composite_Rank, Population_rank,
    `<100km`, `<30km`, `<10km`, `10-30km`, `30-100km`,
    City_count_rank, Num_Cities_100_km, Num_Cities_30_km, Num_Cities_10_km,
    geometry
  )

#Export results
output_folder <- "Volcano_Rank/Results"
dir.create(output_folder, showWarnings = FALSE)
st_write(volcano_points_with_city, file.path(output_folder, "volcano_rank.shp"), delete_layer = TRUE)
final_dataset_df <- volcano_points_with_city %>% st_drop_geometry()
write_xlsx(final_dataset_df, path = file.path(output_folder, "volcano_rank_2020.xlsx"))


