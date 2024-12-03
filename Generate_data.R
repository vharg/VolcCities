#Code to extract population from cities at different distances from volcanoes.
#By Elinor Meredith - 04/12/23

library(sf)              
library(raster)         
library(exactextractr)   
library(writexl)        
library(dplyr)
library(tidyr)
library(terra)
library(writexl)

#Set your working directory
setwd("VolcCities")

#1. NAMING POLYGONS

#Insert your data
city_polygons_path <- "Data/GHS_UCDB_MTUC_GLOBE_2020.shp" #city polygons globally - this one you will need to download from GHS-UCDB R2024 (year 2020)
buffers_path <- "Data/Geodesic_buffers/100km.shp" #100km geodesic buffers around Holocene volcanoes provided
centroids_path <- "Data/centroids_GHS.shp" #Names from GHS 2025 provided
gadm_path <- "Data/gadm.shp" #DUC dataset for missing names - this one you will need to download from GHS-DUC R2023
countries_path <- "Data/ne_10m_admin_0_countries.shp" #Natural Earth countries and subregions provided


#Load data
city_polygons <- st_read(city_polygons_path)
buffers <- st_read(buffers_path)
centroids <- st_read(centroids_path)
gadm <- st_read(gadm_path)
countries <- st_read(countries_path)


#Make all the projections Mollweide
buffers <- st_transform(buffers, st_crs(city_polygons))


#Select only polygons <100km of volcanoes
city_buffers_contact <- st_intersection(city_polygons, buffers)


#Spatial Join: Add Name and Country columns to the city polygons from the centroids
centroid_names <- centroids %>% 
  st_join(city_buffers_contact, join = st_intersects) %>% 
  select(GC_UCN_MAI, GC_CNT_GAD) %>% 
  rename(Name = GC_UCN_MAI, Country = GC_CNT_GAD)

city_buffers_contact <- city_buffers_contact %>% 
  left_join(centroid_names, by = c("geometry" = "geometry"))


#Sort out any missing names
gadm_names <- gadm %>%
  st_join(city_buffers_contact, join = st_contains) %>%
  mutate(Name = ifelse(is.na(Name), NAME_2, Name)) %>% 
  mutate(Name = ifelse(is.na(Name), NAME_1, Name)) %>%
  select(Name)

city_buffers_contact <- city_buffers_contact %>% 
  left_join(gadm_names, by = c("geometry" = "geometry"))


#Join subregion name based on largest intersection
subregion <- countries %>%
  st_intersection(city_buffers_contact) %>%
  group_by(ID_HDC_G0) %>%
  summarize(Subregion = first(SUBREGION)) %>% 
  ungroup()

city_buffers_contact <- city_buffers_contact %>% 
  left_join(subregion, by = "ID_HDC_G0")


#Select final dataset
final_result <- city_buffers_contact %>% 
  select(ID_HDC_G0, Name, Country, Continent = NAME_2, Subregion)

# Save the final shapefile
st_write(final_result, "Results/City_Polygons_Named.shp", delete_dsn = TRUE)





#2. CALCULATE POLYGON DATA

#Input city polygons named, <100km of volcanoes
shapefile_path <- "Results/City_Polygons_Named.shp"
shapefile_data <- st_read(shapefile_path)

#Check in correct projections
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
shapefile_data <- st_set_crs(shapefile_data, mollweide_proj)

#Calculate the area of each polygon and add it as a new column
shapefile_selected <- shapefile_data %>%
  mutate(Area = as.numeric(st_area(geometry)) / 1e6)  # Divide by 1,000,000 to get km²

# Calculate centroids for each polygon
centroids <- st_centroid(shapefile_selected$geometry)

#Extract latitude and longitude from the centroids and add them as new columns
shapefile_selected <- shapefile_selected %>%
  mutate(Lat = st_coordinates(centroids)[, 2],  # Extract latitude
         Lon = st_coordinates(centroids)[, 1])  # Extract longitude

#Rename any columns needed to change
if("SUBREGION" %in% names(shapefile_selected)) {
  shapefile_selected <- shapefile_selected %>%
    rename(Subregion = SUBREGION)
}

if("CONTINENT" %in% names(shapefile_selected)) {
  shapefile_selected <- shapefile_selected %>%
    rename(Continent = CONTINENT)
}

#Merge the polygons by city name
dissolved_shp <- shapefile_selected %>%
  group_by(Name, Country, Subregion) %>%
  summarise(
    Geometry = st_union(geometry),                            
    Area_km2 = sum(Area, na.rm = TRUE),                       
    Largest_polygon = geometry[which.max(Area)],              
    Lat = Lat[which.max(Area)],                               
    Lon = Lon[which.max(Area)],                               
    Centroid = st_centroid(Largest_polygon),                  
    Continent = first(Continent)                              
  ) %>%
  ungroup()

#Drop geometry column for largest_polygon
dissolved_shp <- dissolved_shp %>% select(-Largest_polygon)

#Save file
st_write(dissolved_shp, "Results/City_Polygons_Named.shp", delete_dsn = TRUE)


#3. CLIP BY BUFFERS

#Insert 100km buffer
buffer_100km <- "Data/Geodesic_buffers/100km.shp"
buffers_100km <- st_read(buffer_100km)

#Select your cities dataset
cities1 <- dissolved_shp

# Ensure both layers have the same CRS
if (st_crs(cities1) != st_crs(buffers_100km)) {
  buffers_100km <- st_transform(buffers_100km, st_crs(cities1))
}

#Check projection is Mollweide
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
buffers_100km <- st_set_crs(buffers_100km, mollweide_proj)

#Clip the cities by the 100km buffer
clipped_cities_100km <- st_intersection(cities1, buffers_100km)
clipped_cities_100km <- clipped_cities_100km %>%
  select(Name, Country, Subregion, Area_km2, Geometry
         )

#Define the output file path with the .shp extension
output_file_100km <- "Results/Cities_100km.shp"

#Save the clipped shapefile
st_write(clipped_cities_100km, output_file_100km, delete_dsn = TRUE)


#Insert the 30km buffer
buffer_30km <- "Data/Geodesic_buffers/30km.shp"
buffers_30km <- st_read(buffer_30km)

#Ensure both layers have the same CRS
if (st_crs(clipped_cities_100km) != st_crs(buffers_30km)) {
  buffers_30km <- st_transform(buffers_30km, st_crs(clipped_cities_100km))
}
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
buffers_30km <- st_set_crs(buffers_30km, mollweide_proj)


# Clip the cities by the 30km buffer
clipped_cities_30km <- st_intersection(cities1, buffers_30km)
clipped_cities_30km <- clipped_cities_30km %>%
  select(Name, Country, Subregion, Area_km2, Geometry
         )

#Save the clipped shapefile
output_file_30km <- "Results/Cities_30km.shp"
st_write(clipped_cities_30km, output_file_30km, delete_dsn = TRUE)



#Insert 10km buffer
buffer_10km <- "Data/Geodesic_buffers/10km.shp"
buffers_10km <- st_read(buffer_10km)

#Ensure both layers have the same CRS
if (st_crs(clipped_cities_30km) != st_crs(buffers_10km)) {
  buffers_10km <- st_transform(buffers_10km, st_crs(clipped_cities_30km))
}
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
buffers_10km <- st_set_crs(buffers_10km, mollweide_proj)

#Clip the cities by the 10km buffer
clipped_cities_10km <- st_intersection(cities1, buffers_10km)
clipped_cities_10km <- clipped_cities_10km %>%
  select(Name, Country, Subregion, Area_km2, Geometry
         )

#Save the clipped shapefile
output_file_10km <- "Results/Cities_10km.shp"
st_write(clipped_cities_10km, output_file_10km, delete_dsn = TRUE)





#4. EXTRACT POPULATIONS

# Read the shapefiles
shapefile_100km <- st_read("Results/Cities_100km.shp")
shapefile_30km <- st_read("Results/Cities_30km.shp")
shapefile_10km <- st_read("Results/Cities_10km.shp")
cities <- st_read("Results/City_Polygons_Named.shp")

#Group and summarise the data
#cities <- cities %>%
#  group_by(Name, Country,Subregion) %>%
#  summarize(
#    geometry = st_union(geometry),  #Combine geometries for the groups
#    .groups = "drop"  # Drop the grouping structure
#  )

shapefile_100km <- shapefile_100km %>%
  group_by(Name, Country, Subregion) %>%
  summarize(
    Shape_Area = sum(Area_km2, na.rm = TRUE),
    geometry = st_union(geometry),  # Combine geometries for the groups
    .groups = "drop"  # Drop the grouping structure
  )

# Read the raster
population_raster <- rast("Data/2020.tif")

# Extract and join for 100km cities
extract_and_join <- function(shapefile, raster) {
  extracted_values <- exact_extract(raster, shapefile, fun ="sum") #extract population
  extracted_values <- as.numeric(extracted_values)
  shapefile <- shapefile %>%
    mutate(`100 km Population` = extracted_values) #Population column
  shapefile_df <- as.data.frame(st_drop_geometry(shapefile))
  shapefile_grouped <- shapefile_df %>% #group polygons and sum population
    group_by(Name, Country) %>%
    summarise(`100 km Population` = sum(`100 km Population`, na.rm = TRUE), .groups = 'drop')
  cities_df <- as.data.frame(st_drop_geometry(cities))
  cities_df <- cities_df %>% #Join by Name and Country
    left_join(shapefile_grouped, by = c("Name", "Country")) %>%
    mutate(`100 km Population` = ifelse(is.na(`100 km Population`), 0, `100 km Population`))
  cities <- st_as_sf(cbind(cities_df, st_geometry(cities))) #restore geometery
  return(cities)
}

cities <- extract_and_join(shapefile_100km, population_raster) #takes a while

# Extract and join for 30km cities
shapefile_30km <- shapefile_30km %>%
  group_by(Name, Country, Subregion) %>%
  summarize(
    Shape_Area = sum(Area_km2, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )

extract_and_join <- function(shapefile, raster) {
  extracted_values <- exact_extract(raster, shapefile, fun = "sum")
  extracted_values <- as.numeric(extracted_values)
  shapefile <- shapefile %>%
    mutate(`30 km Population` = extracted_values)
  shapefile_df <- as.data.frame(st_drop_geometry(shapefile))
  shapefile_grouped <- shapefile_df %>%
    group_by(Name, Country) %>%
    summarise(`30 km Population` = sum(`30 km Population`, na.rm = TRUE), .groups = 'drop')
  cities_df <- as.data.frame(st_drop_geometry(cities))
  cities_df <- cities_df %>%
    left_join(shapefile_grouped, by = c("Name", "Country")) %>%
    mutate(`30 km Population` = ifelse(is.na(`30 km Population`), 0, `30 km Population`))
  cities <- st_as_sf(cbind(cities_df, st_geometry(cities)))
  return(cities)
}

cities <- extract_and_join(shapefile_30km, population_raster)

# Extract and join for 10km cities
shapefile_10km <- shapefile_10km %>%
  group_by(Name, Country,  Subregion) %>%
  summarize(
    Shape_Area = sum(Area_km2, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )

extract_and_join <- function(shapefile, raster) {
  extracted_values <- exact_extract(raster, shapefile, fun = "sum")
  extracted_values <- as.numeric(extracted_values)
  shapefile <- shapefile %>%
    mutate(`10 km Population` = extracted_values)
  shapefile_df <- as.data.frame(st_drop_geometry(shapefile))
  shapefile_grouped <- shapefile_df %>%
    group_by(Name, Country) %>%
    summarise(`10 km Population` = sum(`10 km Population`, na.rm = TRUE), .groups = 'drop')
  cities_df <- as.data.frame(st_drop_geometry(cities))
  cities_df <- cities_df %>%
    left_join(shapefile_grouped, by = c("Name", "Country")) %>%
    mutate(`10 km Population` = ifelse(is.na(`10 km Population`), 0, `10 km Population`))
  cities <- st_as_sf(cbind(cities_df, st_geometry(cities)))
  return(cities)
}

cities <- extract_and_join(shapefile_10km, population_raster)

#Remove rows with NA
cities <- cities[!is.na(cities$`100 km Population`), ]
cities <- cities[cities$`100 km Population` != 0, ]

#keep safe here
cities1 <- cities



#5. EXTRACT TOTAL POPULATION

population_raster <- rast("Data/2020.tif")
polygons <- st_read("Results/City_Polygons_Named.shp")

#check the projection
polygons <- st_transform(polygons, crs = st_crs(population_raster))
polygons <- polygons[!st_is_empty(polygons), ]
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
polygons <- st_set_crs(polygons, mollweide_proj)

#Extract population from total city
raster_values <- exact_extract(population_raster, polygons, fun = "sum") #takes a while
polygons <- cbind(st_drop_geometry(polygons), population_sum = raster_values)

#Select only the specified columns
shapefile_selected <- polygons %>%
  select(Name, Country, Subregion, population_sum)

#Merge the polygons by city name
dissolved <- shapefile_selected %>%
  group_by(Name, Country, Subregion,population_sum) %>%
  summarise(                        
  ) %>%
  ungroup()
dissolved <- dissolved %>%
  group_by(Name, Country, Subregion) %>%
  summarise(population_sum=sum(population_sum)                      
  ) %>%
  ungroup()

dissolved <- dissolved %>%
  select(Name, Country, Subregion, population_sum)

#Rename population_sum to Total Population
dissolved <- dissolved %>%
  rename(`Total Population` = population_sum)

#Join to the original dataset based on Name and Country
cities <- cities1 %>%
  left_join(dissolved, by = c("Name", "Country", "Subregion"))
head(cities)



#6. MEASURE NEAREST VOLCANO

#Insert Volcano centroids
volcano_points <- st_read("Data/Volcano_points.shp")

#Check projection
if (st_crs(cities) != st_crs(volcano_points)) {
  volcano_points <- st_transform(volcano_points, st_crs(cities))  # Transform volcano_points to match cities' CRS
}

#Add blank columns
cities$Nearest_Volcano <- NA

#Calculate nearest distance of polygon to volcano
for (i in 1:nrow(cities)) {
  city_polygon <- cities[i, ] #select polygon
  distances <- st_distance(city_polygon, volcano_points) #calculate distance to all points
  min_distance <- min(distances, na.rm = TRUE)  #Select nearest distance and volcano
  nearest_volcano_index <- which.min(distances)
  cities$Nearest_Volcano[i] <- volcano_points$Volcano_Na[nearest_volcano_index]  #Save nearest volcano name
  cities$Nearest_Volcano_Distance_km[i] <- min_distance / 1000  #Add min distance in km
}
head(cities)

#Save file
st_write(cities, "Results/Final_result_2020.shp", delete_dsn = TRUE)



#7. COUNTING NUMBER OF VOLCANOES PEOPLE EXPOSED BY

#insert the cities
cities <- st_read("Results/Final_result_2020.shp")

#Read the number of volcanoes shapefile
union_shapefile <- st_read("Data/Union_2020_100km_Named_2.shp")

#Select the desired columns
selected_columns <- union_shapefile %>%
  select(SUM_Volcan, Name, Country) %>%
  rename(Volcano_No = SUM_Volcan)

#Group by Name and Country, then summarize to get the maximum  number of Volcanoes
grouped_data <- selected_columns %>%
  group_by(Name, Country) %>%
  summarise(Max_Volcano_No = max(Volcano_No, na.rm = TRUE), .groups = 'drop')

#Convert to data frame
cities_df <- st_drop_geometry(cities)

#Join back
cities <- cities_df %>%
  left_join(grouped_data, by = c("Name", "Country"))

#Renaming columns
cities <- cities %>%
  rename(Pop100km = X100kmPp, `30 km Population` = X30kmPpl, `10 km Population` = X10kmPpl, Nearest_Volcano_Distance_km = Nr_V_D_)


#8. ADD RANKINGS

#Add column that ranks cities by 100km
cities <- cities %>%
  mutate(`Rank(Pop100km)` = rank(-Pop100km, na.last = "keep"))

#Calculate population within the buffers
cities <- cities %>%
  mutate(`Not exposed(>100km)` = pmax(TtlPplt - Pop100km, 0),
         `100-30kmPop` = Pop100km - `30 km Population`,
         `30-10kmPop` = `30 km Population` - `10 km Population`)

cities <- cities %>%
  mutate_if(is.numeric, ~ replace_na(., 0))


#Add column that ranks by distance
cities <- cities %>%
  mutate(`Rank(Distance)` = min_rank(Nearest_Volcano_Distance_km))

#Add column that ranks by Max number of volcanoes
cities <- cities %>%
  mutate(`Rank(Max_Volcano_No)` = min_rank(-Max_Volcano_No))

#Add composite rank
cities <- cities %>%
  mutate(`Composite Rank` = round(( `Rank(Max_Volcano_No)` + `Rank(Distance)` + `Rank(Pop100km)` ) / 3))

#Save file
st_write(cities, "Results/Final_result_2020.shp", delete_dsn = TRUE)


#9. ADD POPULATIONS BY NUMBER OF VOLCANOES
# 100KM - add data
union_data <- st_read("Data/Union_2020_100km_Named_2.shp")
population_raster <- rast("Data/2020.tif")

#Sum function
custom_sum <- function(values, coverage_fractions) {
  sum(values, na.rm = TRUE)
}

# Extract population data
union_data$Population <- exact_extract(population_raster, union_data, fun = custom_sum, summarize_df = TRUE)
union_data <- union_data %>%
  rename(Volcano_No = SUM_Volcan)

#Filter for Volcano_No = 1 and sum population
population_1_volcano <- union_data %>%
  filter(Volcano_No == 1) %>%
  group_by(Name, Country) %>%
  summarise(Population100km1volcano = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_1_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 2 and 5 and sum population
population_2_5_volcano <- union_data %>%
  filter(Volcano_No >= 2 & Volcano_No <= 5) %>%
  group_by(Name, Country) %>%
  summarise(Population100km2_5volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_2_5_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 6 and 10 and sum population
population_6_10_volcano <- union_data %>%
  filter(Volcano_No >= 6 & Volcano_No <= 10) %>%
  group_by(Name, Country) %>%
  summarise(Population100km6_10volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_6_10_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 11 and 20 and sum population
population_11_20_volcano <- union_data %>%
  filter(Volcano_No >= 11 & Volcano_No <= 20) %>%
  group_by(Name, Country) %>%
  summarise(Population100km11_20volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_11_20_volcano, by = c("Name", "Country"))

#Filter for Volcano_No greater than 20 and sum population
population_gt_20_volcano <- union_data %>%
  filter(Volcano_No > 20) %>%
  group_by(Name, Country) %>%
  summarise(Population100km_gt_20volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_gt_20_volcano, by = c("Name", "Country"))


#30KM
union_data <- st_read("Data/union_2020_30km_2.shp")

# Step 2: Extract population values using exact extract
population_raster <- rast("Data/2020.tif")

#Extract population data
union_data$Population <- exact_extract(population_raster, union_data, fun = custom_sum, summarize_df = TRUE)

#Select and rename columns
union_data <- union_data %>%
  select(Name, Country, SUM_Volcan, Population) %>%
  rename(Volcano_No = SUM_Volcan) %>%
  as.data.frame() # Convert to dataframe

#Filter for Volcano_No = 1 and sum population
population_1_volcano <- union_data %>%
  filter(Volcano_No == 1) %>%
  group_by(Name, Country) %>%
  summarise(Population30km1volcano = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_1_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 2 and 5 and sum population
population_2_5_volcano <- union_data %>%
  filter(Volcano_No >= 2 & Volcano_No <= 5) %>%
  group_by(Name, Country) %>%
  summarise(Population30km2_5volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_2_5_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 6 and 10 and sum population
population_6_10_volcano <- union_data %>%
  filter(Volcano_No >= 6 & Volcano_No <= 10) %>%
  group_by(Name, Country) %>%
  summarise(Population30km6_10volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_6_10_volcano, by = c("Name", "Country"))


#10KM
union_data <- st_read("Data/union_2020_10km_2.shp")

#Extract population
population_raster <- rast("Data/2020.tif")
union_data$Population <- exact_extract(population_raster, union_data, fun = custom_sum, summarize_df = TRUE)

#Select and rename columns
union_data <- union_data %>%
  select(Name,Country,SUM_Volcan, Population) %>%
  rename(Volcano_No = SUM_Volcan) %>%
  as.data.frame() # Convert to dataframe

#Filter for Volcano_No = 1 and sum population
population_1_volcano <- union_data %>%
  filter(Volcano_No == 1) %>%
  group_by(Name, Country) %>%
  summarise(Population10km1volcano = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_1_volcano, by = c("Name", "Country"))

#Filter for Volcano_No between 2 and 5 and sum population
population_2_5_volcano <- union_data %>%
  filter(Volcano_No >= 2 & Volcano_No <= 5) %>%
  group_by(Name, Country) %>%
  summarise(Population10km2_5volcs = sum(Population, na.rm = TRUE))

#Join with cities dataframe
cities <- cities %>%
  left_join(population_2_5_volcano, by = c("Name", "Country"))

st_write(cities, "Results/Final_result_2020.shp", delete_dsn = TRUE)


#Save final results as an excel file
cities2 <- cities
shapefile_df <- st_drop_geometry(cities2)
cities <- cities %>%
  rename(Subregion = Subregn)
cities <- cities %>%
  rename(
    `Total city area (km2)` = Are_km2,
    Continent = Contnnt,
    `Total city population` = TtlPplt,
    `Nearest volcano` = Nrst_Vl,
    `Distance to nearest volcano (km)` = Nearest_Volcano_Distance_km,
    `Rank by distance` = `Rank(Distance)`,
    `Maximum number of volcanoes exposed by` = Max_Volcano_No,
    `Rank (by <100km)` = `Rank(Pop100km)`
  ) %>%
  select(
    Name, Lat, Lon, Country, Continent, Subregion, 
    `Total city area (km2)`, `Total city population`, `Nearest volcano`,
    `Distance to nearest volcano (km)`, `Rank by distance`, 
    `Maximum number of volcanoes exposed by`, `Composite Rank`, 
    `Rank (by <100km)`, everything()
  ) %>%
  select(-geometry.y.y.y, -geometry.y.y, -geometry.y, 
         -geometry.x, -geometry.x.x, -geometry.x.x.x)

cities <- cities %>%
  arrange(`Composite Rank`)

output_path <- "Results/2020_results.xlsx"
write_xlsx(shapefile_df, output_path)

