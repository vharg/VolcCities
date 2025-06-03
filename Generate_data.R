#Code to extract population from cities at different distances from volcanoes.
#Creates the Results_2020 file
#By Elinor Meredith - 04/12/23

library(sf)              
library(raster)         
library(exactextractr)   
library(writexl)        
library(dplyr)
library(tidyr)
library(terra)
library(writexl)
library(stringr)
library(readxl)

# Set working directory
setwd("C:/Users/MeredithES/OneDrive - University of Twente/Documents/Cities/Submission/Github/City_Rank")

# Load city and volcano shapefiles, transform CRS
cities <- st_read("Data/2020.shp")
volcanoes <- st_read("Data/Volcanoes2025.shp")
mollweide_proj <- "+proj=moll +datum=WGS84"
volcanoes <- st_transform(volcanoes, mollweide_proj)
cities <- st_transform(cities, mollweide_proj)

#Calculate nearest volcano and distance
nearest_volcano_names <- character(nrow(cities))
nearest_distances <- numeric(nrow(cities))

for (i in seq_len(nrow(cities))) {
  city_geom <- cities$geometry[i]
  dists <- st_distance(city_geom, volcanoes)
  min_idx <- which.min(dists)
  nearest_distances[i] <- as.numeric(dists[min_idx]) / 1000 
  nearest_volcano_names[i] <- volcanoes$`Volcano.Na`[min_idx]
}

cities$Distance_to_nearest_volcano <- nearest_distances
cities$Nearest_volcano <- nearest_volcano_names

#save here
cities1 <- cities
cities <- cities1

#join to city_ID
load("Data/Cities_1975to2020_df_v2.Rdata")
city2020 <- city_df_all$city2020 %>%
  mutate(
    GC_UCN_MAI_clean = str_trim(tolower(iconv(GC_UCN_MAI, from = "", to = "UTF-8"))),
    CENTROID_X = round(as.numeric(CENTROID_X), 6),
    join_key = paste0(GC_UCN_MAI_clean, "_", CENTROID_X),
    city_ID = as.character(city_ID)
  )

cities <- cities %>%
  mutate(
    GC_UCN_clean = str_trim(tolower(iconv(GC_UCN_MAI, from = "", to = "UTF-8"))),
    CENTROID_X = round(as.numeric(CENTROID_X), 6),
    join_key = paste0(GC_UCN_clean, "_", CENTROID_X)
  )

match_indices <- match(cities$join_key, city2020$join_key)
cities$city_ID <- city2020$city_ID[match_indices]
cities <- cities %>% select(-join_key, -GC_UCN_clean)

#save here
cities2 <- cities
cities <- cities2


#add total population
population_raster <- raster("Data/2020.tif")
cities$Total_Population <- exact_extract(population_raster, cities, 'sum')

buffer_10km <- st_read("Data/Geodesic_Buffers/10km.shp")
buffer_30km <- st_read("Data/Geodesic_Buffers/30km.shp")
buffer_100km <- st_read("Data/Geodesic_Buffers/100km.shp")
buffer_10km <- st_transform(buffer_10km, crs(population_raster))
buffer_30km <- st_transform(buffer_30km, crs(population_raster))
buffer_100km <- st_transform(buffer_100km, crs(population_raster))
cities <- st_transform(cities, crs(population_raster))

#Clip cities to buffers
cities_10km <- st_intersection(cities, buffer_10km)
cities_30km <- st_intersection(cities, buffer_30km)
cities_100km <- st_intersection(cities, buffer_100km)

#Extract population
cities_10km$Pop_10km <- exact_extract(population_raster, cities_10km, 'sum')
cities_30km$Pop_30km <- exact_extract(population_raster, cities_30km, 'sum')
cities_100km$Pop_100km <- exact_extract(population_raster, cities_100km, 'sum')

pop10_df <- cities_10km %>%
  st_drop_geometry() %>%
  group_by(city_ID) %>%
  summarise(Pop_10km = sum(Pop_10km, na.rm = TRUE))

pop30_df <- cities_30km %>%
  st_drop_geometry() %>%
  group_by(city_ID) %>%
  summarise(Pop_30km = sum(Pop_30km, na.rm = TRUE))

pop100_df <- cities_100km %>%
  st_drop_geometry() %>%
  group_by(city_ID) %>%
  summarise(Pop_100km = sum(Pop_100km, na.rm = TRUE))

#Join these back to original cities
cities_df <- st_drop_geometry(cities)

cities <- cities_df %>%
  left_join(pop10_df, by = "city_ID") %>%
  left_join(pop30_df, by = "city_ID") %>%
  left_join(pop100_df, by = "city_ID")

cities_df <- cities_df %>%
  mutate(across(starts_with("Pop_"), ~replace_na(., 0)))

#save here
cities3 <- cities
cities <- cities3

#how many volcanoes is the city exposed to
process_union_data <- function(shapefile_path, buffer_label, cities, population_raster) {
  union_data <- st_read(shapefile_path, quiet = TRUE)
  union_data <- st_transform(union_data, crs = crs(population_raster))

  union_data <- union_data %>%
    st_cast("POLYGON") %>%
    st_sf()
  union_data$population <- exact_extract(population_raster, union_data, 'sum')

  union_data <- union_data %>%
    select(geometry, population, SUM_Volcan)

  volcano_col <- paste0("Population", buffer_label)
  union_data <- union_data %>%
    mutate(VolcanoGroup = case_when(
      SUM_Volcan == 1 ~ paste0(volcano_col, "1volcano"),
      SUM_Volcan >= 2 & SUM_Volcan <= 5 ~ paste0(volcano_col, "2_5volcs"),
      SUM_Volcan >= 6 & SUM_Volcan <= 10 ~ paste0(volcano_col, "6_10volcs"),
      SUM_Volcan >= 10 & SUM_Volcan <= 20 ~ paste0(volcano_col, "10_20volcs"),
      SUM_Volcan > 20 ~ paste0(volcano_col, ">20volcs"),
      TRUE ~ NA_character_
    ))

  if (!"city_ID" %in% names(cities)) stop("Column 'city_ID' not found in cities.")
  cities_sf <- cities[, "city_ID"]

  union_data <- st_join(
    union_data,
    cities_sf,
    join = st_intersects,
    left = TRUE,
    largest = TRUE
  )
  
  summary_df <- union_data %>%
    group_by(city_ID, VolcanoGroup) %>%
    summarise(Population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = VolcanoGroup, values_from = Population, values_fill = 0) %>%
    st_set_geometry(NULL)

  summary_df <- summary_df %>%
    group_by(city_ID) %>%
    summarise(across(starts_with(paste0("Population", buffer_label)), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
  st_write(union_data, paste0("Union_with_cityID_", buffer_label, ".shp"), delete_dsn = TRUE)
  
  return(summary_df)
}

summary10 <- process_union_data("Data/Union_2020_10km.shp", "10km", cities2, population_raster)
summary30 <- process_union_data("Data/Union_2020_30km.shp", "30km", cities2, population_raster)
summary100 <- process_union_data("Data/Union_2020_100km.shp", "100km", cities2, population_raster)

#join all back to cities
cities <- cities %>%
  left_join(summary10, by = "city_ID") %>%
  left_join(summary30, by = "city_ID") %>%
  left_join(summary100, by = "city_ID")

#max volcanoes
union_data100 <- st_read("Data/Union_with_cityID_100km.shp")
union_data100 <- union_data100 %>% 
  st_cast("POLYGON") %>%
  st_sf()  
selected_columns <- union_data100 %>%
  select(SUM_Vlc, city_ID) %>%
  rename(Volcano_No = SUM_Vlc)

grouped_data <- selected_columns %>%
  group_by(city_ID) %>%
  summarise(Max_Volcano_No = max(Volcano_No, na.rm = TRUE), .groups = 'drop')

cities <- cities %>%
  left_join(grouped_data, by = c('city_ID'))

#save here
cities4 <- cities
cities <- cities4



#add rankings
cities <- cities %>%
  mutate(
    Rank_Pop100km = rank(-Pop_100km, na.last = "keep"),
    Not_exposed_gt_100km = pmax(Total_Population - Pop_100km, 0),
    Pop_100_30km = Pop_100km - Pop_30km,
    Pop_30_10km = Pop_30km - Pop_10km,
    Rank_Distance = min_rank(Distance_to_nearest_volcano),
    Rank_Max_Volcano_No = min_rank(-Max_Volcano_No),
    Composite_Rank = round((Rank_Max_Volcano_No + Rank_Distance + Rank_Pop100km) / 3)
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))


cities <- cities %>%
  rename(
    City = GC_UCN_MAI,
    Country = NAME,
    Continent = CONTINENT_,
    Subregion = SUBREGION,
    Longitude = CENTROID_X,
    Latitude = CENTROID_Y,
    Area = AREA,
  ) %>%
  select(city_ID, City, Country, Continent, Subregion, Longitude, Latitude,
         Distance_to_nearest_volcano, Nearest_volcano, Total_Population,
         Pop_10km, Pop_30km, Pop_100km, Max_Volcano_No, Rank_Pop100km,
         Not_exposed_gt_100km, Pop_100_30km, Pop_30_10km,
         Rank_Distance, Rank_Max_Volcano_No, Composite_Rank,
         starts_with("Pop100km_"), starts_with("Pop30km_"), starts_with("Pop10km_"))

#save
write_xlsx(st_drop_geometry(cities), "Results/Results_2020.xlsx")
cities <- st_sf(cities, geometry = st_geometry(cities2))
st_write(cities, "Results/Results_2020.shp", delete_dsn = TRUE)
