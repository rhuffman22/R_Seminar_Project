library(dplyr)
library(sf)

fia <- read.csv("tree_raw_data_with_env_cleaned.csv") 
ref_species <- read.csv("REF_SPECIES.csv") 
fia <- merge(fia, ref_species, by.x = "spcd", by.y = "SPCD")

fia10 <- fia %>% 
  sample_frac(0.1) %>% 
  select(X, TREEcn, plt_cn, statecd, spcd, dia, ht, lat, long, SPECIES, COMMON_NAME) 
fia <- fia10

fia_sf <- st_as_sf(fia, coords = c("long", "lat"), crs = 4326)
plot(st_geometry(fia_sf),cex = 0.2)

fia_sf_albers <- st_transform(fia_sf, crs = "ESRI:102008") 
plot(st_geometry(fia_sf_albers))
fia_extent <- st_bbox(fia_sf_albers) 
grid <- st_make_grid(st_as_sfc(fia_extent), cellsize = c(20000,20000), square = TRUE)
plot(grid) 

fia_grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
overlaps <- st_join(fia_sf_albers, fia_grid_sf) 

centroids_albers <- st_centroid(fia_grid_sf$geometry) 
id1 <- unique(overlaps$id) 
centroids_albers <- st_centroid(fia_grid_sf[fia_grid_sf$id %in% id1,]$geometry)

centroids_wgs84 <- st_transform(centroids_albers, crs = 4326) 
centroid_wgs84_coords <- st_coordinates(centroids_wgs84) 
summary(centroid_wgs84_coords) 
nrow(centroid_wgs84_coords[is.na(centroid_wgs84_coords["X"]),])

gridid <- fia_grid_sf[fia_grid_sf$id %in% id1,]$id 
centroid_df <- data.frame( GRIDID = id1, centroid_long = centroid_wgs84_coords[,"X"], centroid_lat = centroid_wgs84_coords[,"Y"] )

fia_grid <- merge(overlaps, centroid_df, by.x = "id", by.y = "GRIDID", all.x = TRUE)
fia_grid_df <- as.data.frame(fia_grid)
plot(fia_grid)

n_distinct(fia_grid_df$X)
n_distinct(fia_grid_df$id)

# Q2

fia_total <- fia_grid %>%
  group_by(id) %>%
  summarise(richness = n_distinct(SPECIES))

n_distinct(fia_total$id)

fia_grid_df <- merge(fia_grid_df, fia_total, by.x = "id", by.y = "id")
 
# Q3
# We need a data frame that has only one entry per id so that our averages are not affected
fia_id <- fia_grid_df %>%
  distinct(id, .keep_all = TRUE)

LatRich <- data.frame(
  Latitude = 0,
  Richness = 0
)

max(abs(fia_grid_df$centroid_lat))
min(abs(fia_grid_df$centroid_lat))

#Since we want to include all of the maximum and minimum band, we start at the 25-26 band and go to the 49-50 band
#25 bands total

for (i in 1:25){
  current_range <- fia_id %>%
    filter((i+24) < centroid_lat & centroid_lat < (i+25))
  LatRich[i,1] = i+24
  LatRich[i,2] = mean(current_range$richness.y)
}

# Q4
# For moving average, we essentially want an extra row in between each row we had previously, so 25*2-1 = 49 rows.
# Using 50 rows would give us a NaN value for the last field, since it would be after the last row. 

LatRichMoving <- data.frame(
  Latitude = 0,
  Richness = 0
)

for (i in 1:49){
  current_range <- fia_id %>%
    filter((i/2+24.5) < centroid_lat & centroid_lat < (i/2+25.5))
  LatRichMoving[i,1] = i/2+24.5
  LatRichMoving[i,2] = mean(current_range$richness.y)
}

# Q5
# Now we return to the full data set.
# For each latitude, we create an empty data frame, take the richness from a random sample a certain number of times, 
#   then average them and place that average in a final data frame. Repeat for every latitude.
# Note that the sample size is 100 trees, so latitudes with less than 100 rows will give the same value every time.

LatRichRandom <- data.frame(
  Latitude = 0,
  Richness = 0
)

for (i in 1:25){
  current_range <- fia_grid_df %>%
    filter((i+24) < centroid_lat & centroid_lat < (i+25))
  LatRichSample <- data.frame(
    Run = 0,
    Richness = 0
  )
  for (j in 1:1000){
    current_frac <- current_range %>% slice_sample(n = 100)
    LatRichSample[j,1] = j
    LatRichSample[j,2] = mean(current_frac$richness)
  }
  LatRichRandom[i,1] = i+24
  LatRichRandom[i,2] = mean(LatRichSample$Richness)
}

