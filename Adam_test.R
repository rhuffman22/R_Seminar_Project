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

# Q2

fia_total <- fia_grid %>%
  group_by(X) %>%
  summarise(richness = n_distinct(TREEcn))

fia_grid_df <- merge(fia_grid_df, fia_total, by.x = "X", by.y = "X")
 
# Q3

minmax <- data.frame(
  Latitude = 0,
  Richness = 0,
)

max(abs(fia_grid_df$centroid_lat))
min(abs(fia_grid_df$centroid_lat))
latrange = max(abs(fia_grid_df$centroid_lat)) - min(abs(fia_grid_df$centroid_lat))

for (i in 1:latrange){
  current_range <- fia_grid_df %>%
    filter(latrange < centroid_lat & centroid_lat < latrange+1)
}