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