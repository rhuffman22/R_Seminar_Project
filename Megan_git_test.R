
library(dplyr)
library(sf)


fia_df <- read.csv("Data/tree_raw_data_with_env_cleaned(2).csv")
fia_df
length(unique(fia_df$TREEcn))
names(fia_df)

fia_sf <- st_as_sf(fia_df, coords = c("long", "lat"), crs = 4326)
plot(st_geometry(fia_sf),cex = 0.2)
fia_sf_albers <- st_transform(fia_sf, crs = "ESRI:102008")
st_crs(fia_sf_albers)
plot(st_geometry(fia_sf_albers))
head(fia_sf_albers)
extent <- st_bbox(fia_sf_albers)
extent

grid <- st_make_grid(st_as_sfc(extent), crs = "ESRI:102008", cellsize = c(20000,20000), square = TRUE)
plot(grid)
class(grid)
grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
class(grid_sf)
head(grid_sf)

overlaps <- st_join(fia_sf_albers, grid_sf)
class(overlaps)
head(overlaps)
centroids_albers <- st_centroid(grid_sf$geometry)
head(overlaps)
summary(overlaps)
length(overlaps$id)
length(unique(overlaps$id))
id1 <- unique(overlaps$id)
id1

centroids_albers <- st_centroid(grid_sf[grid_sf$id %in% id1,]$geometry)
length(centroids_albers)
class(centroids_albers)
length(centroids_albers)
head(centroids_albers)
head(overlaps)
centroids_wgs84 <- st_transform(centroids_albers, crs = 4326)
head(centroids_wgs84)
centroid_wgs84_coords <- st_coordinates(centroids_wgs84)

summary(centroid_wgs84_coords)
nrow(centroid_wgs84_coords[is.na(centroid_wgs84_coords["X"]),])

gridid <- grid_sf[grid_sf$id %in% id1,]$id
class(gridid)
id1
nrow(centroid_wgs84_coords)
length(gridid)

head(centroid_wgs84_coords)

centroid_df <- data.frame(
  GRIDID = id1,
  centroid_long = centroid_wgs84_coords[,"X"],
  centroid_lat = centroid_wgs84_coords[,"Y"]
)
table(unique(overlaps$id) %in% centroid_df$GRIDID)
fia_grid <- merge(overlaps, centroid_df, by.x = "id", by.y = "GRIDID", all.x = TRUE)

length(unique(fia_grid$X))
length(unique(fia_grid$TREEcn))
class(fia_grid)
head(fia_grid)



#### SR

seq(from = 25, to = 50)
seq(from = 25, to = 50, by = 0.5)
myseq <- seq(from = 25, to = 50, by = 0.5)
for (i in myseq){
  print(i+1)
}

myseq
lat_min <- myseq[2] - 0.5
lat_min
lat_max <- myseq[2] + 0.5
lat_max
length(myseq)
length(myseq)

sr_bands <- data.frame(matrix(ncol = 2, nrow = length(myseq)))
names(sr_bands) <- c("LatBands","SR")
sr_bands

fia_df_subset
fia_df

for (i in 1:length(myseq)){
  lat_min <- myseq[i] - 0.5
  lat_max <- myseq[i] + 0.5
  fia_df_subset <- fia_df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  sr <- length(unique(fia_df_subset$COMMON_NAME))
  sr_bands[i,1] <- myseq[i]
  sr_bands[i,2] <- sr
}

fia_df
names(fia_df)



#### EDA ###
library(ggplot2)
names(fia_df)
ggplot(fia_df, aes(x = dia))+
  geom_bar()

fia_df %>%
  count(dia)

ggplot(fia_df, aes(x = dia))+
  geom_histogram(binwidth = 5)

smaller <- fia_df %>%
  filter(dia < 10)
head(smaller)

ggplot(smaller, aes(x = dia))+
  geom_histogram(binwidth = 0.5)
ggplot(smaller, aes(x = dia, colour = ht))+
  geom_freqpoly(binwidth = 0.5)







str(fia_df)
summary(fia_df)

lat_seq <- seq(from = 32, to = 49, by = 0.5)


grid_species_richness <- fia_df %>%
  group_by(centroid_lat , COMMON_NAME) %>%
  summarise(species_richness = n_distinct(COMMON_NAME))

grid_species_richness
summary(grid_species_richness)
View(grid_species_richness)#species richness is 1, thats not right

lat_avg_species_richness <- grid_species_richness %>%
  group_by(centroid_lat) %>%
  summarise(avg_species_richness = mean)


