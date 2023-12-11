fia_grid_df <- as.data.frame(fia_grid)

### EDA

library(ggplot2)

ncol(fia_df)
nrow(fia_df)
dim(fia_df)
str(fia_df)
names(fia_df)

print(fia_df)
#9 variables
#8 are numerical
#1 is character
summary(fia_df)

#NAs for dia 1425
#NAs for ht 5380

#graphing
library(raster)
library(sp)
library(sf)
library(maps)
library(grid)
library(tmap)


boxplot(fia_df$dia,fia_df$ht)
ggplot(fia_df, aes(x = dia))+
  geom_bar()

ggplot(fia_df_dia_count, aes(x = dia, y = n))+
  geom_point()

ggplot(fia_df, aes(x = dia))+
  geom_histogram(binwidth = 5)

smaller <- fia_df %>%
  filter(dia < 10)
head(smaller)

ggplot(smaller, aes(x = dia))+
  geom_histogram(binwidth = 0.5)
ggplot(smaller, aes(x = dia, colour = ht))+
  geom_freqpoly(binwidth = 0.5)

?cor
cor(fia_df$dia, fia_df$ht)

#### calc species richness at grid level

grid_sr <- fia_df %>%
  group_by(id) %>%
  summarise(SR = n_distinct(spcd))

n_distinct(grid_sr$id)

fia_grid_df <- merge(fia_df, grid_sr, by.x = "id", by.y = "id")
fia_grid_df

###calc avg sr aggregated at the latitudinal band
###calc moving avg at 0.5 degree interval


myseq <- seq(from = 25, to = 50, by = 0.5)
for (i in myseq){
  print(i+1)
}

lat_min <- myseq[2] - 0.5
lat_min
lat_max <- myseq[2] + 0.5
lat_max
length(myseq)
length(myseq)

sr_bands <- data.frame(matrix(ncol = 2, nrow = length(myseq)))
names(sr_bands) <- c("LatBands","SR")
sr_bands

for (i in 1:length(myseq)){
  lat_min <- myseq[i] - 0.5
  lat_max <- myseq[i] + 0.5
  fia_df_subset <- fia_df %>%
    filter(centroid_lat > lat_min & centroid_lat <= lat_max)
  sr <- length(unique(fia_df_subset$COMMON_NAME))
  sr_bands[i,1] <- myseq[i]
  sr_bands[i,2] <- sr
}

sr_bands
ggplot(data = sr_bands, aes(x = LatBands, y = SR)) +
  geom_point()+
  labs(title = "Species Richness by Latitude Bands")

###Estimate the SR using bootstrapping methods (1000 times simulation) and report mean and std. sr

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
    LatRichSample[j,2] = n_distinct(current_frac$SPECIES)
  }
  LatRichRandom[i,1] = i+24
  LatRichRandom[i,2] = mean(LatRichSample$Richness)
}


LatRichRandom2 <- LatRichRandom
LatRichRandom2

