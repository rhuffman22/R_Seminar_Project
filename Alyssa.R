library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(matrixStats)
library(git2r)

# Use FIA full data set (eastern US forest)

## 1. Conduct EDA

FIA_full = read.csv("C:/Users/rhuffman/OneDrive - The University of Memphis/R-programming/Tree_Data_Analaysis/tree_raw_data_with_env_cleaned.csv")
nrow(FIA_full)
ncol(FIA_full)
names(FIA_full)
fia_full_df = as.data.frame(FIA_full)
head(fia_full_df)


## 2. Calculate species richness (sr) at grid level

species_richness <- fia_full_df %>%
  group_by(plt_cn, common) %>%
  summarise(Richness = length(unique(spcd)))
species_richness


## 3. Calculate average sr aggregated at the latitudinal band
## 4. Calculate the moving average at 0.5 degree interval

# Sequencing by 0.5 degrees
# Your original code for creating the sequence
myseq <- seq(from = 25, to = 50, by = 0.5)
myseq

# Creating an empty data frame to store results
sr_bands <- data.frame(LatBands = numeric(length(myseq)), SR = numeric(length(myseq)))
sr_bands

# Looping through each latitude band
for (i in 1:length(myseq)) {
  # Defining the latitude range for the current band
  lat_min <- myseq[i] - 0.25  # Adjusted to represent a 0.5-degree interval
  lat_max <- myseq[i] + 0.25  # Adjusted to represent a 0.5-degree interval
  
  # Filtering the data within the latitude range
  fia_df_subset <- fia_full_df %>%
    filter(Lat > lat_min & Lat <= lat_max)
  
  # Calculating species richness (number of unique species) for the current band
  sr <- length(unique(fia_df_subset$spcd))
  
  # Storing the results in the data frame
  sr_bands[i, "LatBands"] <- myseq[i]
  sr_bands[i, "SR"] <- sr
}
sr_bands

# Creating a scatter plot
ggplot(data = sr_bands, aes(x = LatBands, y = SR)) +
  geom_point() +
  labs(
    title = "Species Richness Along Latitude Bands",
    x = "Latitude Bands",
    y = "Species Richness")



## 5. Estimate the SR using bootstrapping methods (1000 times simulation) and report mean and std. sr

# Set seed for reproducibility
set.seed(123)

# Number of bootstrap samples
n_bootstrap <- 1000

# Create a data frame to store bootstrap results
results <- data.frame(id = unique(fia_grid_df$id))

# Perform bootstrapping using a for loop
for (i in 1:n_bootstrap) {
  sampled_df <- fia_grid_df[sample(1:nrow(fia_grid_df), replace = TRUE), ]
  grid_sr <- sampled_df %>%
    group_by(id) %>%
    summarise(SR = n_distinct(spcd))
  
  # Merge the bootstrap results with the original data
  results <- merge(results, grid_sr, by = "id", all.x = TRUE)
  colnames(results)[colnames(results) == "SR"] <- paste0("bootstrap_", i)
}

# Calculate mean and standard deviation of SR across bootstrap samples
results$mean_SR <- rowMeans(results[, -(1:1)], na.rm = TRUE)
results$std_SR <- apply(results[, -(1:1)], 1, sd, na.rm = TRUE)


# Print the mean and standard deviation
cat("Mean SR:", mean(results$mean_SR, na.rm = TRUE), "\n")
cat("Standard Deviation of SR:", sd(results$std_SR, na.rm = TRUE), "\n")

# Merge centroid latitude and longitude into the results data frame
results <- merge(results, fia_grid_df[, c("id", "centroid_lat", "centroid_Long")], by = "id", all.x = TRUE)
results


## 6. Present your work using ggplot

# Scatter plot using ggplot with centroid_Long and centroid_lat

Std_SR_plot <- ggplot(results, aes(x = centroid_Long, y = centroid_lat, color = std_SR)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +  # Adjust color scale
  labs(title = "Bootstrap Results for Stabdard Deviation Species Richness",
       x = "Centroid Longitude",
       y = "Centroid Latitude") +
  theme_minimal()
Std_SR_plot

MeanSR_plot <- ggplot(results, aes(x = centroid_Long, y = centroid_lat, color = mean_SR)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +  # Adjust color scale
  labs(title = "Bootstrap Results for Mean Species Richness",
       x = "Centroid Longitude",
       y = "Centroid Latitude") +
  theme_minimal()
MeanSR_plot
