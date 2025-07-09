# Load necessary packages
library(terra)
library(sf)
library(dplyr)

# Set working directory
setwd("D:/Yejin/hs/huangshi")

# 1. Load sites data first to define the spatial extent
cat("读取sites数据...\n")
sites <- st_read("D:/Yejin/hs/huangshi/data/raw_data/surveypoints.shp")

# Transform sites to EPSG:4547 if not already
sites <- st_transform(sites, "epsg:4547")

# Calculate the bounding box of all sites
sites_bbox <- st_bbox(sites)
cat("Sites边界框:\n")
print(sites_bbox)

# Create a SpatExtent object for cropping
# Adding a small buffer to ensure all sites are included
buffer_distance <- 1000  # 1km buffer, adjust as needed
sites_extent <- ext(
  sites_bbox[1] - buffer_distance,  # xmin
  sites_bbox[3] + buffer_distance,  # xmax
  sites_bbox[2] - buffer_distance,  # ymin
  sites_bbox[4] + buffer_distance   # ymax
)

cat("扩展后的裁剪范围:\n")
print(sites_extent)

# 2. Load and process data
cat("加载和处理栅格数据...\n")

# DEM
dem <- terra::rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/dem/dem_16_1.asc") %>%
  project("epsg:4547")

# Slope
slope <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/landserf-slope/slope_15.grd")
dem_original <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/dem/dem_16_1.asc")
crs(slope) <- crs(dem_original)
slope <- project(slope, "epsg:4547")
slope <- resample(slope, dem, method = "bilinear")

# Terrain feature rasters
channel <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/landserf/Channel_15.grd")
planar <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/landserf/Planar_15.grd")
ridge <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/landserf/Ridge_15.grd")

# Set CRS and project
crs(channel) <- crs(dem_original)
crs(planar) <- crs(dem_original)
crs(ridge) <- crs(dem_original)

channel <- project(channel, "epsg:4547")
planar <- project(planar, "epsg:4547")
ridge <- project(ridge, "epsg:4547")

channel <- resample(channel, dem, method = "bilinear")
planar <- resample(planar, dem, method = "bilinear")
ridge <- resample(ridge, dem, method = "bilinear")

# LCP Density raster
lcp_density <- rast("D:/Yejin/hs/huangshi/results/density_analysis/grid_5000_sigma_2000/lcp_density_grid5000_sigma2000.tif")
crs(lcp_density) <- crs(dem_original)
lcp_density <- project(lcp_density, "epsg:4547")
lcp_density <- resample(lcp_density, dem, method = "bilinear")

# Viewshed rasters
outgoing <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/TVM/outgoing.tif")
incoming <- rast("D:/Yejin/hs/huangshi/data/raw_data/parameters/TVM/incoming.tif")

crs(outgoing) <- crs(dem_original)
crs(incoming) <- crs(dem_original)

outgoing <- project(outgoing, "epsg:4547")
incoming <- project(incoming, "epsg:4547")

outgoing <- resample(outgoing, dem, method = "bilinear")
incoming <- resample(incoming, dem, method = "bilinear")

# Walking time rasters
walking_time_to_mines <- rast("D:/Yejin/hs/huangshi/results/lcps_parallel/min_weighted_avg_walking_time.tif")
walk_all_water <- rast("D:/Yejin/hs/huangshi/results/water_distance/walk_all_water.tif")

crs(walking_time_to_mines) <- crs(dem_original)
crs(walk_all_water) <- crs(dem_original)

walking_time_to_mines <- project(walking_time_to_mines, "epsg:4547")
walk_all_water <- project(walk_all_water, "epsg:4547")

walking_time_to_mines <- resample(walking_time_to_mines, dem, method = "bilinear")
walk_all_water <- resample(walk_all_water, dem, method = "bilinear")

# 3. Crop ALL rasters to the sites extent (including lcp_density)
cat("裁剪所有栅格到sites范围...\n")

dem_cropped <- crop(dem, sites_extent, snap = "near")
slope_cropped <- crop(slope, sites_extent, snap = "near")
channel_cropped <- crop(channel, sites_extent, snap = "near")
planar_cropped <- crop(planar, sites_extent, snap = "near")
ridge_cropped <- crop(ridge, sites_extent, snap = "near")
lcp_density_cropped <- crop(lcp_density, sites_extent, snap = "near")  # 也裁剪lcp_density
outgoing_cropped <- crop(outgoing, sites_extent, snap = "near")
incoming_cropped <- crop(incoming, sites_extent, snap = "near")
walking_time_to_mines_cropped <- crop(walking_time_to_mines, sites_extent, snap = "near")
walk_all_water_cropped <- crop(walk_all_water, sites_extent, snap = "near")

# 4. Verify all extents are the same
cat("验证所有栅格的空间范围...\n")
cat("目标范围: ", as.vector(sites_extent), "\n")
cat("DEM范围: ", as.vector(ext(dem_cropped)), "\n")
cat("Slope范围: ", as.vector(ext(slope_cropped)), "\n")
cat("Channel范围: ", as.vector(ext(channel_cropped)), "\n")
cat("Planar范围: ", as.vector(ext(planar_cropped)), "\n")
cat("Ridge范围: ", as.vector(ext(ridge_cropped)), "\n")
cat("LCP Density范围: ", as.vector(ext(lcp_density_cropped)), "\n")
cat("Outgoing范围: ", as.vector(ext(outgoing_cropped)), "\n")
cat("Incoming范围: ", as.vector(ext(incoming_cropped)), "\n")
cat("Walking time to mines范围: ", as.vector(ext(walking_time_to_mines_cropped)), "\n")
cat("Walk all water范围: ", as.vector(ext(walk_all_water_cropped)), "\n")

# 5. Create a function to plot with consistent extent
plot_with_consistent_extent <- function(raster_data, title, color_palette, sites_data = NULL) {
  # 确保使用相同的空间范围
  plot(raster_data, 
       main = title, 
       col = color_palette, 
       axes = FALSE, 
       legend = TRUE,
       ext = sites_extent)  # 强制使用相同的extent
  
  # 可选：叠加sites点
  if(!is.null(sites_data)) {
    sites_vect <- vect(sites_data)
    plot(sites_vect, add = TRUE, col = "red", cex = 0.5, pch = 16)
  }
}

# 6. Plot all rasters with consistent extent
cat("生成一致空间范围的图像...\n")

par(mfrow = c(2, 5), mar = c(2, 2, 2, 4))  # Set 2x5 layout, adjust margins

# Plot DEM
plot_with_consistent_extent(dem_cropped, "Digital Elevation Model (DEM)", 
                          terrain.colors(100), sites)

# Plot Slope
plot_with_consistent_extent(slope_cropped, "Slope", 
                          hcl.colors(100, "YlOrRd"), sites)

# Plot Channel
plot_with_consistent_extent(channel_cropped, "Channel", 
                          hcl.colors(100, "Blues"), sites)

# Plot Planar
plot_with_consistent_extent(planar_cropped, "Planar", 
                          hcl.colors(100, "Grays"), sites)

# Plot Ridge
plot_with_consistent_extent(ridge_cropped, "Ridge", 
                          hcl.colors(100, "Reds"), sites)

# Plot LCP Density (now also cropped)
plot_with_consistent_extent(lcp_density_cropped, "LCP Density (Grid 5000, SIGMA 2000)", 
                          hcl.colors(100, "Viridis"), sites)

# Plot Outgoing Viewshed
plot_with_consistent_extent(outgoing_cropped, "Outgoing Viewshed", 
                          hcl.colors(100, "PuBu"), sites)

# Plot Incoming Viewshed
plot_with_consistent_extent(incoming_cropped, "Incoming Viewshed", 
                          hcl.colors(100, "GnBu"), sites)

# Plot Walking Time to Mines
plot_with_consistent_extent(walking_time_to_mines_cropped, "Walking Time to Mines", 
                          hcl.colors(100, "RdBu"), sites)

# Plot Walking Time to Water
plot_with_consistent_extent(walk_all_water_cropped, "Walking Time to All Water", 
                          hcl.colors(100, "BuGn"), sites)

# Restore default plotting parameters
par(mfrow = c(1, 1))

# 7. Optional: Save the cropped rasters for future use
cat("保存裁剪后的栅格...\n")
output_dir <- "results/cropped_rasters"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

writeRaster(dem_cropped, file.path(output_dir, "dem_sites_extent.tif"), overwrite = TRUE)
writeRaster(slope_cropped, file.path(output_dir, "slope_sites_extent.tif"), overwrite = TRUE)
writeRaster(channel_cropped, file.path(output_dir, "channel_sites_extent.tif"), overwrite = TRUE)
writeRaster(planar_cropped, file.path(output_dir, "planar_sites_extent.tif"), overwrite = TRUE)
writeRaster(ridge_cropped, file.path(output_dir, "ridge_sites_extent.tif"), overwrite = TRUE)
writeRaster(lcp_density_cropped, file.path(output_dir, "lcp_density_sites_extent.tif"), overwrite = TRUE)
writeRaster(outgoing_cropped, file.path(output_dir, "outgoing_sites_extent.tif"), overwrite = TRUE)
writeRaster(incoming_cropped, file.path(output_dir, "incoming_sites_extent.tif"), overwrite = TRUE)
writeRaster(walking_time_to_mines_cropped, file.path(output_dir, "walking_time_mines_sites_extent.tif"), overwrite = TRUE)
writeRaster(walk_all_water_cropped, file.path(output_dir, "walk_water_sites_extent.tif"), overwrite = TRUE)

cat("完成！所有栅格已裁剪到sites的空间范围。\n")
cat("Sites数量:", nrow(sites), "\n")
cat("最终空间范围:", as.vector(sites_extent), "\n")