# Load necessary packages
library(terra)
library(sf)
library(dplyr)

# Set working directory
setwd("D:/Yejin/hs/huangshi")

# 1. Load and process data
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

# 2. Crop all rasters to lcp_density extent
cat("裁剪所有栅格到统一范围...\n")

dem_cropped <- crop(dem, lcp_density, snap = "near")
slope_cropped <- crop(slope, lcp_density, snap = "near")
channel_cropped <- crop(channel, lcp_density, snap = "near")
planar_cropped <- crop(planar, lcp_density, snap = "near")
ridge_cropped <- crop(ridge, lcp_density, snap = "near")
outgoing_cropped <- crop(outgoing, lcp_density, snap = "near")
incoming_cropped <- crop(incoming, lcp_density, snap = "near")
walking_time_to_mines_cropped <- crop(walking_time_to_mines, lcp_density, snap = "near")
walk_all_water_cropped <- crop(walk_all_water, lcp_density, snap = "near")

# 3. Verify extents
cat("验证所有栅格的空间范围...\n")
cat("Extent of lcp_density: ", as.vector(ext(lcp_density)), "\n")
cat("Extent of dem_cropped: ", as.vector(ext(dem_cropped)), "\n")
cat("Extent of slope_cropped: ", as.vector(ext(slope_cropped)), "\n")
cat("Extent of channel_cropped: ", as.vector(ext(channel_cropped)), "\n")
cat("Extent of planar_cropped: ", as.vector(ext(planar_cropped)), "\n")
cat("Extent of ridge_cropped: ", as.vector(ext(ridge_cropped)), "\n")
cat("Extent of outgoing_cropped: ", as.vector(ext(outgoing_cropped)), "\n")
cat("Extent of incoming_cropped: ", as.vector(ext(incoming_cropped)), "\n")
cat("Extent of walking_time_to_mines_cropped: ", as.vector(ext(walking_time_to_mines_cropped)), "\n")
cat("Extent of walk_all_water_cropped: ", as.vector(ext(walk_all_water_cropped)), "\n")

# 4. Plot cropped rasters with consistent extent
cat("生成可视化图像...\n")

par(mfrow = c(2, 5), mar = c(2, 2, 2, 4))  # Set 2x5 layout, adjust margins

# Plot DEM
plot(dem_cropped, main = "Digital Elevation Model (DEM)", 
     col = terrain.colors(100), 
     axes = FALSE, 
     legend = TRUE)

# Plot Slope
plot(slope_cropped, main = "Slope", 
     col = hcl.colors(100, "YlOrRd"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Channel
plot(channel_cropped, main = "Channel", 
     col = hcl.colors(100, "Blues"), 
     axes = FALSE,
     legend = TRUE)

# Plot Planar
plot(planar_cropped, main = "Planar", 
     col = hcl.colors(100, "Grays"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Ridge
plot(ridge_cropped, main = "Ridge", 
     col = hcl.colors(100, "Reds"), 
     axes = FALSE, 
     legend = TRUE)

# Plot LCP Density
plot(lcp_density, main = "LCP Density (Grid 5000, SIGMA 2000)", 
     col = hcl.colors(100, "Viridis"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Outgoing Viewshed
plot(outgoing_cropped, main = "Outgoing Viewshed", 
     col = hcl.colors(100, "PuBu"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Incoming Viewshed
plot(incoming_cropped, main = "Incoming Viewshed", 
     col = hcl.colors(100, "GnBu"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Walking Time to Mines
plot(walking_time_to_mines_cropped, main = "Walking Time to Mines", 
     col = hcl.colors(100, "RdBu"), 
     axes = FALSE, 
     legend = TRUE)

# Plot Walking Time to Water
plot(walk_all_water_cropped, main = "Walking Time to All Water", 
     col = hcl.colors(100, "BuGn"), 
     axes = FALSE, 
     legend = TRUE)

# Restore default plotting parameters
par(mfrow = c(1, 1))

# 5. 创建新文件夹并导出所有参数栅格
cat("创建新文件夹并导出所有参数栅格...\n")

# 创建导出目录
export_dir <- "data/processed_parameters"
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

# 获取当前日期作为子文件夹名称
current_date <- format(Sys.Date(), "%Y%m%d")
dated_export_dir <- file.path(export_dir, paste0("parameters_", current_date))
dir.create(dated_export_dir, recursive = TRUE, showWarnings = FALSE)

cat("导出目录:", dated_export_dir, "\n")

# 创建参数列表和对应的文件名
parameter_list <- list(
  "01_dem" = dem_cropped,
  "02_slope" = slope_cropped,
  "03_channel" = channel_cropped,
  "04_planar" = planar_cropped,
  "05_ridge" = ridge_cropped,
  "06_lcp_density" = lcp_density,
  "07_outgoing_viewshed" = outgoing_cropped,
  "08_incoming_viewshed" = incoming_cropped,
  "09_walking_time_to_mines" = walking_time_to_mines_cropped,
  "10_walking_time_to_water" = walk_all_water_cropped
)

# 导出所有栅格
cat("正在导出栅格文件...\n")
for(name in names(parameter_list)) {
  output_path <- file.path(dated_export_dir, paste0(name, ".tif"))
  writeRaster(parameter_list[[name]], output_path, overwrite = TRUE)
  cat("✓ 已导出:", basename(output_path), "\n")
}

# 6. 创建参数说明文件
cat("创建参数说明文件...\n")

metadata_file <- file.path(dated_export_dir, "parameters_metadata.txt")
metadata_content <- paste0(
  "参数栅格数据说明\n",
  "================\n\n",
  "生成日期: ", Sys.time(), "\n",
  "坐标系: EPSG:4547 (CGCS2000 / 3-degree Gauss-Kruger CM 114E)\n",
  "空间范围: ", paste(as.vector(ext(lcp_density)), collapse = ", "), "\n",
  "像元大小: ", res(dem_cropped)[1], " x ", res(dem_cropped)[2], " 米\n\n",
  
  "文件说明:\n",
  "---------\n",
  "01_dem.tif                    - 数字高程模型 (Digital Elevation Model)\n",
  "02_slope.tif                  - 坡度 (Slope)\n",
  "03_channel.tif                - 河道特征 (Channel Network Feature)\n",
  "04_planar.tif                 - 平面特征 (Planar Feature)\n",
  "05_ridge.tif                  - 山脊特征 (Ridge Feature)\n",
  "06_lcp_density.tif            - 最小成本路径密度 (Least Cost Path Density)\n",
  "07_outgoing_viewshed.tif      - 向外视域 (Outgoing Viewshed)\n",
  "08_incoming_viewshed.tif      - 向内视域 (Incoming Viewshed)\n",
  "09_walking_time_to_mines.tif  - 到矿点的步行时间 (Walking Time to Mines)\n",
  "10_walking_time_to_water.tif  - 到水源的步行时间 (Walking Time to Water)\n\n",
  
  "数据处理步骤:\n",
  "------------\n",
  "1. 所有栅格数据统一投影到 EPSG:4547\n",
  "2. 重采样到相同的空间分辨率和网格\n",
  "3. 裁剪到相同的空间范围\n",
  "4. 适用于MaxEnt建模和空间分析\n\n",
  
  "使用说明:\n",
  "--------\n",
  "这些栅格文件可以直接用于:\n",
  "- MaxEnt物种分布建模\n",
  "- 空间分析和建模\n",
  "- GIS可视化和制图\n",
  "- 考古遗址适宜性分析\n"
)

writeLines(metadata_content, metadata_file)
cat("✓ 已创建参数说明文件:", basename(metadata_file), "\n")

# 7. 生成统计报告
cat("生成统计报告...\n")

stats_file <- file.path(dated_export_dir, "parameters_statistics.csv")

# 计算每个栅格的统计信息
stats_df <- data.frame(
  Parameter = names(parameter_list),
  Min_Value = numeric(length(parameter_list)),
  Max_Value = numeric(length(parameter_list)),
  Mean_Value = numeric(length(parameter_list)),
  Std_Dev = numeric(length(parameter_list)),
  Valid_Cells = numeric(length(parameter_list)),
  NA_Cells = numeric(length(parameter_list)),
  stringsAsFactors = FALSE
)

for(i in seq_along(parameter_list)) {
  raster_data <- parameter_list[[i]]
  values_vec <- values(raster_data, na.rm = FALSE)
  
  stats_df$Min_Value[i] <- min(values_vec, na.rm = TRUE)
  stats_df$Max_Value[i] <- max(values_vec, na.rm = TRUE)
  stats_df$Mean_Value[i] <- mean(values_vec, na.rm = TRUE)
  stats_df$Std_Dev[i] <- sd(values_vec, na.rm = TRUE)
  stats_df$Valid_Cells[i] <- sum(!is.na(values_vec))
  stats_df$NA_Cells[i] <- sum(is.na(values_vec))
}

write.csv(stats_df, stats_file, row.names = FALSE)
cat("✓ 已创建统计报告:", basename(stats_file), "\n")

# 8. 打印完成信息
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("所有参数栅格导出完成！\n")
cat("导出位置:", dated_export_dir, "\n")
cat("文件数量:", length(parameter_list), "个栅格文件\n")
cat("附加文件:\n")
cat("- parameters_metadata.txt (参数说明)\n")
cat("- parameters_statistics.csv (统计报告)\n")
cat("\n导出的文件列表:\n")
for(name in names(parameter_list)) {
  cat("✓", paste0(name, ".tif"), "\n")
}
cat(paste(rep("=", 60), collapse = ""), "\n")