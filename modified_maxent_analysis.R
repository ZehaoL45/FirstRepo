# 黄石地区冶金考古MaxEnt分析：基于r.walk的水文距离栅格生成
# 投影坐标系：CGCS2000 / 3-degree Gauss-Kruger CM 114E (EPSG:4547)

# 加载必要的包
library(terra)
library(sf)
library(dplyr)
library(rgrass)  # 连接GRASS GIS

# 设置工作目录和参数
setwd("D:/Yejin/hs/huangshi/")

# 使用提供的参数配置
friction_path <- "D:/Yejin/hs/huangshi/data/raw_data/parameters/friction_final.tif"
dem_path <- "D:/Yejin/hs/huangshi/data/raw_data/parameters/dem/dem_16_1.asc"
crs_code <- 4547
crs_proj <- paste0("EPSG:", crs_code)
grass_dbase <- "D:/Huangshi"
home_dir <- "D:/Huangshi/temp"
ram_to_use <- 600  # 减少内存使用
grassdir <- "C:/Program Files/GRASS GIS 8.4"
targetdir <- "D:/Yejin/hs/huangshi/results/lcps_parallel"
dir.create(targetdir, recursive = TRUE, showWarnings = FALSE)
buffer_distance <- 2000

# r.walk系数参数
walk_coeff <- c(0.8, 5.0, -3.25, -5.5)

# 1. 读取数据
study_area <- st_read("data/raw_data/square_sa_prj.shp")
rivers <- vect("data/raw_data/parameters/hydrosheds_river.shp")
lakes <- vect("data/raw_data/parameters/hydrosheds_lake.shp")

# 读取DEM和摩擦力数据
dem <- terra::rast(dem_path) %>% project(crs_proj)
friction <- terra::rast(friction_path) %>% project(crs_proj)

# 2. 统一投影坐标系
study_area <- st_transform(study_area, crs_proj)
rivers <- project(rivers, crs_proj)
lakes <- project(lakes, crs_proj)

# 3. 设置参数
resolution <- 30

# 4. 创建模板栅格
study_extent <- ext(vect(study_area))
template_raster <- rast(study_extent, resolution=resolution, crs=crs_proj)

# 5. 处理湖泊分级
area_quantiles <- quantile(lakes$Lake_area, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)
lakes$size_class <- cut(lakes$Lake_area, 
                        breaks=area_quantiles, 
                        labels=c("小型", "中小型", "中型", "大型"),
                        include.lowest=TRUE)

# 6. 创建必要的目录
dir.create(grass_dbase, recursive = TRUE, showWarnings = FALSE)
dir.create(home_dir, recursive = TRUE, showWarnings = FALSE)

# 7. 初始化GRASS GIS环境
location_name <- "temp_location_maxent"
initGRASS(gisBase = grassdir,
          home = home_dir,
          gisDbase = grass_dbase,
          location = location_name,
          mapset = "PERMANENT",
          override = TRUE)

# 8. 设置投影
execGRASS("g.proj", flags = "c", parameters = list(epsg = crs_code))

# 9. 导入DEM和摩擦力数据
cat("导入DEM数据...\n")
execGRASS("r.in.gdal", parameters = list(
  input = dem_path,
  output = "dem"
), flags = c("overwrite", "e"))

cat("导入摩擦力数据...\n")
execGRASS("r.in.gdal", parameters = list(
  input = friction_path,
  output = "friction"
), flags = "overwrite")

# 10. 设置GRASS区域
execGRASS("g.region", parameters = list(raster = "dem"))

# 11. 函数：使用r.walk计算到水体的距离（修改为使用坐标起点）
calculate_walk_distance_from_water <- function(water_features, output_name, max_time = 3600) {
  
  # 创建临时矢量文件
  temp_file <- tempfile(fileext = ".shp")
  writeVector(water_features, temp_file)
  
  # 导入矢量到GRASS
  execGRASS("v.in.ogr", input = temp_file, output = "temp_water", flags = "overwrite")
  
  # 将矢量转换为栅格
  execGRASS("v.to.rast", 
            input = "temp_water",
            output = "water_raster",
            use = "val",
            value = 1,
            flags = "overwrite")
  
  # 使用r.walk计算通行时间（使用起始栅格）
  execGRASS("r.walk",
            flags = c("overwrite", "k"),
            parameters = list(
              elevation = "dem",
              friction = "friction",
              output = output_name,
              start_raster = "water_raster",
              walk_coeff = paste(walk_coeff, collapse = ","),
              max_cost = max_time,  # 最大通行时间（秒）
              memory = ram_to_use
            ))
  
  # 清理临时文件
  unlink(temp_file)
  files_to_remove <- list.files(pattern = paste0(tools::file_path_sans_ext(basename(temp_file)), "\\."))
  if(length(files_to_remove) > 0) {
    file.remove(files_to_remove)
  }
  
  return(output_name)
}

# 12. 制作距离栅格
cat("正在生成基于r.walk的水文距离栅格...\n")

# 距离所有水体
lakes_boundary <- as.lines(lakes)
all_water <- rbind(rivers, lakes_boundary)
all_water_clipped <- crop(all_water, vect(study_area))

cat("计算距离所有水体...\n")
walk_all_water <- calculate_walk_distance_from_water(all_water_clipped, "walk_all_water")

# 距离主要河流
major_rivers <- subset(rivers, rivers$ORD_CLAS >= 3)
major_rivers_clipped <- crop(major_rivers, vect(study_area))

cat("计算距离主要河流...\n")
walk_major_rivers <- calculate_walk_distance_from_water(major_rivers_clipped, "walk_major_rivers")

# 距离大型湖泊
large_lakes <- subset(lakes, lakes$size_class %in% c("中型", "大型"))
large_lakes_clipped <- crop(large_lakes, vect(study_area))
large_lakes_boundary <- as.lines(large_lakes_clipped)

cat("计算距离大型湖泊...\n")
walk_large_lakes <- calculate_walk_distance_from_water(large_lakes_boundary, "walk_large_lakes")

# 13. 导出结果到指定目录
output_dir <- "data/raw_data/parameters"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

execGRASS("r.out.gdal", parameters = list(
  input = "walk_all_water",
  output = file.path(output_dir, "temp_walk_all_water.tif"),
  format = "GTiff"
), flags = "overwrite")

execGRASS("r.out.gdal", parameters = list(
  input = "walk_major_rivers",
  output = file.path(output_dir, "temp_walk_major_rivers.tif"),
  format = "GTiff"
), flags = "overwrite")

execGRASS("r.out.gdal", parameters = list(
  input = "walk_large_lakes",
  output = file.path(output_dir, "temp_walk_large_lakes.tif"),
  format = "GTiff"
), flags = "overwrite")

# 14. 读取结果
walk_all_water_rast <- rast(file.path(output_dir, "temp_walk_all_water.tif"))
walk_major_rivers_rast <- rast(file.path(output_dir, "temp_walk_major_rivers.tif"))
walk_large_lakes_rast <- rast(file.path(output_dir, "temp_walk_large_lakes.tif"))

# 掩膜到研究区域
walk_all_water_rast <- mask(walk_all_water_rast, vect(study_area))
walk_major_rivers_rast <- mask(walk_major_rivers_rast, vect(study_area))
walk_large_lakes_rast <- mask(walk_large_lakes_rast, vect(study_area))

# 15. 保存最终结果
writeRaster(walk_all_water_rast, file.path(output_dir, "walk_all_water.tif"), overwrite = TRUE)
writeRaster(walk_major_rivers_rast, file.path(output_dir, "walk_major_rivers.tif"), overwrite = TRUE)
writeRaster(walk_large_lakes_rast, file.path(output_dir, "walk_large_lakes.tif"), overwrite = TRUE)

cat("通行时间距离栅格已保存：\n")
cat("- 到所有水体的通行时间:", file.path(output_dir, "walk_all_water.tif"), "\n")
cat("- 到主要河流的通行时间:", file.path(output_dir, "walk_major_rivers.tif"), "\n")
cat("- 到大型湖泊的通行时间:", file.path(output_dir, "walk_large_lakes.tif"), "\n")

# 16. 可视化函数（修改为显示时间）
plot_walk_raster <- function(raster_data, title, water_features = NULL) {
  # 创建颜色方案
  color_palette <- colorRampPalette(c("#000080", "#0066CC", "#00CCFF", "#66FFFF", "#CCFFFF", "#FFFFFF"))(100)
  
  # 主图
  plot(raster_data, 
       main = title,
       col = color_palette,
       axes = FALSE,
       box = FALSE,
       legend = FALSE,
       smooth = TRUE)
  
  # 添加坐标轴
  axis(1, las = 1, cex.axis = 0.8)
  axis(2, las = 1, cex.axis = 0.8)
  box()
  
  # 叠加水文要素
  if(!is.null(water_features) && length(water_features) > 0) {
    plot(water_features, add = TRUE, col = "#001f3f", lwd = 0.8)
  }
  
  # 叠加研究区边界
  plot(vect(study_area), add = TRUE, border = "#2c3e50", lwd = 1.5, fill = NA)
  
  # 添加颜色条图例（时间单位）
  min_time <- global(raster_data, "min", na.rm = TRUE)[1,1]
  max_time <- global(raster_data, "max", na.rm = TRUE)[1,1]
  
  # 创建颜色条
  legend_x <- par("usr")[2] + (par("usr")[2] - par("usr")[1]) * 0.02
  legend_y1 <- par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2
  legend_y2 <- par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.8
  
  # 添加颜色条
  rasterImage(as.raster(rev(color_palette)), 
              xleft = legend_x, 
              ybottom = legend_y1, 
              xright = legend_x + (par("usr")[2] - par("usr")[1]) * 0.03, 
              ytop = legend_y2)
  
  # 添加时间标签（转换为分钟）
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, legend_y1, 
       paste0(round(min_time/60, 1), "分钟"), pos = 4, cex = 0.7)
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, legend_y2, 
       paste0(round(max_time/60, 1), "分钟"), pos = 4, cex = 0.7)
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, 
       (legend_y1 + legend_y2) / 2, 
       "通行时间", pos = 4, cex = 0.8, font = 2)
}

# 17. 可视化三个距离栅格
par(mfrow = c(2, 2), mar = c(4, 4, 3, 5))

# 到所有水体的通行时间
plot_walk_raster(walk_all_water_rast, "到所有水体的通行时间", all_water_clipped)

# 到主要河流的通行时间
plot_walk_raster(walk_major_rivers_rast, "到主要河流的通行时间", major_rivers_clipped)

# 到大型湖泊的通行时间
plot_walk_raster(walk_large_lakes_rast, "到大型湖泊的通行时间", large_lakes_boundary)

# 重置图形参数
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# 18. 统计信息
cat("\n=== 通行时间距离栅格统计信息 ===\n")
cat("使用的r.walk参数：\n")
cat("- walk_coeff:", paste(walk_coeff, collapse = ", "), "\n")
cat("- 内存使用:", ram_to_use, "MB\n")
cat("- 摩擦力数据:", friction_path, "\n")

# 所有水体通行时间统计
cat("\n到所有水体的通行时间:\n")
cat("最短时间:", round(global(walk_all_water_rast, "min", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("最长时间:", round(global(walk_all_water_rast, "max", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("平均时间:", round(global(walk_all_water_rast, "mean", na.rm = TRUE)[1,1]/60, 2), "分钟\n")

# 主要河流通行时间统计
cat("\n到主要河流的通行时间:\n")
cat("最短时间:", round(global(walk_major_rivers_rast, "min", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("最长时间:", round(global(walk_major_rivers_rast, "max", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("平均时间:", round(global(walk_major_rivers_rast, "mean", na.rm = TRUE)[1,1]/60, 2), "分钟\n")

# 大型湖泊通行时间统计
cat("\n到大型湖泊的通行时间:\n")
cat("最短时间:", round(global(walk_large_lakes_rast, "min", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("最长时间:", round(global(walk_large_lakes_rast, "max", na.rm = TRUE)[1,1]/60, 2), "分钟\n")
cat("平均时间:", round(global(walk_large_lakes_rast, "mean", na.rm = TRUE)[1,1]/60, 2), "分钟\n")

# 19. 清理临时文件
temp_files <- c(
  file.path(output_dir, "temp_walk_all_water.tif"),
  file.path(output_dir, "temp_walk_major_rivers.tif"),
  file.path(output_dir, "temp_walk_large_lakes.tif")
)
unlink(temp_files)

cat("\n所有基于r.walk的水文距离栅格生成完成！\n")
cat("使用的参数配置：\n")
cat("- GRASS版本:", grassdir, "\n")
cat("- 工作数据库:", grass_dbase, "\n")
cat("- 摩擦系数:", paste(walk_coeff, collapse = ", "), "\n")
cat("- 内存限制:", ram_to_use, "MB\n")
cat("注意：栅格值为通行时间（秒），在分析中可以作为到水体的'成本距离'使用。\n")