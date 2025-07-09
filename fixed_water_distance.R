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
targetdir <- "D:/Yejin/hs/huangshi/results/water_distance"
dir.create(targetdir, recursive = TRUE, showWarnings = FALSE)
buffer_distance <- 2000

# r.walk系数参数
walk_coeff <- c(0.8, 5.0, -3.25, -5.5)

# 1. 读取数据
cat("读取输入数据...\n")
study_area <- st_read("data/raw_data/square_sa_prj.shp")
rivers <- vect("data/raw_data/parameters/hydrosheds_river.shp")
lakes <- vect("data/raw_data/parameters/hydrosheds_lake.shp")

# 检查输入文件是否存在
if(!file.exists(dem_path)) stop(paste("DEM文件不存在:", dem_path))
if(!file.exists(friction_path)) stop(paste("摩擦力文件不存在:", friction_path))
cat("✓ 所有输入文件存在\n")

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
cat("初始化GRASS GIS环境...\n")
location_name <- "temp_location_maxent"

tryCatch({
  initGRASS(gisBase = grassdir,
            home = home_dir,
            gisDbase = grass_dbase,
            location = location_name,
            mapset = "PERMANENT",
            override = TRUE)
  cat("✓ GRASS初始化成功\n")
}, error = function(e) {
  stop(paste("GRASS初始化失败:", e$message))
})

# 8. 设置投影
cat("设置投影...\n")
tryCatch({
  execGRASS("g.proj", flags = "c", parameters = list(epsg = crs_code))
  cat("✓ 投影设置成功\n")
}, error = function(e) {
  stop(paste("投影设置失败:", e$message))
})

# 9. 导入DEM和摩擦力数据
cat("导入DEM数据...\n")
tryCatch({
  execGRASS("r.in.gdal", parameters = list(
    input = dem_path,
    output = "dem"
  ), flags = c("overwrite", "e"))
  cat("✓ DEM导入成功\n")
}, error = function(e) {
  stop(paste("DEM导入失败:", e$message))
})

cat("导入摩擦力数据...\n")
tryCatch({
  execGRASS("r.in.gdal", parameters = list(
    input = friction_path,
    output = "friction"
  ), flags = "overwrite")
  cat("✓ 摩擦力数据导入成功\n")
}, error = function(e) {
  stop(paste("摩擦力数据导入失败:", e$message))
})

# 10. 设置GRASS区域
cat("设置GRASS区域...\n")
tryCatch({
  execGRASS("g.region", parameters = list(raster = "dem"))
  cat("✓ 区域设置成功\n")
}, error = function(e) {
  stop(paste("区域设置失败:", e$message))
})

# 验证导入的数据
cat("验证导入的栅格数据...\n")
raster_list <- execGRASS("g.list", type = "raster", intern = TRUE)
cat("GRASS中的栅格数据:", paste(raster_list, collapse = ", "), "\n")

if(!"dem" %in% raster_list) stop("DEM数据未正确导入")
if(!"friction" %in% raster_list) stop("摩擦力数据未正确导入")

# 11. 改进的函数：使用r.walk计算到水体的距离
calculate_walk_distance_from_water <- function(water_features, output_name) {
  
  cat(sprintf("处理 %s...\n", output_name))
  
  # 创建临时矢量文件
  temp_file <- tempfile(fileext = ".shp")
  writeVector(water_features, temp_file)
  cat(sprintf("  创建临时文件: %s\n", basename(temp_file)))
  
  # 导入矢量到GRASS
  tryCatch({
    execGRASS("v.in.ogr", 
              parameters = list(input = temp_file, output = "temp_water"), 
              flags = "overwrite")
    cat("  ✓ 矢量导入成功\n")
  }, error = function(e) {
    stop(paste("矢量导入失败:", e$message))
  })
  
  # 将矢量转换为栅格
  tryCatch({
    execGRASS("v.to.rast", 
              parameters = list(
                input = "temp_water",
                output = "water_raster",
                use = "val",
                value = 1
              ),
              flags = "overwrite")
    cat("  ✓ 矢量转栅格成功\n")
  }, error = function(e) {
    stop(paste("矢量转栅格失败:", e$message))
  })
  
  # 使用r.walk计算通行时间
  cat("  执行r.walk计算...\n")
  tryCatch({
    execGRASS("r.walk",
              flags = c("overwrite", "k"),
              parameters = list(
                elevation = "dem",
                friction = "friction",
                output = paste0(output_name, "_seconds"),
                start_raster = "water_raster",
                walk_coeff = paste(walk_coeff, collapse = ","),
                memory = ram_to_use
              ))
    cat("  ✓ r.walk计算成功\n")
  }, error = function(e) {
    stop(paste("r.walk计算失败:", e$message))
  })
  
  # 验证r.walk输出
  raster_list_after <- execGRASS("g.list", type = "raster", intern = TRUE)
  if(!paste0(output_name, "_seconds") %in% raster_list_after) {
    stop(paste("r.walk输出栅格未创建:", paste0(output_name, "_seconds")))
  }
  
  # 将秒转换为小时
  tryCatch({
    execGRASS("r.mapcalc",
              parameters = list(
                expression = paste0(output_name, " = ", output_name, "_seconds / 3600.0")
              ),
              flags = "overwrite")
    cat("  ✓ 单位转换成功\n")
  }, error = function(e) {
    stop(paste("单位转换失败:", e$message))
  })
  
  # 删除临时的秒单位栅格
  execGRASS("g.remove", type = "raster", name = paste0(output_name, "_seconds"), flags = "f")
  
  # 清理临时文件
  unlink(temp_file)
  files_to_remove <- list.files(pattern = paste0(tools::file_path_sans_ext(basename(temp_file)), "\\."))
  if(length(files_to_remove) > 0) {
    file.remove(files_to_remove)
  }
  
  # 验证最终输出
  final_raster_list <- execGRASS("g.list", type = "raster", intern = TRUE)
  if(!output_name %in% final_raster_list) {
    stop(paste("最终输出栅格未创建:", output_name))
  }
  
  cat(sprintf("  ✓ %s 计算完成\n", output_name))
  return(output_name)
}

# 12. 制作距离栅格
cat("正在生成基于r.walk的水文距离栅格（小时单位）...\n")

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

# 13. 验证所有计算结果
cat("验证计算结果...\n")
final_raster_list <- execGRASS("g.list", type = "raster", intern = TRUE)
required_rasters <- c("walk_all_water", "walk_major_rivers", "walk_large_lakes")

for(raster_name in required_rasters) {
  if(!raster_name %in% final_raster_list) {
    stop(paste("栅格计算失败:", raster_name))
  } else {
    cat(sprintf("  ✓ %s 存在\n", raster_name))
  }
}

# 14. 导出结果到指定目录
cat("导出结果...\n")
output_dir <- "results/water_distance"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 使用绝对路径确保导出位置正确
abs_output_dir <- file.path(getwd(), output_dir)
dir.create(abs_output_dir, recursive = TRUE, showWarnings = FALSE)

# 简化的导出参数
export_raster <- function(input_name, output_filename) {
  output_path <- file.path(abs_output_dir, output_filename)
  cat(sprintf("  导出 %s 到 %s\n", input_name, output_path))
  
  tryCatch({
    execGRASS("r.out.gdal", 
              parameters = list(
                input = input_name,
                output = output_path,
                format = "GTiff"
              ), 
              flags = "overwrite")
    
    # 验证文件是否创建
    if(file.exists(output_path)) {
      cat(sprintf("  ✓ %s 导出成功\n", basename(output_path)))
      return(TRUE)
    } else {
      stop(paste("导出失败，文件不存在:", output_path))
    }
  }, error = function(e) {
    stop(paste("导出失败:", e$message))
  })
}

# 导出所有栅格
export_raster("walk_all_water", "temp_walk_all_water.tif")
export_raster("walk_major_rivers", "temp_walk_major_rivers.tif")
export_raster("walk_large_lakes", "temp_walk_large_lakes.tif")

# 15. 读取结果并验证
cat("读取并验证导出的栅格...\n")

# 检查文件是否存在
temp_files <- c(
  file.path(abs_output_dir, "temp_walk_all_water.tif"),
  file.path(abs_output_dir, "temp_walk_major_rivers.tif"),
  file.path(abs_output_dir, "temp_walk_large_lakes.tif")
)

for(file in temp_files) {
  if(!file.exists(file)) {
    stop(paste("文件不存在:", file))
  } else {
    cat("✓ 文件存在:", basename(file), "\n")
  }
}

# 读取结果
walk_all_water_rast <- rast(temp_files[1])
walk_major_rivers_rast <- rast(temp_files[2])
walk_large_lakes_rast <- rast(temp_files[3])

# 检查数据完整性
cat("检查数据完整性...\n")
check_raster <- function(rast_obj, name) {
  valid_cells <- sum(!is.na(values(rast_obj, na.rm=FALSE)))
  total_cells <- ncell(rast_obj)
  cat(sprintf("  %s: %d/%d 有效像元 (%.1f%%)\n", 
              name, valid_cells, total_cells, 
              100 * valid_cells / total_cells))
  return(valid_cells > 0)
}

all_water_ok <- check_raster(walk_all_water_rast, "所有水体")
major_rivers_ok <- check_raster(walk_major_rivers_rast, "主要河流") 
large_lakes_ok <- check_raster(walk_large_lakes_rast, "大型湖泊")

if(!all(all_water_ok, major_rivers_ok, large_lakes_ok)) {
  warning("某些栅格数据可能有问题，请检查输入数据")
}

# 掩膜到研究区域
walk_all_water_rast <- mask(walk_all_water_rast, vect(study_area))
walk_major_rivers_rast <- mask(walk_major_rivers_rast, vect(study_area))
walk_large_lakes_rast <- mask(walk_large_lakes_rast, vect(study_area))

# 16. 保存最终结果
final_files <- c(
  file.path(abs_output_dir, "walk_all_water.tif"),
  file.path(abs_output_dir, "walk_major_rivers.tif"),
  file.path(abs_output_dir, "walk_large_lakes.tif")
)

writeRaster(walk_all_water_rast, final_files[1], overwrite = TRUE)
writeRaster(walk_major_rivers_rast, final_files[2], overwrite = TRUE)
writeRaster(walk_large_lakes_rast, final_files[3], overwrite = TRUE)

cat("通行时间距离栅格已保存（小时单位）：\n")
for(file in final_files) {
  cat("-", file, "\n")
}

# 17. 可视化函数（修改为显示小时）
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
  
  # 添加颜色条图例（小时单位）
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
  
  # 添加小时标签
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, legend_y1, 
       paste0(round(min_time, 2), "小时"), pos = 4, cex = 0.7)
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, legend_y2, 
       paste0(round(max_time, 2), "小时"), pos = 4, cex = 0.7)
  text(legend_x + (par("usr")[2] - par("usr")[1]) * 0.04, 
       (legend_y1 + legend_y2) / 2, 
       "通行时间", pos = 4, cex = 0.8, font = 2)
}

# 18. 可视化三个距离栅格
cat("生成可视化图像...\n")
par(mfrow = c(2, 2), mar = c(4, 4, 3, 5))

# 到所有水体的通行时间
plot_walk_raster(walk_all_water_rast, "到所有水体的通行时间", all_water_clipped)

# 到主要河流的通行时间
plot_walk_raster(walk_major_rivers_rast, "到主要河流的通行时间", major_rivers_clipped)

# 到大型湖泊的通行时间
plot_walk_raster(walk_large_lakes_rast, "到大型湖泊的通行时间", large_lakes_boundary)

# 重置图形参数
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# 19. 统计信息
cat("\n=== 通行时间距离栅格统计信息 ===\n")
cat("使用的r.walk参数：\n")
cat("- walk_coeff:", paste(walk_coeff, collapse = ", "), "\n")
cat("- 内存使用:", ram_to_use, "MB\n")
cat("- 摩擦力数据:", friction_path, "\n")
cat("- 输出单位: 小时\n")
cat("- 无最大通行时间限制\n")

# 所有水体通行时间统计
cat("\n到所有水体的通行时间:\n")
cat("最短时间:", round(global(walk_all_water_rast, "min", na.rm = TRUE)[1,1], 3), "小时\n")
cat("最长时间:", round(global(walk_all_water_rast, "max", na.rm = TRUE)[1,1], 3), "小时\n")
cat("平均时间:", round(global(walk_all_water_rast, "mean", na.rm = TRUE)[1,1], 3), "小时\n")

# 主要河流通行时间统计
cat("\n到主要河流的通行时间:\n")
cat("最短时间:", round(global(walk_major_rivers_rast, "min", na.rm = TRUE)[1,1], 3), "小时\n")
cat("最长时间:", round(global(walk_major_rivers_rast, "max", na.rm = TRUE)[1,1], 3), "小时\n")
cat("平均时间:", round(global(walk_major_rivers_rast, "mean", na.rm = TRUE)[1,1], 3), "小时\n")

# 大型湖泊通行时间统计
cat("\n到大型湖泊的通行时间:\n")
cat("最短时间:", round(global(walk_large_lakes_rast, "min", na.rm = TRUE)[1,1], 3), "小时\n")
cat("最长时间:", round(global(walk_large_lakes_rast, "max", na.rm = TRUE)[1,1], 3), "小时\n")
cat("平均时间:", round(global(walk_large_lakes_rast, "mean", na.rm = TRUE)[1,1], 3), "小时\n")

# 20. 清理临时文件
cat("清理临时文件...\n")
unlink(temp_files)

cat("\n所有基于r.walk的水文距离栅格生成完成！\n")
cat("使用的参数配置：\n")
cat("- GRASS版本:", grassdir, "\n")
cat("- 工作数据库:", grass_dbase, "\n")
cat("- 摩擦系数:", paste(walk_coeff, collapse = ", "), "\n")
cat("- 内存限制:", ram_to_use, "MB\n")
cat("注意：栅格值为通行时间（小时），可以直接用于MaxEnt分析。\n")