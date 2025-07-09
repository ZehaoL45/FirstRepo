# 根据shapefile裁剪文件夹中所有tif文件的脚本
# Load necessary packages
library(terra)
library(sf)
library(dplyr)

# 1. 设置参数
target_folder <- "D:/Yejin/hs/huangshi/data/processed_parameters/parameters_20250709"
target_crs <- "EPSG:4547"
target_resolution <- 30
na_value <- NaN

cat("目标文件夹:", target_folder, "\n")
cat("目标坐标系:", target_crs, "\n") 
cat("目标分辨率:", target_resolution, "米\n")
cat("空值设置:", na_value, "\n\n")

# 2. 检查文件夹是否存在
if(!dir.exists(target_folder)) {
  stop(paste("文件夹不存在:", target_folder))
}

# 3. 查找shapefile
cat("查找shapefile...\n")
shp_files <- list.files(target_folder, pattern = "\\.shp$", full.names = TRUE)

if(length(shp_files) == 0) {
  stop("在目标文件夹中未找到shapefile (.shp)")
} else if(length(shp_files) > 1) {
  cat("发现多个shapefile:\n")
  for(i in seq_along(shp_files)) {
    cat(paste0("  ", i, ". ", basename(shp_files[i]), "\n"))
  }
  # 使用第一个shapefile
  shapefile_path <- shp_files[1]
  cat("使用第一个shapefile:", basename(shapefile_path), "\n")
} else {
  shapefile_path <- shp_files[1]
  cat("找到shapefile:", basename(shapefile_path), "\n")
}

# 4. 读取shapefile
cat("读取shapefile...\n")
tryCatch({
  # 尝试多种编码方式读取shapefile
  boundary <- NULL
  
  # 方法1: UTF-8编码
  tryCatch({
    boundary <- st_read(shapefile_path, options = c("ENCODING=UTF-8"), quiet = TRUE)
    cat("✓ 使用UTF-8编码成功读取\n")
  }, error = function(e1) {
    # 方法2: GBK编码
    tryCatch({
      boundary <- st_read(shapefile_path, options = c("ENCODING=GBK"), quiet = TRUE)
      cat("✓ 使用GBK编码成功读取\n")
    }, error = function(e2) {
      # 方法3: 默认编码
      boundary <- st_read(shapefile_path, quiet = TRUE)
      cat("✓ 使用默认编码成功读取\n")
    })
  })
  
  if(is.null(boundary)) {
    stop("无法读取shapefile")
  }
  
}, error = function(e) {
  stop(paste("读取shapefile失败:", e$message))
})

# 转换到目标坐标系
boundary <- st_transform(boundary, target_crs)
cat("✓ Shapefile坐标系已转换到", target_crs, "\n")
cat("边界要素数量:", nrow(boundary), "\n")

# 计算边界范围
boundary_bbox <- st_bbox(boundary)
cat("边界范围:\n")
print(boundary_bbox)

# 5. 查找所有tif文件
cat("\n查找tif文件...\n")
tif_files <- list.files(target_folder, pattern = "\\.tif$", full.names = TRUE)

if(length(tif_files) == 0) {
  stop("在目标文件夹中未找到tif文件")
}

cat("找到", length(tif_files), "个tif文件:\n")
for(i in seq_along(tif_files)) {
  cat(paste0("  ", i, ". ", basename(tif_files[i]), "\n"))
}

# 6. 创建输出文件夹
output_folder <- file.path(target_folder, "cropped")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
cat("\n输出文件夹:", output_folder, "\n")

# 7. 处理每个tif文件
cat("\n开始处理tif文件...\n")

# 创建处理日志
log_file <- file.path(output_folder, "processing_log.txt")
log_content <- c(
  paste("处理日志 - ", Sys.time()),
  paste("原始文件夹:", target_folder),
  paste("输出文件夹:", output_folder),
  paste("边界文件:", basename(shapefile_path)),
  paste("目标坐标系:", target_crs),
  paste("目标分辨率:", target_resolution, "米"),
  paste("空值设置:", na_value),
  "",
  "处理结果:"
)

# 统计信息
success_count <- 0
error_count <- 0
processing_summary <- data.frame(
  File = character(),
  Status = character(),
  Original_CRS = character(),
  Original_Resolution = character(),
  Original_Extent = character(),
  Cropped_Extent = character(),
  Valid_Cells = numeric(),
  NA_Cells = numeric(),
  stringsAsFactors = FALSE
)

for(i in seq_along(tif_files)) {
  tif_file <- tif_files[i]
  file_name <- tools::file_path_sans_ext(basename(tif_file))
  output_file <- file.path(output_folder, paste0(file_name, "_cropped.tif"))
  
  cat(sprintf("\n[%d/%d] 处理 %s...\n", i, length(tif_files), basename(tif_file)))
  
  tryCatch({
    # 读取原始栅格
    raster_data <- rast(tif_file)
    
    # 记录原始信息
    original_crs <- as.character(crs(raster_data))
    original_res <- paste(res(raster_data), collapse = " x ")
    original_extent <- paste(as.vector(ext(raster_data)), collapse = ", ")
    
    cat("  原始坐标系:", ifelse(original_crs == "", "未定义", original_crs), "\n")
    cat("  原始分辨率:", original_res, "\n")
    
    # 如果坐标系未定义或不正确，设置为目标坐标系
    if(is.na(crs(raster_data)) || crs(raster_data) == "" || !grepl("4547", as.character(crs(raster_data)))) {
      cat("  设置坐标系为", target_crs, "\n")
      crs(raster_data) <- target_crs
    }
    
    # 如果不是目标坐标系，进行投影变换
    if(!grepl("4547", as.character(crs(raster_data)))) {
      cat("  投影变换到", target_crs, "\n")
      raster_data <- project(raster_data, target_crs)
    }
    
    # 设置分辨率（如果需要）
    current_res <- res(raster_data)[1]
    if(abs(current_res - target_resolution) > 1) {  # 允许1米的误差
      cat(sprintf("  重采样分辨率从 %.1f 到 %d 米\n", current_res, target_resolution))
      # 创建目标模板
      template <- rast(ext(raster_data), resolution = target_resolution, crs = target_crs)
      raster_data <- resample(raster_data, template, method = "bilinear")
    }
    
    # 裁剪到边界
    cat("  裁剪到边界范围...\n")
    raster_cropped <- crop(raster_data, boundary, snap = "near")
    raster_masked <- mask(raster_cropped, boundary)
    
    # 设置空值
    if(!is.nan(na_value)) {
      cat("  设置空值为", na_value, "\n")
      NAflag(raster_masked) <- na_value
    }
    
    # 计算统计信息
    values_vec <- values(raster_masked, na.rm = FALSE)
    valid_cells <- sum(!is.na(values_vec))
    na_cells <- sum(is.na(values_vec))
    cropped_extent <- paste(as.vector(ext(raster_masked)), collapse = ", ")
    
    # 保存结果
    writeRaster(raster_masked, output_file, overwrite = TRUE, NAflag = na_value)
    
    cat("  ✓ 成功保存:", basename(output_file), "\n")
    cat(sprintf("    有效像元: %d, 空值像元: %d\n", valid_cells, na_cells))
    
    # 记录成功
    success_count <- success_count + 1
    log_content <- c(log_content, paste("✓", basename(tif_file), "- 处理成功"))
    
    # 添加到汇总表
    processing_summary <- rbind(processing_summary, data.frame(
      File = basename(tif_file),
      Status = "成功",
      Original_CRS = original_crs,
      Original_Resolution = original_res,
      Original_Extent = original_extent,
      Cropped_Extent = cropped_extent,
      Valid_Cells = valid_cells,
      NA_Cells = na_cells,
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat("  ✗ 处理失败:", e$message, "\n")
    error_count <- error_count + 1
    log_content <<- c(log_content, paste("✗", basename(tif_file), "- 处理失败:", e$message))
    
    # 添加到汇总表
    processing_summary <<- rbind(processing_summary, data.frame(
      File = basename(tif_file),
      Status = paste("失败:", e$message),
      Original_CRS = NA,
      Original_Resolution = NA,
      Original_Extent = NA,
      Cropped_Extent = NA,
      Valid_Cells = NA,
      NA_Cells = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# 8. 生成处理报告
cat("\n" , paste(rep("=", 60), collapse = ""), "\n")
cat("处理完成！\n")
cat("成功处理:", success_count, "个文件\n")
cat("处理失败:", error_count, "个文件\n")
cat("输出位置:", output_folder, "\n")

# 保存日志
log_content <- c(
  log_content,
  "",
  paste("处理完成时间:", Sys.time()),
  paste("成功:", success_count, "个文件"),
  paste("失败:", error_count, "个文件")
)
writeLines(log_content, log_file)

# 保存处理汇总表
summary_file <- file.path(output_folder, "processing_summary.csv")
write.csv(processing_summary, summary_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n生成的文件:\n")
cat("- processing_log.txt (处理日志)\n")
cat("- processing_summary.csv (处理汇总表)\n")

# 列出输出的tif文件
output_tifs <- list.files(output_folder, pattern = "_cropped\\.tif$")
if(length(output_tifs) > 0) {
  cat("\n裁剪后的tif文件:\n")
  for(tif in output_tifs) {
    cat("✓", tif, "\n")
  }
}

cat(paste(rep("=", 60), collapse = ""), "\n")

# 9. 可选：验证处理结果
cat("\n验证处理结果...\n")
if(length(output_tifs) > 0) {
  # 读取第一个文件进行验证
  test_file <- file.path(output_folder, output_tifs[1])
  test_raster <- rast(test_file)
  
  cat("验证样本文件:", output_tifs[1], "\n")
  cat("坐标系:", as.character(crs(test_raster)), "\n")
  cat("分辨率:", paste(res(test_raster), collapse = " x "), "米\n")
  cat("空间范围:", paste(as.vector(ext(test_raster)), collapse = ", "), "\n")
  cat("数据范围:", paste(range(values(test_raster), na.rm = TRUE), collapse = " 到 "), "\n")
}