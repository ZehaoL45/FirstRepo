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

# 10. 生成可视化图像
cat("\n生成可视化图像...\n")

if(length(output_tifs) > 0) {
  
  # 创建绘图函数
  plot_raster_with_boundary <- function(raster_path, title, color_palette, boundary_sf) {
    raster_data <- rast(raster_path)
    
    # 绘制栅格
    plot(raster_data, 
         main = title,
         col = color_palette,
         axes = FALSE,
         legend = TRUE,
         mar = c(2, 2, 3, 4))
    
    # 叠加边界
    boundary_vect <- vect(boundary_sf)
    plot(boundary_vect, add = TRUE, border = "red", lwd = 2, fill = NA)
    
    # 添加坐标轴
    axis(1, las = 1, cex.axis = 0.7)
    axis(2, las = 1, cex.axis = 0.7)
    box()
  }
  
  # 定义颜色方案
  color_schemes <- list(
    dem = terrain.colors(100),
    slope = hcl.colors(100, "YlOrRd"),
    channel = hcl.colors(100, "Blues"),
    planar = hcl.colors(100, "Grays"),
    ridge = hcl.colors(100, "Reds"),
    lcp = hcl.colors(100, "Viridis"),
    outgoing = hcl.colors(100, "PuBu"),
    incoming = hcl.colors(100, "GnBu"),
    mines = hcl.colors(100, "RdBu"),
    water = hcl.colors(100, "BuGn")
  )
  
  # 根据文件名匹配颜色方案
  get_color_scheme <- function(filename) {
    filename_lower <- tolower(filename)
    if(grepl("dem", filename_lower)) return(color_schemes$dem)
    if(grepl("slope", filename_lower)) return(color_schemes$slope)
    if(grepl("channel", filename_lower)) return(color_schemes$channel)
    if(grepl("planar", filename_lower)) return(color_schemes$planar)
    if(grepl("ridge", filename_lower)) return(color_schemes$ridge)
    if(grepl("lcp|density", filename_lower)) return(color_schemes$lcp)
    if(grepl("outgoing", filename_lower)) return(color_schemes$outgoing)
    if(grepl("incoming", filename_lower)) return(color_schemes$incoming)
    if(grepl("mine", filename_lower)) return(color_schemes$mines)
    if(grepl("water", filename_lower)) return(color_schemes$water)
    return(hcl.colors(100, "Spectral"))  # 默认颜色
  }
  
  # 生成标题
  get_plot_title <- function(filename) {
    filename_clean <- gsub("_cropped\\.tif$", "", filename)
    filename_clean <- gsub("^\\d+_", "", filename_clean)  # 移除数字前缀
    
    title_map <- list(
      "dem" = "Digital Elevation Model (DEM)",
      "slope" = "Slope",
      "channel" = "Channel Network",
      "planar" = "Planar Curvature",
      "ridge" = "Ridge Features",
      "lcp_density" = "LCP Density",
      "outgoing_viewshed" = "Outgoing Viewshed",
      "incoming_viewshed" = "Incoming Viewshed",
      "walking_time_to_mines" = "Walking Time to Mines",
      "walking_time_to_water" = "Walking Time to Water"
    )
    
    # 查找匹配的标题
    for(key in names(title_map)) {
      if(grepl(key, filename_clean)) {
        return(title_map[[key]])
      }
    }
    
    # 如果没找到匹配，使用文件名
    return(tools::toTitleCase(gsub("_", " ", filename_clean)))
  }
  
  # 1. 生成单独的图像文件
  cat("生成单独的图像文件...\n")
  for(i in seq_along(output_tifs)) {
    tif_file <- output_tifs[i]
    tif_path <- file.path(output_folder, tif_file)
    
    # 生成输出文件名
    plot_name <- gsub("\\.tif$", ".png", tif_file)
    plot_path <- file.path(output_folder, plot_name)
    
    tryCatch({
      # 开始绘图设备
      png(plot_path, width = 800, height = 600, res = 150)
      
      # 设置边距
      par(mar = c(4, 4, 3, 6))
      
      # 绘制图像
      plot_raster_with_boundary(
        tif_path, 
        get_plot_title(tif_file),
        get_color_scheme(tif_file),
        boundary
      )
      
      # 关闭绘图设备
      dev.off()
      
      cat("✓ 生成:", plot_name, "\n")
      
    }, error = function(e) {
      cat("✗ 绘图失败:", tif_file, "-", e$message, "\n")
      if(dev.cur() != 1) dev.off()  # 确保关闭设备
    })
  }
  
  # 2. 生成综合图像（多子图）
  cat("\n生成综合展示图像...\n")
  
  # 计算最佳的子图布局
  n_plots <- length(output_tifs)
  if(n_plots <= 4) {
    nrow <- 2; ncol <- 2
  } else if(n_plots <= 6) {
    nrow <- 2; ncol <- 3
  } else if(n_plots <= 9) {
    nrow <- 3; ncol <- 3
  } else if(n_plots <= 12) {
    nrow <- 3; ncol <- 4
  } else {
    nrow <- 4; ncol <- 4
  }
  
  # 生成综合图像
  combined_plot_path <- file.path(output_folder, "all_parameters_combined.png")
  
  tryCatch({
    # 创建大图
    png(combined_plot_path, width = ncol * 400, height = nrow * 300, res = 150)
    
    # 设置多子图布局
    par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 4))
    
    # 绘制每个子图
    for(i in seq_along(output_tifs)) {
      if(i > nrow * ncol) break  # 如果超出布局范围就停止
      
      tif_file <- output_tifs[i]
      tif_path <- file.path(output_folder, tif_file)
      
      plot_raster_with_boundary(
        tif_path, 
        get_plot_title(tif_file),
        get_color_scheme(tif_file),
        boundary
      )
    }
    
    # 如果有剩余的子图位置，留空
    if(n_plots < nrow * ncol) {
      for(i in (n_plots + 1):(nrow * ncol)) {
        plot.new()
      }
    }
    
    # 重置图形参数
    par(mfrow = c(1, 1))
    
    # 关闭设备
    dev.off()
    
    cat("✓ 生成综合图像: all_parameters_combined.png\n")
    
  }, error = function(e) {
    cat("✗ 综合图像生成失败:", e$message, "\n")
    if(dev.cur() != 1) dev.off()
  })
  
  # 3. 生成边界图
  cat("\n生成边界范围图像...\n")
  boundary_plot_path <- file.path(output_folder, "study_area_boundary.png")
  
  tryCatch({
    png(boundary_plot_path, width = 600, height = 600, res = 150)
    
    par(mar = c(4, 4, 3, 2))
    
    # 绘制边界
    boundary_vect <- vect(boundary)
    plot(boundary_vect, 
         main = "Study Area Boundary",
         border = "red", 
         col = "lightblue",
         lwd = 2)
    
    # 添加坐标轴
    axis(1, las = 1)
    axis(2, las = 1)
    box()
    
    # 添加网格
    grid(col = "gray", lty = 2)
    
    dev.off()
    
    cat("✓ 生成边界图像: study_area_boundary.png\n")
    
  }, error = function(e) {
    cat("✗ 边界图像生成失败:", e$message, "\n")
    if(dev.cur() != 1) dev.off()
  })
  
  # 4. 生成图像列表
  cat("\n生成的图像文件:\n")
  plot_files <- list.files(output_folder, pattern = "\\.png$")
  for(plot_file in plot_files) {
    cat("✓", plot_file, "\n")
  }
  
} else {
  cat("没有成功处理的tif文件，跳过可视化\n")
}

# 11. 最终总结
cat("\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("处理完成总结\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("原始文件夹:", target_folder, "\n")
cat("输出文件夹:", output_folder, "\n")
cat("使用的边界文件:", basename(shapefile_path), "\n")
cat("目标坐标系:", target_crs, "\n")
cat("目标分辨率:", target_resolution, "米\n")
cat("空值设置:", na_value, "\n\n")

cat("处理结果:\n")
cat("- 成功处理的tif文件:", success_count, "个\n")
cat("- 处理失败的文件:", error_count, "个\n")

if(success_count > 0) {
  cat("\n生成的输出文件:\n")
  
  # 裁剪后的tif文件
  output_tifs <- list.files(output_folder, pattern = "_cropped\\.tif$")
  cat("栅格数据文件 (", length(output_tifs), "个):\n")
  for(tif in output_tifs) {
    cat("  ✓", tif, "\n")
  }
  
  # 图像文件
  plot_files <- list.files(output_folder, pattern = "\\.png$")
  if(length(plot_files) > 0) {
    cat("\n可视化图像文件 (", length(plot_files), "个):\n")
    for(png in plot_files) {
      cat("  ✓", png, "\n")
    }
  }
  
  # 报告文件
  cat("\n报告文件:\n")
  if(file.exists(file.path(output_folder, "processing_log.txt"))) {
    cat("  ✓ processing_log.txt (处理日志)\n")
  }
  if(file.exists(file.path(output_folder, "processing_summary.csv"))) {
    cat("  ✓ processing_summary.csv (处理汇总表)\n")
  }
}

cat("\n所有文件已保存到:", output_folder, "\n")
cat("可以直接用于MaxEnt建模或其他GIS分析！\n")
cat(paste(rep("=", 80), collapse = ""), "\n")