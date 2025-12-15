########################################### 图a#################################################
####################底图合并
library(terra)  # 新一代栅格处理包，比 raster 快很多
library(sf)     # 处理矢量边界

# ================= 1. 设置文件路径 =================
path_maize <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/CHN_Maize_2019.tif"
path_rice  <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/CHN_Rice_2019.tif"
path_wheat <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/CHN_Wheat_2019.tif"
path_shp   <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/china_202504_shp/provincial_boundary.shp"
path_out   <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/CHN_Total_Staple_Crops_2019.tif" # 输出路径

# ================= 2. 读取数据 =================
r_maize <- rast(path_maize)
r_rice  <- rast(path_rice)
r_wheat <- rast(path_wheat)
chn_shp <- vect(path_shp) # 使用 terra 自带的 vect 读取矢量，速度更快

message("数据读取完成。正在检查空间参考...")

# ================= 3. 空间对齐与重采样 (关键步骤) =================
# 我们以玉米 (Maize) 为【基准图层】。
# 如果水稻和小麦的网格跟玉米不完全重合（行列数、分辨率有微小差异），R无法直接相加。
# 这里强制将水稻和小麦“对齐”到玉米的网格上。

# 3.1 处理空值：将 NA 替换为 0，否则相加时会造成数据丢失
r_maize[is.na(r_maize)] <- 0
r_rice[is.na(r_rice)]   <- 0
r_wheat[is.na(r_wheat)] <- 0

# 3.2 检查并重采样 (Resample)
# 如果 r_rice 的几何参数和 r_maize 不一样，就进行重采样
if (!compareGeom(r_maize, r_rice, stopOnError = FALSE)) {
  message("正在对齐水稻数据到基准网格...")
  r_rice <- resample(r_rice, r_maize, method = "bilinear") # 线性插值
}

if (!compareGeom(r_maize, r_wheat, stopOnError = FALSE)) {
  message("正在对齐小麦数据到基准网格...")
  r_wheat <- resample(r_wheat, r_maize, method = "bilinear")
}

# ================= 4. 叠加计算 =================
message("正在进行栅格叠加计算...")

# 直接矩阵相加：三大主粮总和
r_total <- r_maize + r_rice + r_wheat

# ================= 5. 按地图掩膜 (Mask) =================
# 这一步是为了让结果只保留在中国版图内，把海上或国界外的 0 值切掉
message("正在使用省界进行掩膜裁剪...")

# 确保矢量和栅格投影一致
if (crs(chn_shp) != crs(r_total)) {
  chn_shp <- project(chn_shp, crs(r_total))
}

# 裁剪 (Crop) 和 掩膜 (Mask)
r_final <- crop(r_total, chn_shp)
r_final <- mask(r_final, chn_shp)

# 重命名图层内部名称
names(r_final) <- "Total_Staple_Crop_Distribution"

# ================= 6. 输出为 GeoTIFF =================
message("正在保存为 .tif 文件...")

writeRaster(r_final, path_out, overwrite = TRUE)

message(paste("处理完成！文件已保存至：", path_out))








####################aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa#######################################
####################aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa#######################################
####################aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa#######################################
library(tidyverse)
library(data.table)
library(sf)
library(ggplot2)
library(readxl)
library(cowplot)
library(terra)
library(tidyterra)

# ================= 1. 数据准备 =================

# 1.1 读取采样点
site <- readxl::read_xlsx("E:/After phD/paper 6/meta_paper6/作图数据&代码/Standardized_organic_fertilizer_data.xlsx", sheet = 1)
site <- as.data.table(site)
site[crop_type == 'wheat', crop_type := 'Wheat']
site[crop_type == 'maize', crop_type := 'Maize']
site[crop_type == 'rice', crop_type := 'Rice']
names(site)[names(site) == "crop_type"] <- "Crop type"
site$`Crop type` <- factor(site$`Crop type`, levels = c('Wheat', 'Maize', 'Rice'))

# 1.2 地图与栅格读取
china_map <- st_read("E:/After phD/paper 6/meta_paper6/作图数据&代码/china_202504_shp/provincial_boundary.shp")
tif_path <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/CHN_Total_Staple_Crops_2019.tif"
r_total <- rast(tif_path)

# 投影转换 (保持不变)
if (!is.lonlat(r_total)) { r_total <- project(r_total, "EPSG:4326") }
if (st_crs(china_map)$epsg != 4326) { china_map <- st_transform(china_map, 4326) }

# ================= 【核心修改】定义更好看的颜色和形状 =================
# 颜色：选用柔和且区分度高的科研配色
my_colors <- c("Wheat" = "#D6604D", # 柔和红
               "Maize" = "#5AAE61", # 清新绿
               "Rice"  = "#2166AC") # 稳重蓝

# 形状：选用实心符号 (15=方, 17=三角, 16=圆)
my_shapes <- c("Wheat" = 15, "Maize" = 17, "Rice" = 16)

# ================= 2. 绘制主图 =================
p_main <- ggplot() +
  # --- 第1层：栅格底图 ---
  geom_spatraster(data = r_total, maxcell = 2e6) + 
  scale_fill_gradientn(
    colors = c("white", "#4DBBD5", "#006D87"), # 青蓝渐变底图
    na.value = "transparent",
    guide = "none"
  ) +
  
  # --- 第2层：省界 ---
  geom_sf(data = china_map, fill = NA, color = "#838B8B", size = 0.1) +
  
  # --- 第3层：【回归】使用 geom_point 绘制符号 ---
  geom_point(data = site,
             aes(x = lon, y = lat, color = `Crop type`, shape = `Crop type`),
             alpha = 0.9, # 稍微一点点透明度，避免点重叠时糊成一片
             size = 1.5) + # 调整点的大小，2.5 比较适中
  
  # --- 样式调整 ---
  # 同时设置颜色和形状映射
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  
  coord_sf(ylim = c(17.5, 54), xlim = c(73, 135), expand = FALSE) + 
  
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        
        # 图例设置 (调整位置和样式)
        legend.position = c(0.1, 0.15), 
        legend.text = element_text(size = 10, face = "bold", color = "black", family = "Arial"),
        legend.title = element_text(face = "bold", size = 12),
        legend.background = element_blank(),
        # 增加图例项之间的间距
        legend.key.size = unit(1.5, "lines")) +
  
  # 合并颜色和形状图例，并放大图例中的符号
  guides(
    color = guide_legend(title = NULL, override.aes = list(size = 3)),
    shape = guide_legend(title = NULL, override.aes = list(size = 3))
  )

# ================= 3. 绘制插图 =================
p_sub <- ggplot() +
  geom_spatraster(data = r_total, maxcell = 2e6) + 
  scale_fill_gradientn(
    colors = c("white", "#4DBBD5", "#006D87"),
    na.value = "transparent",
    guide = "none"
  ) +
  geom_sf(data = china_map, fill = NA, color = "#838B8B", size = 0.1) +
  
  # 插图的点也相应调整
  geom_point(data = site,
             aes(x = lon, y = lat, color = `Crop type`, shape = `Crop type`),
             alpha = 0.9, size = 1) + # 插图的点稍微小一点
  
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  
  coord_sf(xlim = c(106, 124), ylim = c(2, 25), expand = FALSE) +
  
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "#838B88", fill = NA, size = 0.5), 
        plot.margin = margin(0, 0, 0, 0))

# ================= 4. 拼图与保存 =================
final_plot <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_sub, x = 0.85, y = 0.08, width = 0.1, height = 0.2) 

ggsave(final_plot, file = "E:/After phD/paper 6/meta_paper6/作图数据&代码/p1_shape_map.png",
       width = 200, height = 140, dpi = 600, units = "mm", bg = "white")



########################################### 图b #################################################


# 图b
library(data.table)
library(readxl)
library(ggplot2)
library(patchwork)

# -------------------------
# 1. 路径配置
# -------------------------
file_path <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/meta_result.xlsx"
out_file  <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/Forest_Plot.png"

# -------------------------
# 2. 读取数据
# -------------------------
data1 <- read_xlsx(file_path, sheet = 2)
data1 <- as.data.table(data1)
data1 <- data1[, .(group, label, mean = mean.V1, ci.lb, ci.ub, ci.lb_99, ci.ub_99)]

data2 <- read_xlsx(file_path, sheet = 3)
data2 <- as.data.table(data2)
data2 <- data2[, .(group, label, mean = mean.V1, ci.lb, ci.ub, ci.lb_99, ci.ub_99)]

# -------------------------
# 3. 分组重命名与排序
# -------------------------

# === 处理 Data 1 (Yield) ===
data1[label %like% "Wheat" | label %like% "Maize" | label %like% "Rice", group := "Crop type"]
data1[label %like% "0-100" | label %like% "100-200" | label %like% "200-300" | label %like% ">300", group := "N rate (kg N/ha)"]
data1[label %like% "0-5" | label %like% "5-10" | label %like% "10-15" | label %like% ">15", group := "Duration (year)"]
data1[label %like% "Manure" | label %like% "Other", group := "Fertilizer type"]
data1[label %like% "All", group := "Overall"]

# Duration 强制排序
data1[, sort_order := 99] 
data1[group == "Duration (year)" & label %like% "0-5",   sort_order := 1]
data1[group == "Duration (year)" & label %like% "5-10",  sort_order := 2]
data1[group == "Duration (year)" & label %like% "10-15", sort_order := 3]
data1[group == "Duration (year)" & label %like% ">15",   sort_order := 4]

data1 <- data1[order(group, sort_order, label)]
data1$label_id <- seq_len(nrow(data1))
data1$label_plotmath <- factor(data1$label, levels = data1$label[order(data1$label_id)])

# === 处理 Data 2 (Soil Health) ===
# 修正化学式标签 (NH₄⁺-N, NO₃⁻-N)
data2[, label_plotmath := label]
data2[label %like% "NH4", label_plotmath := gsub("NH4", "NH\u2084\u207A-N", label)]
data2[label %like% "NO3", label_plotmath := gsub("NO3", "NO\u2083\u207B-N", label)]

data2[, pure_label := gsub("\\n.*| \\(.*", "", label)] 

list_enzyme <- c("AKP", "βG", "BG") 
list_microbial_act <- c("B_R", "B_S", "F_R", "F_S")

# 分组名插入换行符
data2[pure_label %in% list_enzyme, group := "Soil enzyme\nactivity"]
data2[pure_label %in% list_microbial_act, group := "Soil microbial activity"]
data2[!(pure_label %in% list_enzyme) & !(pure_label %in% list_microbial_act), group := "Soil property"]

target_order <- c("Soil microbial activity", "Soil enzyme\nactivity", "Soil property")
data2[, group := factor(group, levels = target_order)]
data2 <- data2[order(group, label)]
data2$label_id <- seq_len(nrow(data2))
data2$label_plotmath <- factor(data2$label_plotmath, levels = data2$label_plotmath[order(data2$label_id)])

# -------------------------
# 4. 颜色映射
# -------------------------
base_colors1 <- c(
  "Crop type"        = "#E8A64A", 
  "N rate (kg N/ha)" = "#3C99C9", 
  "Duration (year)"  = "#557D29", 
  "Fertilizer type"  = "#B06BB0", 
  "Overall"          = "grey50"   
)

base_colors2 <- c(
  "Soil microbial activity" = "#386881", 
  "Soil enzyme\nactivity"    = "#E8A64A",
  "Soil property"           = "#A0522D"  
)

apply_gradient_colors <- function(df, base_colors) {
  df$color <- NA_character_
  for (g in unique(df$group)) {
    base_col <- base_colors[as.character(g)]
    if (is.na(base_col)) base_col <- "black"
    n_items <- nrow(df[group == g])
    if (n_items == 1) {
      col_func <- colorRampPalette(c(base_col, "white"))
      df[group == g, color := col_func(10)[3]] 
    } else {
      long_palette_func <- colorRampPalette(c(base_col, "white"))
      if (g == "Soil property") {
        total_steps <- n_items + 3
        full_colors <- long_palette_func(total_steps)
        selected_colors <- full_colors[1:n_items]
      } else {
        total_steps <- n_items + 4 
        full_colors <- long_palette_func(total_steps)
        selected_colors <- full_colors[2:(n_items + 1)]
      }
      df[group == g, color := selected_colors]
    }
  }
  return(df)
}

data1 <- apply_gradient_colors(data1, base_colors1)
data2 <- apply_gradient_colors(data2, base_colors2)

# -------------------------
# 5. 生成分组标签
# -------------------------
generate_group_anno <- function(df) {
  global_y_max <- max(df$ci.ub_99, na.rm = TRUE) * 1.15 
  group_ranges <- df[, .(min_id = min(label_id), max_id = max(label_id)), by = group]
  group_labels <- group_ranges[, .(xpos = (min_id + max_id) / 2, ypos = global_y_max, group = group)]
  bg_blocks <- group_ranges[, .(xmin = min_id - 0.4, xmax = max_id + 0.4, fill = ifelse(seq_len(.N) %% 2 == 0, "#F2F2F2", "#F8F8F8"))]
  return(list(group_labels = group_labels, bg_blocks = bg_blocks))
}

group_anno1 <- generate_group_anno(data1)
group_anno2 <- generate_group_anno(data2)

# ========== 核心修改：调整横(xpos)、纵(ypos)位置 ==========
group_anno2$group_labels[group == "Soil enzyme\nactivity", 
                         `:=`(
                           xpos = xpos,        # 横向不变
                           ypos = ypos     # 纵向下移5
                         )]  

# -------------------------
# 6. 绘图函数（关键修复：放开y轴上限 + 调整对齐）
# -------------------------
plot_forest_horizontal_each <- function(df, group_labels, bg_blocks, ylab) {
  # 修复1：重新计算y轴上限，包含下移后的标签ypos
  all_y_vals <- c(df$ci.ub_99, group_labels$ypos)  # 加入分组标签的ypos
  y_limit_max <- max(all_y_vals, na.rm = TRUE) * 1.05  # 基于所有y值计算上限
  y_limit_min <- min(c(df$ci.lb_99, 0), na.rm = TRUE) * 1.1
  
  p <- ggplot(df, aes(x = label_id, y = mean)) +
    geom_rect(data = bg_blocks, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), inherit.aes = FALSE, alpha = 0.5) +
    scale_fill_identity() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.3) +
    
    geom_linerange(aes(ymin = ci.lb_99, ymax = ci.ub_99, color = I(color)), linewidth = 0.6, alpha = 0.8) +
    geom_linerange(aes(ymin = ci.lb, ymax = ci.ub, color = I(color)), linewidth = 1.5) +
    
    geom_point(aes(fill = I(color)), color = "white", shape = 21, size = 3, stroke = 0.8) +
    
    # 修复2：调整vjust=1（底部对齐ypos），确保下移后的标签可见
    geom_text(data = group_labels, aes(x = xpos, y = ypos, label = group),
              inherit.aes = FALSE, 
              vjust = 1,        # 关键：标签底部对齐ypos（下移后能完整显示）
              hjust = 0.5, 
              size = 8 / .pt, 
              family = "Arial", 
              fontface = "bold", 
              lineheight = 1.0) +
    
    scale_x_continuous(breaks = df$label_id, labels = df$label_plotmath, expand = expansion(add = 0.5)) +
    scale_y_continuous(limits = c(y_limit_min, y_limit_max), expand = expansion(mult = 0.1)) +  # 补充少量扩展
    
    labs(x = NULL, y = ylab) +
    theme_minimal(base_family = "Arial") +
    theme(
      text = element_text(size = 8, color = "black", family = "Arial"),
      axis.text.x = element_text(size = 8, color = "black", family = "Arial", lineheight = 1.2),
      axis.text.y = element_text(size = 8, family = "Arial", color = "black"),
      axis.title.y = element_text(size = 8, family = "Arial", face = "bold"),
      panel.grid = element_blank(),
      plot.margin = margin(5, 5, 5, 5),  # 可改为margin(10,5,10,5)增加上下边距
      panel.border = element_rect(colour = "gray40", fill = NA, linewidth = 0.5),
      legend.position = "none"
    )
  return(p)
}

# -------------------------
# 7. 组合与保存
# -------------------------
p1 <- plot_forest_horizontal_each(data1, group_anno1$group_labels, group_anno1$bg_blocks, "Change in Crop yield (%)")
p1 <- p1 + labs(tag = "")

p2 <- plot_forest_horizontal_each(data2, group_anno2$group_labels, group_anno2$bg_blocks, "Change in Soil health indicators (%)")

p_combined <- p1 / p2 &
  theme(plot.tag = element_text(family = "Arial", size = 16, face = "bold"))

ggsave(
  filename = out_file,
  plot = p_combined,
  width = 18, height = 12, units = "cm",
  dpi = 600, device = "png"
)

print(paste("绘图完成！已调整Soil enzyme\\nactivity的横/纵位置:", out_file))


# 
# 
# 
# 
# library(ggplot2)
# library(cowplot)
# library(magick)
# 
# # ================= 1. 读取现有的图片 =================
# img_path1 <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/p1_shape_map.png"  # 上图
# img_path2 <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/Forest_Plot.png"    # 下图
# 
# # 读取图片
# p1_img <- ggdraw() + draw_image(img_path1)
# p2_img <- ggdraw() + draw_image(img_path2)
# 
# # ================= 2. 组合图片 =================
# combined_plot <- plot_grid(
#   p1_img, p2_img,
#   ncol = 1,               # 上下排列
#   labels = c("a", "b"),   # 标号
#   label_size = 12,        # 字号 10
#   label_fontfamily = "Arial",
#   label_colour = "black",
#   label_fontface = "bold" ,
# 
#   # 【位置设置】：标号定位在图内左上角
#   label_x = 0,
#   label_y = 1,
# 
#   # 【微调】：通过负边距把字“挤”进图里
#   hjust = -0.5, # 向右移一点
#   vjust = 1.5,  # 向下移一点
# 
#   # 【比例设置】：默认高度1:1，如需调整可改这里
#   rel_heights = c(1, 1)
# )
# 
# 
# # ================= 3. 保存 (核心修改：背景设为白色) =================
# ggsave("E:/After phD/paper 6/meta_paper6/作图数据&代码/Combined_Figure.png", 
#        combined_plot, 
#        width = 180,       # 宽度锁定 180mm (18cm)
#        height = 220,      # 高度可根据需要微调
#        units = "mm", 
#        dpi = 600,
#        
#        # 【关键】：强制背景为白色，填补所有透明空隙
#        bg = "white")
# 
# 




##########################组合
library(ggplot2)
library(cowplot)
library(magick)

# ================= 1. 读取现有的图片 =================
img_path1 <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/p1_shape_map.png"  # 上图
img_path2 <- "E:/After phD/paper 6/meta_paper6/作图数据&代码/Forest_Plot.png"    # 下图

# 读取图片
p1_img <- ggdraw() + draw_image(img_path1)
p2_img <- ggdraw() + draw_image(img_path2)

# ================= 2. 组合图片 =================
combined_plot <- plot_grid(
  p1_img, p2_img,
  ncol = 1,               # 上下排列
  labels = c("a", "b"),   # 标号
  label_size = 12,        # 字号 10
  label_fontfamily = "Arial", 
  label_colour = "black",
  label_fontface = "bold",
  
  # 【位置设置】：标号定位在图内左上角
  label_x = 0.02,   
  label_y = 1,
  
  # 【微调】：通过负边距把字“挤”进图里
  hjust = -0.5, # 向右移一点
  vjust = 1.5,  # 向下移一点
  
  # 【比例设置】：默认高度1:1，如需调整可改这里
  rel_heights = c(1, 1)   
)

# ================= 3. 保存 (核心修改：背景设为白色) =================
ggsave("E:/After phD/paper 6/meta_paper6/作图数据&代码/Combined_Figure.png", 
       combined_plot, 
       width = 180,       # 宽度锁定 180mm (18cm)
       height = 220,      # 高度可根据需要微调
       units = "mm", 
       dpi = 600,
       
       # 【关键】：强制背景为白色，填补所有透明空隙
       bg = "white")