library(data.table)
library(readxl)
library(ggplot2)
library(patchwork)

file_path <- "...Data/Fig4.xlsx"

data1 <- read_xlsx(file_path, sheet = x) |> as.data.table()
data1 <- data1[, .(group, label, mean = mean.V1, ci.lb, ci.ub)]
data1$label <- factor(data1$label, levels = rev(unique(data1$label)))
data1 <- data1[order(group, label)]
data1$label_id <- seq_len(nrow(data1))
data1[, label_plotmath := label] 

group_labels1 <- data.table(
  group = c("overall","Fertilizer type", "Duration (year)", "N rate (kg N/ha)", "Crop type"),
  x = rep(20, 5),
  y = c(14.5,13.5, 11.5, 7.5, 3.5)
)

data2 <- read_xlsx(file_path, sheet = x) |> as.data.table()
data2 <- data2[, .(group, label, mean = mean.V1, ci.lb, ci.ub)]

data2[label == "NH4 (77)",  label_plotmath := "NH\u2084\u207A-N (n=77)"]
data2[label == "NO3 (82)",  label_plotmath := "NO\u2083\u207B-N (n=82)"]

data2[is.na(label_plotmath), label_plotmath := label]

data2$label <- factor(data2$label, levels = rev(unique(data2$label)))
data2$label_plotmath <- factor(data2$label_plotmath, levels = rev(unique(data2$label_plotmath))) 
data2 <- data2[order(factor(label, levels = unique(label)))]
data2$label_id <- seq_len(nrow(data2))

data2$group <- c(rep("Soil property", 11),
                 rep("Soil enzyme activity", 2),
                 rep("Soil microbial activity", 4))

group_labels2 <- data.table(
  group = c("Soil property", "Soil enzyme activity", "Soil microbial activity"),
  x = rep(64, 3),
  y = c(17.5, 6.5, 4.5)
)


plot_forest_custom_group_label <- function(df, group_labels, bg_blocks, title_text, xlab = "Change (%)", header_height = 0) {
  df[, y_position := label_id]
  
  p <- ggplot(df, aes(x = mean, y = label_plotmath)) +

    geom_rect(data = bg_blocks, aes(ymin = ymin, ymax = ymax, fill = fill),
              xmin = -Inf, xmax = Inf, inherit.aes = FALSE, alpha = 0.4) +
    scale_fill_identity() +
    
    geom_hline(data = group_labels, aes(yintercept = y),
               linetype = "dashed", color = "grey85", size = 0.5) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    
    annotate("rect",
             ymin = max(df$label_id) + 0.5,
             ymax = max(df$label_id) + 0.5 + header_height,
             xmin = -Inf, xmax = Inf,
             fill = "gray80", color = "gray40") +
    
    geom_point(size = 2, color = "steelblue") +
    geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, color = "gray10") +
    
    geom_text(data = group_labels,
              aes(x = x, y = y, label = group),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.05, fontface = "plain", size = 3.5,
              x = Inf, color = "black", family = "Arial") +
    
    labs(x = xlab, y = NULL, title = title_text) +
    
    theme(
      text = element_text(family = "Arial", size = 9, color = "black"),  # 统一字体样式
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.ticks = element_line(color = "gray40"),
      plot.title = element_blank(),
      axis.text.y = element_text(size = 9, color = "black", family = "Arial"),
      axis.text.x = element_text(size = 9, color = "black", family = "Arial"),
      axis.title.x = element_text(size = 9, color = "black", family = "Arial"),
      plot.margin = margin(30, 5, 10, 5),
      panel.border = element_rect(colour = "gray40", fill = NA, linewidth = 0.5)
    ) +
    coord_cartesian(clip = "off")
  
  if (any(grepl("\\^|\\~|\\*", df$label_plotmath))) {
    p <- p + scale_y_discrete(labels = function(x) parse(text = x),
                              expand = expansion(add = 0.5))
  } else {
    p <- p + scale_y_discrete(expand = expansion(add = 0.5))
  }
  
  return(p)
}

bg_blocks1 <- data.table(
  ymin = c(0.5, 3.5, 7.5, 11.5, 13.5),
  ymax = c(3.5, 7.5, 11.5, 13.5, 14.5),
  fill = c("#B5DDE6", "#DAE5E8", "#B5DDE6", "#DAE5E8", "#B5DDE6")
)

bg_blocks2 <- data.table(
  ymin = c(0.5, 4.5, 6.5),
  ymax = c(4.5, 6.5, 17.5),
  fill = c("#B5DDE6", "#DAE5E8", "#B5DDE6")
)

p1 <- plot_forest_custom_group_label(
  data1, group_labels1, bg_blocks1,
  title_text = "", xlab = "Change in Crop yield (%)", header_height = 0
)

p2 <- plot_forest_custom_group_label(
  data2, group_labels2, bg_blocks2,
  title_text = "", xlab = "Change in Soil health indicators (%)", header_height = 0
)

library(patchwork)

p_combined <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    tag_levels = "a"
  ) &
  theme(
    plot.tag = element_text(
      family = "Arial",
      size = 12,
      face = "bold",
      color = "black"
    )
  )

print(p_combined)

