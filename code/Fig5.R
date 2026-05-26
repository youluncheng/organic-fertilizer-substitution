library(tidyverse)
library(readxl)
library(ggrepel)
library(GGally)      
library(patchwork)   
library(reshape2)
library(RColorBrewer)
library(ggplot2)

asi_raw <- read_excel("...Data/Fig5.xlsx")

positive_vars <- c("Yield (t/ha)", "ISHI")  
negative_vars <- c("CV", "Nr loss (kg N/ha)", "GHG emissions (kg CO₂-eq/ha)") 

asi_raw <- asi_raw %>%
  mutate(across(all_of(positive_vars), ~ (.-min(.))/(max(.)-min(.)), .names = "{.col}_norm")) %>%
  mutate(across(all_of(negative_vars), ~ (max(.)-.)/(max(.)-min(.)), .names = "{.col}_norm"))

norm_vars <- c(paste0(positive_vars, "_norm"), paste0(negative_vars, "_norm"))
asi_raw <- asi_raw %>%
  mutate(ASI = rowMeans(select(., all_of(norm_vars))))

asi_final <- asi_raw %>%
  select(Treatment, all_of(norm_vars), ASI) %>%
  arrange(desc(ASI)) %>%
  mutate(Treatment = factor(Treatment, levels = Treatment))  

col_rename <- c(
  "Yield (t/ha)_norm" = "Yield",
  "ISHI_norm" = "ISHI",
  "CV_norm" = "Stability",
  "Nr loss (kg N/ha)_norm" = "Nr loss",
  "GHG emissions (kg CO₂-eq/ha)_norm" = "GHG"
)
names(asi_final) <- ifelse(names(asi_final) %in% names(col_rename),
                           col_rename[names(asi_final)],
                           names(asi_final))

library(ggplot2)
library(ggrepel)
library(grid)

gradient_background <- rasterGrob(
  colorRampPalette(c("white", "#f0f8ff"))(100),  
  width = unit(1, "npc"), 
  height = unit(1, "npc"),
  interpolate = TRUE
)

p1 <- ggplot(asi_final, aes(x = Treatment, y = ASI, color = ASI)) +
  annotation_custom(gradient_background, 
                    xmin = -Inf, xmax = Inf, 
                    ymin = -Inf, ymax = Inf) +  
  geom_point(size = 5) +
  geom_line(aes(group = 1), linetype = "dashed", color = "grey60") +
  scale_color_gradient(low = "blue", high = "red") +
  geom_text_repel(
    aes(label = round(ASI, 2)),
    nudge_y = 0.04,
    point.padding = 0.5,
    box.padding = 0.5,
    size = 4
  ) +
  labs(x = NULL, y = "Agricultural sustainability index") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(color = "black"),
    panel.border = element_rect(color = "#838B8B", fill = NA, linewidth = 1), 
    panel.background = element_rect(fill = NA, color = NA),  
    plot.background = element_rect(fill = NA, color = NA),   
    panel.grid.major = element_line(color = "grey80"),      
    panel.grid.minor = element_line(color = "grey90"),      
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.position = "none"
  )


p1

heatmap_data <- asi_final %>%
  column_to_rownames(var = "Treatment") %>%
  select(-ASI) %>%
  as.matrix()

heatmap_df <- as.data.frame(heatmap_data) %>%
  rownames_to_column(var = "Treatment") %>%
  pivot_longer(-Treatment, names_to = "Indicator", values_to = "Value")

p2 <- ggplot(heatmap_df, aes(x = Indicator, y = Treatment, fill = Value)) +
  geom_tile(color = "white", size=0.5) +
  geom_text(aes(label = round(Value, 2)), color="black", size=4) +
  scale_fill_gradientn(colours = c("#e0f3f8", "#abd9e9", "#74add1", "#4575b4"), limits=c(0,1)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(color = "black"),  
    axis.text.x = element_text(angle = 30, hjust = 0.5, size = 10, color = "black"), 
    axis.text.y = element_text(size = 10, color = "black"), 
    panel.grid = element_blank(),
    legend.position = "none"  
  )

p2

pairs_data <- asi_final %>%
  select(Yield, ISHI, Stability, `Nr loss`, `GHG`)

p3 <- ggpairs(
  pairs_data,
  upper = list(continuous = wrap("cor", size = 3, color = "#cd661d", alpha = 1, digits = 2,
                                 mapping = aes(color = ..r..))),
  lower = list(continuous = wrap("points", alpha = 0.6, size = 1, color = "steelblue")),
  diag = list(continuous = wrap("densityDiag", fill = "lightblue", alpha = 0.8))
) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(
    text = element_text(color = "black"),  
    panel.grid = element_blank(),
    strip.text = element_text(size = 9, face = "bold", family = "Arial"),
    strip.background = element_rect(fill = "#F0FFFF", color = NA), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black", family = "Arial"), 
    axis.text.y = element_text(size = 10, color = "black", family = "Arial") 
  )

for(i in 1:p3$nrow){
  for(j in 1:p3$ncol){
    p3[i,j] <- p3[i,j] + 
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"))
  }
}

p3

library(cowplot)
library(grid)

p1_grob <- ggplotGrob(p1)
p2_grob <- ggplotGrob(p2)
p3_grob <- GGally::ggmatrix_gtable(p3)

p_bottom_grob <- plot_grid(
  p2_grob, p3_grob, 
  ncol = 2, rel_widths = c(1, 1.2),
  labels = c("b", "c"),    
  label_size = 12,         
  label_fontface = "bold",  
  label_y = 1.01,           
  label_x = -0.008           
)

p_final <- plot_grid(
  p1_grob, p_bottom_grob,
  ncol = 1, rel_heights = c(1, 1.4),
  labels = c("a", ""),     
  label_size = 12,
  label_fontface = "bold",
  label_y = 1.01,            
  label_x = -0.008             
)
