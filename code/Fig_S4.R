library(dplyr)
library(stringr)
library(ggradar)
library(scales)
library(tibble)
library(ggplot2)
library(patchwork)
library(extrafont)

loadfonts(device = "win") 

df <- read.csv("...Data/FigS6.csv")
df$Treatment <- str_extract(df$Treatment, "^[A-Za-z]+\\d*")

my_colors <- c(
  "CK" = "#666666",
  "O1" = "#4d9221", "O2" = "#4d9221", "O3" = "#4d9221", "O4" = "#4d9221",
  "F1" = "#c51b7d", "F2" = "#c51b7d", "F3" = "#c51b7d", "F4" = "#c51b7d"
)

make_radar <- function(dat, group_order, colors, axis_labels = NULL, title = NULL, show_title = TRUE) {
  radar_df <- dat %>%
    filter(Treatment %in% group_order) %>%
    mutate(Treatment = factor(Treatment, levels = group_order)) %>%
    group_by(Treatment) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  radar_mat <- as.matrix(radar_df[, -1])
  rownames(radar_mat) <- radar_df$Treatment
  
  radar_data <- rbind(
    rep(1, ncol(radar_mat)),
    rep(0, ncol(radar_mat)),
    radar_mat
  ) %>%
    as.data.frame() %>%
    mutate(group = c(NA, NA, rownames(radar_mat))) %>%
    relocate(group)
  
  radar_data$group <- factor(radar_data$group, levels = group_order)
  
  if (!is.null(axis_labels)) {
    colnames(radar_data)[-1] <- axis_labels
  }
  
  # 绘图部分
  p <- ggradar(
    radar_data,                                         
    values.radar = c("0", "0.5", "1.0"),               
    grid.min = 0, grid.mid = 0.5, grid.max = 1,        
    group.line.width = 0.5,                              
    group.point.size = 1,                              
    font.radar = "Arial",                              
    legend.position = "none",                          
    axis.label.size = 2,                               
    grid.label.size = 1.5,                             
    group.colours = colors[group_order],                
    background.circle.colour = "transparent",          
    gridline.min.colour = "#eeeeee",   
    gridline.mid.colour = "#cccccc",  
    gridline.max.colour = "#999999"   
  ) +
    coord_cartesian(clip = 'off') +                    
    xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +                 
    theme(
      plot.margin = margin(0, 0, 0, 0),               
      panel.background = element_blank()               
    )

  if (show_title) {
    p <- p + ggtitle(title) +
      theme(
        plot.title = element_text(
          family = "Arial", size = 12, hjust = 0.5, face = "plain", color = "black",
          margin = margin(b = 5)
        )
      )
  }
  
  return(p)
}

# Step 4: 定义轴标签
axis_labels_soil <- c("pH", "SOC", "TN", "BD", "NH₄⁺-N", "NO₃⁻-N", "AP", "AK", "MWD", "MBC", "MBN")
axis_labels_micro <- c("B_R", "B_S", "B_P", "F_R", "F_S", "F_P", "P_R", "P_S", "P_P")
axis_labels_n_transform <- c("M", "I_NH₄", "I_NO₃", "O_Norg", "O_NH₄")
axis_labels_gene_abundance <- c("AOA_A", "AOB_A", "Com_A", "nirS_A", "nirK_A", "nosZ_A")
axis_labels_gene_diversity <- c("AOA_R", "AOA_S", "AOB_R", "AOB_S", "Com_R", "Com_S")
axis_labels_enzyme <- c("AKP", "βG", "NAG")

generate_category_plots <- function(data_cols, axis_labels, show_titles = FALSE) {
  sub_df <- df %>% select(Treatment, all_of(data_cols))
  titles <- c("O1 vs F1", "O2 vs F2", "O3 vs F3", "O4 vs F4")
  p1 <- make_radar(sub_df, c("CK", "O1", "F1"), my_colors, axis_labels, ifelse(show_titles, titles[1], NULL), show_titles)
  p2 <- make_radar(sub_df, c("CK", "O2", "F2"), my_colors, axis_labels, ifelse(show_titles, titles[2], NULL), show_titles)
  p3 <- make_radar(sub_df, c("CK", "O3", "F3"), my_colors, axis_labels, ifelse(show_titles, titles[3], NULL), show_titles)
  p4 <- make_radar(sub_df, c("CK", "O4", "F4"), my_colors, axis_labels, ifelse(show_titles, titles[4], NULL), show_titles)
  return(list(p1, p2, p3, p4))
}

row1 <- generate_category_plots(2:12, axis_labels_soil, show_titles = TRUE)  
row2 <- generate_category_plots(13:21, axis_labels_micro)
row3 <- generate_category_plots(22:26, axis_labels_n_transform)
row4 <- generate_category_plots(27:32, axis_labels_gene_abundance)
row5 <- generate_category_plots(33:38, axis_labels_gene_diversity)
row6 <- generate_category_plots(39:41, axis_labels_enzyme)

create_legend <- function() {

  legend_data <- data.frame(
    x = c(1, 2, 3),                                 
    y = 1,                                         
    Group = factor(c("CK", "O", "F"),              
                   levels = c("CK", "O", "F"))
  )
  
  ggplot(legend_data, aes(x = x, y = y, color = Group)) +  
    geom_line(aes(group = Group), size = 1.5) +            
    scale_color_manual(values = c(                         
      "CK" = "#666666",   
      "O" = "#4d9221",    
      "F" = "#c51b7d"     
    )) +
    theme_void() +                                          
    theme(
      legend.position = "bottom",                          
      legend.title = element_blank(),                      
      legend.text = element_text(size = 11, family = "Arial"), 
      legend.key.width = unit(1.5, "cm"),                    
      legend.key.height = unit(0.01, "cm"),                  
      plot.margin = margin(0, 0, 0, 0)                       
    ) +
    guides(color = guide_legend(
      direction = "horizontal",                             
      nrow = 1,                                             
      override.aes = list(size = 0.1)                       
    ))
}

legend_plot <- create_legend()  
full_main <- wrap_plots(row1, ncol = 4) / 
  wrap_plots(row2, ncol = 4) / 
  wrap_plots(row6, ncol = 4) / 
  wrap_plots(row3, ncol = 4) / 
  wrap_plots(row4, ncol = 4) / 
  wrap_plots(row5, ncol = 4) 

full_plot <- full_main / wrap_plots(legend_plot) +
  plot_layout(
    heights = c(rep(4, 6), 0.8),  
    widths = c(1, 1, 1, 1)        
  ) &
  theme(
    plot.margin = margin(0),
    panel.spacing = unit(0.2, "cm") 
  )

print(full_plot)

