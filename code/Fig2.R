
##########################################fig2##############################################

library(dplyr)
library(stringr)
library(ggradar)
library(scales)
library(tibble)
library(ggplot2)
library(ggradar)

data <- readxl::read_xlsx('...Data/Fig2.xlsx',sheet = x)
d1_wide <- as.data.table(data)

############## Soil properties ##########################
d1_wide_soil <- d1_wide %>%
  select(1:12)

d1_wide_soil <- d1_wide_soil %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_soil <- d1_wide_soil %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_soil <- d1_radar_soil$Treatment

rownames(d1_radar_soil) <- d1_radar_soil$Treatment
d1_radar_soil <- d1_radar_soil[, -1]

num_vars <- ncol(d1_radar_soil)
print(paste("Number of variables:", num_vars))

axis_labels <- c(
  "pH", "SOC", "TN", "BD",
  "NH\u2084\u207A-N",  # NH₄⁺-N
  "NO\u2083\u207B-N",  # NO₃⁻-N
  "AP", "AK", "MWD", "MBC", "MBN"
)

if(length(axis_labels) != num_vars) {
  stop(paste("Number of axis labels (", length(axis_labels), 
             ") does not match number of variables (", num_vars, ")"))
}

d1_radar_soil <- rbind(
  rep(1, ncol(d1_radar_soil)),
  rep(0, ncol(d1_radar_soil)),
  d1_radar_soil
)

d1_radar_soil <- d1_radar_soil %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_soil)) %>%
  relocate(group)

p1 <- ggradar(d1_radar_soil,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none", 
              axis.labels = axis_labels) +
  ggtitle("Soil properties") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "sans"),  
        axis.text = element_text(family = "Arial", color = "black"),  
        axis.title = element_text(family = "Arial"),  
        legend.text = element_text(family = "Arial"),  
        legend.title = element_text(family = "Arial"),  
        grid.text = element_text(family = "Arial"),  
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))  

print(p1)

############## Soil microbial diversity ##########################
d1_wide_micro <- d1_wide %>%
  select(1, 13:21)

d1_wide_micro <- d1_wide_micro %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_micro <- d1_wide_micro %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_micro <- d1_radar_micro$Treatment

rownames(d1_radar_micro) <- d1_radar_micro$Treatment
d1_radar_micro <- d1_radar_micro[, -1]

num_vars_micro <- ncol(d1_radar_micro)

axis_labels_micro <- c(
  "B_R", "B_S", "B_P",
  "F_R", "F_S", "F_P",
  "P_R", "P_S", "P_P"
)

if(length(axis_labels_micro) != num_vars_micro) {
  stop(paste("Number of axis labels (", length(axis_labels_micro), 
             ") does not match number of variables (", num_vars_micro, ")"))
}

d1_radar_micro <- rbind(
  rep(1, ncol(d1_radar_micro)),
  rep(0, ncol(d1_radar_micro)),
  d1_radar_micro
)

d1_radar_micro <- d1_radar_micro %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_micro)) %>%
  relocate(group)

p2 <- ggradar(d1_radar_micro,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none", 
              axis.labels = axis_labels_micro) +
  ggtitle("Soil microbial diversity") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "Arial"),
        axis.text = element_text(family = "Arial", color = "black"),
        axis.title = element_text(family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.title = element_text(family = "Arial"),
        grid.text = element_text(family = "Arial"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(p2)

############## Soil N transformation rate ##########################
d1_wide_n_transform <- d1_wide %>%
  select(1, 22:26)

d1_wide_n_transform <- d1_wide_n_transform %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_n_transform <- d1_wide_n_transform %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_n_transform <- d1_radar_n_transform$Treatment

rownames(d1_radar_n_transform) <- d1_radar_n_transform$Treatment
d1_radar_n_transform <- d1_radar_n_transform[, -1]

num_vars_n_transform <- ncol(d1_radar_n_transform)

axis_labels_n_transform <- c("M", "I_NH₄", "I_NO₃", "O_Norg", "O_NH₄")

if(length(axis_labels_n_transform) != num_vars_n_transform) {
  stop(paste("Number of axis labels (", length(axis_labels_n_transform), 
             ") does not match number of variables (", num_vars_n_transform, ")"))
}

d1_radar_n_transform <- rbind(
  rep(1, ncol(d1_radar_n_transform)),
  rep(0, ncol(d1_radar_n_transform)),
  d1_radar_n_transform
)

d1_radar_n_transform <- d1_radar_n_transform %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_n_transform)) %>%
  relocate(group)

p3 <- ggradar(d1_radar_n_transform,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none", 
              axis.labels = axis_labels_n_transform) +
  ggtitle("Soil N transformation rate") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "Arial"),
        axis.text = element_text(family = "Arial", color = "black"),
        axis.title = element_text(family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.title = element_text(family = "Arial"),
        grid.text = element_text(family = "Arial"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(p3)

##############  Soil N transformation functional gene abundance ##########################
d1_wide_gene_abundance <- d1_wide %>%
  select(1, 27:32)

d1_wide_gene_abundance <- d1_wide_gene_abundance %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_gene_abundance <- d1_wide_gene_abundance %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_gene_abundance <- d1_radar_gene_abundance$Treatment

rownames(d1_radar_gene_abundance) <- d1_radar_gene_abundance$Treatment
d1_radar_gene_abundance <- d1_radar_gene_abundance[, -1]

num_vars_gene_abundance <- ncol(d1_radar_gene_abundance)

axis_labels_gene_abundance <- c(
  "AOA_A", "AOB_A", "Com_A",
  "nirS_A", "nirK_A", "nosZ_A"
)

if(length(axis_labels_gene_abundance) != num_vars_gene_abundance) {
  stop(paste("Number of axis labels (", length(axis_labels_gene_abundance), 
             ") does not match number of variables (", num_vars_gene_abundance, ")"))
}

d1_radar_gene_abundance <- rbind(
  rep(1, ncol(d1_radar_gene_abundance)),
  rep(0, ncol(d1_radar_gene_abundance)),
  d1_radar_gene_abundance
)

d1_radar_gene_abundance <- d1_radar_gene_abundance %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_gene_abundance)) %>%
  relocate(group)

p4 <- ggradar(d1_radar_gene_abundance,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none", 
              axis.labels = axis_labels_gene_abundance) +
  ggtitle("Soil N transformation\nfunctional gene abundance") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "Arial"),
        axis.text = element_text(family = "Arial", color = "black"),
        axis.title = element_text(family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.title = element_text(family = "Arial"),
        grid.text = element_text(family = "Arial"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(p4)

############## Soil N transformation functional gene diversity ##########################
d1_wide_gene_diversity <- d1_wide %>%
  select(1, 33:38)

d1_wide_gene_diversity <- d1_wide_gene_diversity %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_gene_diversity <- d1_wide_gene_diversity %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_gene_diversity <- d1_radar_gene_diversity$Treatment

rownames(d1_radar_gene_diversity) <- d1_radar_gene_diversity$Treatment
d1_radar_gene_diversity <- d1_radar_gene_diversity[, -1]

num_vars_gene_diversity <- ncol(d1_radar_gene_diversity)

axis_labels_gene_diversity <- c(
  "AOA_R", "AOA_S", "AOB_R",
  "AOB_S", "Com_R", "Com_S"
)

if(length(axis_labels_gene_diversity) != num_vars_gene_diversity) {
  stop(paste("Number of axis labels (", length(axis_labels_gene_diversity), 
             ") does not match number of variables (", num_vars_gene_diversity, ")"))
}

d1_radar_gene_diversity <- rbind(
  rep(1, ncol(d1_radar_gene_diversity)),
  rep(0, ncol(d1_radar_gene_diversity)),
  d1_radar_gene_diversity
)

d1_radar_gene_diversity <- d1_radar_gene_diversity %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_gene_diversity)) %>%
  relocate(group)

p5 <- ggradar(d1_radar_gene_diversity,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none",  # ← 不保留图例
              axis.labels = axis_labels_gene_diversity) +
  ggtitle("Soil N transformation\nfunctional gene diversity") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "Arial"),
        axis.text = element_text(family = "Arial", color = "black"),
        axis.title = element_text(family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.title = element_text(family = "Arial"),
        grid.text = element_text(family = "Arial"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(p5)

############## Soil enzyme activity ##########################
d1_wide_enzyme <- d1_wide %>%
  select(1, 39:41)

d1_wide_enzyme <- d1_wide_enzyme %>%
  mutate(Treatment = str_extract(Treatment, "^[A-Za-z]+\\d*")) %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "^O") ~ "O",
    str_detect(Treatment, "^F") ~ "F",
    TRUE ~ Treatment
  ))

d1_radar_enzyme <- d1_wide_enzyme %>%
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  ungroup()

treatment_names_enzyme <- d1_radar_enzyme$Treatment

rownames(d1_radar_enzyme) <- d1_radar_enzyme$Treatment
d1_radar_enzyme <- d1_radar_enzyme[, -1]

num_vars_enzyme <- ncol(d1_radar_enzyme)

axis_labels_enzyme <- c("AKP", "βG", "NAG")

if(length(axis_labels_enzyme) != num_vars_enzyme) {
  stop(paste("Number of axis labels (", length(axis_labels_enzyme), 
             ") does not match number of variables (", num_vars_enzyme, ")"))
}

d1_radar_enzyme <- rbind(
  rep(1, ncol(d1_radar_enzyme)),
  rep(0, ncol(d1_radar_enzyme)),
  d1_radar_enzyme
)

d1_radar_enzyme <- d1_radar_enzyme %>%
  as_tibble() %>%
  mutate(group = c(NA, NA, treatment_names_enzyme)) %>%
  relocate(group)

p6 <- ggradar(d1_radar_enzyme,
              values.radar = c("0", "0.5", "1.0"),
              grid.min = 0,
              grid.mid = 0.5,
              grid.max = 1,
              group.line.width = 0.5,
              group.point.size = 1.5,
              font.radar = "sans",
              legend.text.size = 10,
              grid.label.size = 3,
              axis.label.size = 2.5,
              group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
              background.circle.colour = "transparent",
              gridline.mid.colour = "grey85",
              legend.position = "none",  # ← 不保留图例
              axis.labels = axis_labels_enzyme) +
  ggtitle("Soil enzyme activity") +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  color = "black",
                                  family = "Arial"),
        axis.text = element_text(family = "Arial", color = "black"),
        axis.title = element_text(family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.title = element_text(family = "Arial"),
        grid.text = element_text(family = "Arial"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(p6)

############################### # Soil Health Index ###########################
library(ggplot2)
library(data.table)

data <- readxl::read_xlsx('...Data/Fig2.xlsx',sheet = x)
d1 <- as.data.table(data)

d1$Treatment <- factor(d1$Treatment, levels = c("CK", "O1", "O2", "O3", "O4", "F1","F2", "F3", "F4"))

p7 <- ggplot(d1, aes(x=Treatment, y=SHI_mean, size=SHI_mean, color=Treatment)) +
  geom_point(alpha=0.7) +
  
  geom_text(aes(label=round(SHI_mean, 2)), vjust=-1.5, size=3.5) +
  
  geom_segment(aes(xend = Treatment, yend = 0, color = Treatment), size = 1) +
  
  geom_point(aes(color = Treatment), size = 1, alpha = 1) +
  
  labs(y="ISHI", x="Treatment")+
  
  theme_bw()+
  
  theme(axis.text = element_text(colour = 'black', size = 8),  
        axis.title = element_text(colour = 'black', size = 12),  
        legend.position = "none",  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+  
  geom_hline(yintercept = 0, linetype = "dashed", color = "brown")+
  
  scale_y_continuous(
    limits = c(0, 1.2),
    breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
  )

print(p7)

library(ggpubr)

row1 <- ggarrange(p1, p2, p6, 
                  ncol = 3, labels = c("a", "b", "c"),
                  font.label = list(family = "Arial", size = 12, color = "black", face = "bold"),
                  widths = c(4, 4, 4),
                  label.y = 0.95,   
                  label.x = -0.02)  

row2 <- ggarrange(p3, p4, p5,  
                  ncol = 3, labels = c("d", "e", "f"),
                  font.label = list(family = "Arial", size = 12, color = "black", face = "bold"),
                  widths = c(4, 4, 4),
                  label.y = 1,    
                  label.x = -0.02)   

row3 <- ggarrange(p7, 
                  ncol = 1, labels = "g",
                  font.label = list(family = "Arial", size = 12, color = "black", face = "bold"),
                  label.y = 1.15,  
                  label.x = -0.007) 

library(cowplot)

d1_radar_soil$group <- factor(d1_radar_soil$group, levels = c("CK", "O", "F"))

p1_with_legend <- ggradar(d1_radar_soil,
                          values.radar = c("0", "0.5", "1.0"),
                          grid.min = 0,
                          grid.mid = 0.5,
                          grid.max = 1,
                          group.line.width = 0.5,
                          group.point.size = 1.5,
                          font.radar = "sans",
                          legend.text.size = 10,
                          grid.label.size = 3,
                          axis.label.size = 3,
                          group.colours = c("CK" = "#666666", "O" = "#4d9221", "F" = "#c51b7d"),
                          background.circle.colour = "transparent",
                          gridline.mid.colour = "grey85",
                          legend.position = "top",
                          axis.labels = axis_labels) +
  theme(legend.text = element_text(family = "Arial"))

legend_plot <- get_legend(p1_with_legend)

main_plot <- ggarrange(row1, row2, row3, 
                       ncol = 1, heights = c(3, 3, 2))

final_plot <- plot_grid(legend_plot, main_plot,
                        ncol = 1,
                        rel_heights = c(0.03, 1))  # 图例占3%，主图占剩余空间

print(final_plot)

