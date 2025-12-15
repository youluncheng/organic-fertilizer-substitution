#########################################fig S5a#####################################################

library(ggplot2)
library(dplyr)
library(data.table)

new_data <- readxl::read_xlsx('...Data/FigS5.xlsx',sheet = X)  
new_data <- as.data.table(new_data)

new_data$Treatment <- factor(new_data$Treatment, levels = c("O", "F"),
                             labels = c("Organic fertilizer", "Chemical fertilizer"))

linear_models <- new_data %>%
  group_by(Treatment) %>%
  do(model = lm(Value ~ Year, data = .))

slope_p_values <- summarise(linear_models, 
                            slope = coef(model)[2], 
                            p_value = summary(model)$coefficients[2, 4],
                            Treatment = unique(Treatment)) 

organic_slope <- slope_p_values$slope[slope_p_values$Treatment == "Organic fertilizer"]
organic_p <- slope_p_values$p_value[slope_p_values$Treatment == "Organic fertilizer"]
chemical_slope <- slope_p_values$slope[slope_p_values$Treatment == "Chemical fertilizer"]
chemical_p <- slope_p_values$p_value[slope_p_values$Treatment == "Chemical fertilizer"]

create_label <- function(treatment, slope, p) {
  if(p < 0.05) {
    bquote(paste(.(treatment), ": slope = ", .(round(slope, 2)), ", ", italic("P"), " < 0.05"))
  } else {
    bquote(paste(.(treatment), ": slope = ", .(round(slope, 2)), ", ", italic("P"), " = ", .(round(p, 2))))
  }
}

custom_labels <- list(
  "Organic fertilizer" = create_label("Organic fertilizer", organic_slope, organic_p),
  "Chemical fertilizer" = create_label("Chemical fertilizer", chemical_slope, chemical_p)
)

p1 <- ggplot(new_data, aes(x = Year, y = Value, color = Treatment)) +
  geom_point(shape = 1, size = 2, stroke = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, fill = "#996633") +
  labs(x = "Year", y = expression("AEN"~(kg~kg^-1)), color = "Treatment") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(breaks = unique(new_data$Year), labels = unique(new_data$Year)) + 
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("Organic fertilizer" = "#1B9E77", "Chemical fertilizer" = "#D95F02"),
                     labels = custom_labels) + 
  guides(color = guide_legend(title = NULL),  
         shape = guide_legend(title = NULL)) +  
  my_theme+ 
  annotate("text", x = Inf, y = Inf, label = "Winter wheat", 
           hjust = 1.1, vjust = 1.1, color = "black", family = "Arial", size = 4, fontface = "bold")+
  theme(legend.position = c(0.02, 1), legend.justification = c(0, 1),
        legend.text = element_text(size = 12, hjust = 0),  
        axis.title.x = element_text(family = "Arial", size = 12, color = "black"),
        axis.title.y = element_text(family = "Arial", size = 12, color = "black"),
        axis.text.x = element_text(family = "Arial", size = 10, color = "black"), 
        axis.text.y = element_text(family = "Arial", size = 10, color = "black"), 
        
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "#E0EEE0", color = "black", size = 1))  

print(p1)


#########################################fig S5b）#####################################################

library(ggplot2)
library(dplyr)
library(data.table)


new_data <- readxl::read_xlsx('...Data/FigS5.xlsx',sheet = X)  
new_data <- as.data.table(new_data)

new_data$Treatment <- factor(new_data$Treatment, levels = c("O", "F"),
                             labels = c("Organic fertilizer", "Chemical fertilizer"))

linear_models <- new_data %>%
  group_by(Treatment) %>%
  do(model = lm(Value ~ Year, data = .))

slope_p_values <- summarise(linear_models, 
                            slope = coef(model)[2], 
                            p_value = summary(model)$coefficients[2, 4],
                            Treatment = unique(Treatment))  
organic_slope <- slope_p_values$slope[slope_p_values$Treatment == "Organic fertilizer"]
organic_p <- slope_p_values$p_value[slope_p_values$Treatment == "Organic fertilizer"]
chemical_slope <- slope_p_values$slope[slope_p_values$Treatment == "Chemical fertilizer"]
chemical_p <- slope_p_values$p_value[slope_p_values$Treatment == "Chemical fertilizer"]

create_label <- function(treatment, slope, p) {
  if(p < 0.05) {
    bquote(paste(.(treatment), ": slope = ", .(round(slope, 2)), ", ", italic("P"), " < 0.05"))
  } else {
    bquote(paste(.(treatment), ": slope = ", .(round(slope, 2)), ", ", italic("P"), " = ", .(round(p, 2))))
  }
}

custom_labels <- list(
  "Organic fertilizer" = create_label("Organic fertilizer", organic_slope, organic_p),
  "Chemical fertilizer" = create_label("Chemical fertilizer", chemical_slope, chemical_p)
)

p2 <- ggplot(new_data, aes(x = Year, y = Value, color = Treatment)) +
  geom_point(shape = 1, size = 2, stroke = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, fill = "#996633") +
  labs(x = "Year", y = expression("AEN"~(kg~kg^-1)), color = "Treatment") +
  scale_y_continuous(limits = c(0, 60)) +  
  scale_x_continuous(breaks = unique(new_data$Year), labels = unique(new_data$Year)) +  
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("Organic fertilizer" = "#1B9E77", "Chemical fertilizer" = "#D95F02"),
                     labels = custom_labels) +  
  guides(color = guide_legend(title = NULL), 
         shape = guide_legend(title = NULL)) + 
  my_theme+ 
  annotate("text", x = Inf, y = Inf, label = "Summer maize", 
           hjust = 1.1, vjust = 1.1, color = "black", family = "Arial", size = 4, fontface = "bold")+
  theme(legend.position = c(0.02, 1), legend.justification = c(0, 1),
        legend.text = element_text(size = 12, hjust = 0), 
        axis.title.x = element_text(family = "Arial", size = 12, color = "black"),
        axis.title.y = element_text(family = "Arial", size = 12, color = "black"),
        axis.text.x = element_text(family = "Arial", size = 10, color = "black"), 
        axis.text.y = element_text(family = "Arial", size = 10, color = "black"), 
        
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "#E0EEE0", color = "black", size = 1))  

print(p2)


library(ggpubr)

p_yield_final <- ggarrange(p1, p2,
                           ncol = 1,
                           heights = c(2.2, 2.2))  

print(p_yield_final)
