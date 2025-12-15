# Yield data
# Wheat

library(dplyr)
library(car)
library(ggplot2)
library(data.table)
library(agricolae)


d1 <- readxl::read_xlsx('...Data/Fig1.xlsx',sheet = x)
d1 <- as.data.table(d1)

## Convert grouping information to factor
d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

# Homogeneity of variance tests
nom <- bartlett.test(d1$mean_w ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$mean_w ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$mean_w ~ d1$Treatment,data = d1)
anova(oneway)

# Multiple comparisons
out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

# Prepare a table for plotting
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$mean_w`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

# Sort the groups dataframe by column name to align with means and SDs one-to-one
Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,marker)
plotdata

# Create a vector to specify the new Treatment order
new_order <- c("CK", "O1", "F1", "O2", "F2", "O3", "F3", "O4", "F4")

# Convert Treatment to factor and reorder by the new order
plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

# plot
my_theme <- theme(

  text = element_text(family = "Arial", size = 12, color = "black"), 

  axis.text.x = element_text(family = "Arial", size = 8, angle = 60, color = "black", vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", size = 8, color = "black"), 

  panel.background = element_blank(),

  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)


p1 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+sd,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = expression("Grain yiled"~(t~ha^-1))) +
  ylim(0, 8) +  
  guides(fill = FALSE) + 
  my_theme+ 
  annotate("text", x = 0.5, y = 8, label = "Winter wheat", 
           hjust = 0, vjust = 1, color = "black", family = "Arial", size = 3, fontface = "bold") +
  scale_fill_manual(values = c(
    "CK" = "#7F7F7F",       
    "O1" = "#FDBE85",       
    "O2" = "#FD8D3C",       
    "O3" = "#E6550D",       
    "O4" = "#A63603",       
    "F1" = "#9ECAE1",       
    "F2" = "#6BAED6",       
    "F3" = "#3182BD",       
    "F4" = "#08519C"        
  ))

print(p1)


#############################################
# Maize
# Yield data


d1 <- readxl::read_xlsx('...Data/Fig1.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$mean_m ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$mean_m ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$mean_m ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$mean_m`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,marker)
plotdata

new_order <- c("CK", "O1", "F1", "O2", "F2", "O3", "F3", "O4", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  
  axis.text.x = element_text(family = "Arial", size = 8, angle = 60,color = "black", vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", size = 8, color = "black"),   

  panel.background = element_blank(),

  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),

  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p2 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+sd,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = expression("Grain yiled"~(t~ha^-1))) +
  ylim(0, 10) +  
  guides(fill = FALSE) +  
  my_theme+ 
  annotate("text", x = 0.5, y = 10, label = "Summer maize", 
           hjust = 0, vjust = 1, color = "black", family = "Arial", size = 3, fontface = "bold")+
  scale_fill_manual(values = c(
    "CK" = "#7F7F7F",       
    "O1" = "#FDBE85",       
    "O2" = "#FD8D3C",      
    "O3" = "#E6550D",       
    "O4" = "#A63603",      
    "F1" = "#9ECAE1",      
    "F2" = "#6BAED6",      
    "F3" = "#3182BD",       
    "F4" = "#08519C"       
  ))

print(p2)

#############################################
# Wheat & Maize Equivalent Yield
# Yield data

d1 <- readxl::read_xlsx('...Data/Fig1.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$mean_wm ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$mean_wm ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$mean_wm ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$mean_wm`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,marker)
plotdata

new_order <- c("CK", "O1", "F1", "O2", "F2", "O3", "F3", "O4", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(

  text = element_text(family = "Arial", size = 12, color = "black"),  

  axis.text.x = element_text(family = "Arial", size = 8, angle = 60, color = "black", vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", size = 8, color = "black"), 

  panel.background = element_blank(),

  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),

  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p3 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+sd,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "", y = expression("Equivalent yield"~(t~ha^-1))) +
  ylim(0, 16) +  
  guides(fill = FALSE) + 
  my_theme+ 
  annotate("text", x = 0.5, y = 16, label = "Winter wheat-\nsummer maize", 
           hjust = 0, vjust = 1, color = "black", family = "Arial", size = 3, fontface = "bold")+
  scale_fill_manual(values = c(
    "CK" = "#7F7F7F",      
    "O1" = "#FDBE85",       
    "O2" = "#FD8D3C",      
    "O3" = "#E6550D",       
    "O4" = "#A63603",       
    "F1" = "#9ECAE1",       
    "F2" = "#6BAED6",       
    "F3" = "#3182BD",      
    "F4" = "#08519C"       
  ))

print(p3)


######################################## Yield stability ##########################################

library(ggplot2)
library(data.table)

data <- readxl::read_xlsx('...Data/Fig1.xlsx',sheet = x)
data <- as.data.table(data)
data <- data[, 1:17, with = FALSE]

mean_e <- apply(data[data$Experiment %in% c("O1", "O2", "O3", "O4"), -1], 1, mean)
sd_e <- apply(data[data$Experiment %in% c("O1", "O2", "O3", "O4"), -1], 1, sd)

mean_c <- apply(data[data$Experiment %in% c("F1", "F2", "F3", "F4"), -1], 1, mean)
sd_c <- apply(data[data$Experiment %in% c("F1", "F2", "F3", "F4"), -1], 1, sd)


CVe <- sd_e / mean_e
CVc <- sd_c / mean_c

Relative_Stability <- CVe / CVc

forest_data <- data.frame(
  Experiment = c("O1", "O2", "O3", "O4"),
  Relative_Stability = Relative_Stability
)

p4 <- ggplot(forest_data, aes(y = Experiment, x = Relative_Stability)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = Relative_Stability - 1.96 * sqrt(1/mean_e + 1/mean_c), 
                     xmax = Relative_Stability + 1.96 * sqrt(1/mean_e + 1/mean_c)), 
                 height = 0.2, color = "steelblue", size = 0.9) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Relative stability ratio", y = "") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12, color = "black"),  
    axis.text.x = element_text(family = "Arial", size = 8, angle = 270, color = "black"),
    axis.text.y = element_text(family = "Arial", size = 8, color = "black"),
        axis.line = element_line(color = "black", size = 0.5),  
        panel.border = element_rect(color = "black", size = 0.5, fill = NA),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),  
        plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(x = Relative_Stability, color = Experiment), size = 3) +
  scale_color_manual(values = c("O1" = "red", "O2" = "green", "O3" = "blue", "O4" = "purple")) +
  guides(color = FALSE) +  
  scale_x_continuous(breaks = seq(0.8, 1.6, by = 0.2), limits = c(0.8, 1.6)) +    
  scale_y_discrete(labels = c("O1" = "O1/F1", "O2" = "O2/F2", "O3" = "O3/F3", "O4" = "O4/F4")) +
  labs(color = "Experiment")+
  coord_flip()

print(p4)

#########################################Yearly yield change trends#####################################################

library(ggplot2)
library(dplyr)
library(data.table)

new_data <- readxl::read_xlsx('...Data/Fig1.xlsx',sheet = x)
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

p5 <- ggplot(new_data, aes(x = Year, y = Value, color = Treatment)) +
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
  theme(legend.position = c(0.02, 1), legend.justification = c(0, 1),
        legend.text = element_text(size = 12, hjust = 0),  
        axis.title.x = element_text(family = "Arial", size = 12, color = "black"),
        axis.title.y = element_text(family = "Arial", size = 12, color = "black"),
        axis.text.x = element_text(family = "Arial", size = 10, color = "black"), 
        axis.text.y = element_text(family = "Arial", size = 10, color = "black"),  

        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "#E0EEE0", color = "black", size = 1))  

print(p5)

#########################################

library(ggpubr)

p12_combined <- ggarrange(p1, p2,
                          ncol = 1, nrow = 2,
                          align = "v",
                          heights = c(1, 1),
                          labels = c("a", "b"),
                          font.label = list(family = "Arial", size = 12, color = "black", face = "bold"))

top_row <- ggarrange(p12_combined, p3, p4,
                     ncol = 3,
                     widths = c(3, 2, 2),
                     labels = c("", "c", "d"), 
                     font.label = list(family = "Arial", size = 12, color = "black", face = "bold"))

bottom_row <- ggarrange(p5,
                        ncol = 1, nrow = 1,
                        labels = "e",
                        label.x = 0,
                        label.y = 1.1,
                        font.label = list(family = "Arial", size = 12, color = "black", face = "bold"))

p_yield_final <- ggarrange(top_row, bottom_row,
                           ncol = 1,
                           heights = c(2.2, 1.3)) 

print(p_yield_final)


