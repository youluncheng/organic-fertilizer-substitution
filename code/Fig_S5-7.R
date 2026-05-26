############################### N2O emission #################################

library(ggsci)
library(ggplot2)
library(data.table)

# Read data
d1 <- readxl::read_xlsx('...Data/FigS7-9.xlsx', sheet = X)
d1 <- as.data.frame(d1)  

d1$group <- factor(d1$group, levels = c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4"))

theme <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 6, color = "black"), 
        axis.text.y = element_text(hjust = 0.5,size = 6, color = "black"),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"), 
        axis.title.x = element_text(size = 12, color = "black"), 
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())   

legend_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")


p1 <- ggplot(data = d1,aes(x = DATE,y = mean,group = group,colour = group,fill = group)) +
  geom_errorbar(aes(ymin = mean - se,ymax = mean + se),width = 0.2) +
  geom_point(size = 1) +
  geom_line(size = 0.2) +
  labs(x = "", y = expression("N"[2]*"O flux"~(ug~N~m^-2~h^-1))) +
 
  scale_color_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                     labels = legend_order) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                    labels = legend_order) +
  theme+ 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
  guides(colour = guide_legend(nrow = 1),  
    fill = guide_legend(nrow = 1))+    
  facet_wrap(~group,scales = "free_y", strip.position = "right", ncol = 1, nrow = 9)+
  geom_vline(xintercept = 50.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 70.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 95.5,linetype=2,cex=0.2,color = "pink4")

p1

############################### NH3 volatilization #################################

library(ggsci)
library(ggplot2)
library(data.table)

# Read data
d1 <- readxl::read_xlsx('...Data/FigS7-9.xlsx', sheet = X)
d1 <- as.data.frame(d1)  
d1$group <- factor(d1$group, levels = c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4"))

theme <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 6, color = "black"),  
        axis.text.y = element_text(hjust = 0.5,size = 6, color = "black"),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"), 
        axis.title.x = element_text(size = 12, color = "black"), 
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())   

legend_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")


p2 <- ggplot(data = d1,aes(x = DATE,y = mean,group = group,colour = group,fill = group)) +
  geom_errorbar(aes(ymin = mean - se,ymax = mean + se),width = 0.2) +
  geom_point(size = 1.5) +
  geom_line(size = 0.5) +
  labs(x = "",y = expression("NH"[3]*" flux"~(kg~N~ha^-1~h^-1))) +
  
  scale_color_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                     labels = legend_order) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                    labels = legend_order) +
  theme+ 
  
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+  
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1))+    
  facet_wrap(~group,scales = "free_y", strip.position = "right", ncol = 1, nrow = 9)+
  geom_vline(xintercept = 10.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 22.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 38.5,linetype=2,cex=0.2,color = "pink4")

p2

############################### N leaching #################################

library(ggsci)
library(ggplot2)
library(data.table)

# Read data
d1 <- readxl::read_xlsx('...Data/FigS7-9.xlsx', sheet = X)
d1 <- as.data.frame(d1)  
d1$group <- factor(d1$group, levels = c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4"))

theme <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size = 8),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 6, color = "black"),  
        axis.text.y = element_text(hjust = 0.5,size = 6, color = "black"),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"), 
        axis.title.x = element_text(size = 12, color = "black"), 
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())   

legend_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")


p3 <- ggplot(data = d1,aes(x = DATE,y = mean,group = group,colour = group,fill = group)) +
  geom_errorbar(aes(ymin = mean - se,ymax = mean + se),width = 0.2) +
  geom_point(size = 1.5) +
  geom_line(size = 0.5) +
  labs(x = "", y = expression("N leaching concentration"~(mg~N~L^-1)))+
 
  scale_color_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                     labels = legend_order) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(length(legend_order), "Set1"), 
                    labels = legend_order) +
  theme+ 

  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
  guides(colour = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1))+   
  facet_wrap(~group,scales = "free_y", strip.position = "right", ncol = 1, nrow = 9)+
  geom_vline(xintercept = 16.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 30.5,linetype=2,cex=0.2,color = "pink4")+
  geom_vline(xintercept = 51.5,linetype=2,cex=0.2,color = "pink4")
p3
