#Nrlosses

library(dplyr)
library(car)
library(agricolae)
library(ggplot2)
library(data.table)

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$Nr_losses ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$Nr_losse ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$Nr_losse ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$Nr_losse`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),  
  panel.background = element_blank(),

  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),

  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p1 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("Nr losses"~(kg~N~ha^-1))) +
  #labs(title = "", x = "Treatment", y = expression("Reactive nitrogen losses"~(kg~N~ha^-1))) +
  ylim(0, 100) + 
  my_theme+
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


#######################GHG#########################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$GHG ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$GHG ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$GHG ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$GHG`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),
  axis.text.y = element_text(family = "Arial", color = "black"),
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p2 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("GHG"~(t~CO[2]-eq~ha^-1))) +
  #labs(title = "", x = "Treatment", y = expression("Global warming potential"~(kg~CO[2]-eq~ha^-1))) +
  ylim(0, 3) +  
  my_theme+
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



#################### n2o_emission ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$n2o_emission ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$n2o_emission ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$n2o_emission ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$n2o_emission`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),   
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p3 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("N"[2]*"O emission"~(kg~N~ha^-1))) +
  ylim(0, 8) +  
  my_theme+
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


#################### nh3_volatilization ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$nh3_volatilization ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$nh3_volatilization ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$nh3_volatilization ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$nh3_volatilization`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),   
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p4 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("NH"[3]*" emission"~(kg~N~ha^-1))) +
  ylim(0, 30) +  
  my_theme+
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

print(p4)


#################### N_leaching ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$N_leaching ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$N_leaching ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$N_leaching ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$N_leaching`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("CK", "O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),  
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p5 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("N leaching"~(kg~N~ha^-1))) +
  ylim(0, 60) +  
  my_theme+
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

print(p5)

#################### n2o_emission factor ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$EF_N2O ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$EF_N2O ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$EF_N2O ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$EF_N2O`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"),  
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5), 
  axis.text.y = element_text(family = "Arial", color = "black"),
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p8 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("N"[2]*"O emission fraction (%)")) +
  ylim(0, 4) + 
  my_theme+
  scale_fill_manual(values = c(
    "O1" = "#FDBE85",       
    "O2" = "#FD8D3C",       
    "O3" = "#E6550D",       
    "O4" = "#A63603",       
    "F1" = "#9ECAE1",      
    "F2" = "#6BAED6",      
    "F3" = "#3182BD",      
    "F4" = "#08519C"        
  ))

print(p8)


#################### nh3_volatilization emission factor ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$EF_NH3 ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$EF_NH3 ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$EF_NH3 ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$EF_NH3`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5), 
  axis.text.y = element_text(family = "Arial", color = "black"),   
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p9 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("NH"[3]*" emission fraction (%)")) +
  ylim(0, 30) +  
  my_theme+
  scale_fill_manual(values = c(
    "O1" = "#FDBE85",      
    "O2" = "#FD8D3C",       
    "O3" = "#E6550D",       
    "O4" = "#A63603",       
    "F1" = "#9ECAE1",       
    "F2" = "#6BAED6",       
    "F3" = "#3182BD",      
    "F4" = "#08519C"        
  ))

print(p9)


#################### N_leaching loss rate ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$EF_Nleaching ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$EF_Nleaching ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$EF_Nleaching ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$EF_Nleaching`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"), 
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),  

  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p10 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("N leaching loss fraction (%)")) +
  ylim(0, 15) +  
  my_theme+
  scale_fill_manual(values = c(
    "O1" = "#FDBE85",       
    "O2" = "#FD8D3C",      
    "O3" = "#E6550D",      
    "O4" = "#A63603",       
    "F1" = "#9ECAE1",       
    "F2" = "#6BAED6",       
    "F3" = "#3182BD",       
    "F4" = "#08519C"        
  ))

print(p10)

#################### Nr losses rate ############################################

d1 <- readxl::read_xlsx('...Data/Fig3.xlsx',sheet = x)
d1 <- as.data.table(d1)

d1$Treatment <- as.factor(d1$Treatment)
class(d1$Treatment)

nom <- bartlett.test(d1$EF_Nr ~ d1$Treatment,data = d1)
nom

nom1<-leveneTest(d1$EF_Nr ~ d1$Treatment,data = d1)
nom$p.value

oneway<-aov(d1$EF_Nr ~ d1$Treatment,data = d1)
anova(oneway)

out <- LSD.test(oneway,"d1$Treatment",p.adj="none")
out

mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`d1$EF_Nr`,mar$groups)
sort<-newmar[order(newmar$rownamemar),]

Treatment<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
se<-sd / sqrt(3)
marker<-sort$mar.groups
plotdata<-data.frame(Treatment,mean,sd,se,marker)
plotdata

new_order <- c("O1", "O2", "O3", "O4", "F1", "F2", "F3", "F4")

plotdata <- plotdata %>%
  mutate(Treatment = factor(Treatment, levels = new_order)) %>%
  arrange(Treatment)

print(plotdata)

my_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"),  
  axis.text.x = element_text(family = "Arial", angle = 60, color = "black" , vjust = 0.5),  
  axis.text.y = element_text(family = "Arial", color = "black"),   
  
  panel.background = element_blank(),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)
)

p11 <- ggplot(plotdata, aes(x=factor(Treatment),y=mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width = 0.2, position = position_dodge(0.9), colour = "darkgreen") +
  geom_text(aes(x=factor(Treatment),y=mean+se,label=marker), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "", x = "", y = expression("Nr losses fraction (%)")) +
  ylim(0, 35) + 
  my_theme+
  scale_fill_manual(values = c(
    "O1" = "#FDBE85",    
    "O2" = "#FD8D3C",       
    "O3" = "#E6550D",       
    "O4" = "#A63603",     
    "F1" = "#9ECAE1",      
    "F2" = "#6BAED6",      
    "F3" = "#3182BD",     
    "F4" = "#08519C"       
  ))

print(p11)

library(ggpubr)
library(cowplot)

p_arranged <- ggarrange(
  p3, p4, p5, p8, p9, p10, p1, p11, p2,
  ncol = 3, nrow = 3,
  labels = c("a", "b", "c", "", "", "", "g", "h", "i"),
  font.label = list(family = "Arial", size = 12, color = "black", face = "bold"),
  common.legend = TRUE, legend = "none",
  hjust = 0, vjust = 1
)

p_Environmental_factor <- ggdraw(p_arranged) +
  draw_plot_label(c("d", "e", "f"),
                  x = c(-0.003, 0.33, 0.66), 
                  y = c(0.69, 0.69, 0.69),  
                  fontface = "bold",
                  size = 12,
                  family = "Arial")

