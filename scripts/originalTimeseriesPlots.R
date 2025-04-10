library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(ggtext)

exp1 <- read.csv("C:\\Users\\timet\\OneDrive\\Desktop\\exp6.csv", fileEncoding = 'UTF-8-BOM')

exp1$treatment <- factor(exp1$treatment,
                         levels = c("adultTube", "naturalBiof", "labBiof", "control"))

myColors = c("#E07355", "#9AD4D9", "#497371", "#826b51")
#adult, control, lab, field

exp1 %>%
ggplot(aes(x = day, y = settlement, color = treatment, group = treatment)) +
  geom_point(size = 7) +
  geom_line(size=2,aes(linetype = treatment)) +
  scale_linetype_manual(values=c("solid","twodash", "dashed", "dotted"))+
  
  ylim(0,100)+
  labs(title = "Family 6",
       x = "",
       y = "",
       color = "") +
  
  
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face= "bold")) +
  theme(legend.position="none") +
  
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white")) +
  
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  
  theme(panel.grid.major.x = element_blank()) +
  
  theme(legend.text = element_text(face= "bold")) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values = myColors,
                     labels = c("Adult Tube Chip", "Field-biofilmed Chip", "Lab-biofilmed Chip", "Control")) + 
  theme(axis.title.x = element_text(vjust=-1)) +
  theme(axis.title.y = element_text(vjust=4))
  



