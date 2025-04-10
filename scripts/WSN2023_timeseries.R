rm(list=ls())
library(car)
library(tidyverse)

ficopomatus_e <- read.csv("data/ficopomatus_e.csv")

ggplot(ficopomatus_e, aes(x=dpf, y=settlement, group=treatment, color=treatment)) +
  geom_line(aes(linetype=treatment)) +
  geom_point()+
  #geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", width=0.1) +
  labs(x="DPF", y="Settlement (%)") + 
  theme_bw()



ficopomatus_e$treatment <- as.factor(ficopomatus_e$treatment)
ficopomatus_e$family <- as.factor(ficopomatus_e$family)
ficopomatus_e$dpf <- as.factor(ficopomatus_e$dpf)

SummaryByTreatment <- ficopomatus_e %>%
  group_by(dpf, treatment) %>%
  summarize(mean=mean(ficopomatus_e$settlement, na.rm=TRUE), std_err=sd(ficopomatus_e$settlement, na.rm=TRUE)/sqrt(length((na.omit(ficopomatus_e$settlement)))))



FicoGraph <- ficopomatus_e %>%
  group_by(dpf, treatment) %>%
  summarize(mean=mean(settlement), std_err=sd(settlement, na.rm=TRUE)/sqrt(length((na.omit(settlement)))))
FicoGraph


FicoGraph$treatment <- factor(SummaryByTreatment$treatment, levels = c("adult", "mussel", "biofilm", "control"))

custom_legend_order <- c("Conspecific Cue", "Field Biofilm", "Lab Biofilm", "Control (no biofilm)")


ggplot(FicoGraph, aes(x=dpf, y=mean, color = treatment)) +
  geom_line(aes(linetype=treatment)) +
  geom_point() +
  geom_errorbar(aes(ymax=mean+std_err, ymin=mean-std_err), stat="identity", width=0.1) +
  labs(x="Days Post Fertilization (DPF)", y="Cumulative Settlement (%)") +
  scale_color_discrete(name = "Treatment", labels = custom_legend_order) +
  theme_bw() +
  guides(linetype=FALSE)  


