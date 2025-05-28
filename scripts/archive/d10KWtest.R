library(tidyverse)
library(multcompView)
library(FSA)

data <- read.csv("data/labeledFicoSettlement.csv")

data$Family <- as.factor(data$Family)
data$DPF <- as.factor(data$DPF)
data$Treatment <- as.factor(data$Treatment)
data$Score <- as.factor(data$Score)

d10settle <- data %>%
  filter(DPF == "10") %>%
  group_by(Family, Treatment) %>%
  summarise(Settled_perc = sum(Score == "Juvenile") / n() * 100)

kruskal.test(Settled_perc ~ Treatment, data = d10settle)

dunn_result <- dunnTest(Settled_perc ~ Treatment, data = d10settle, method = "holm")

dunn_df <- dunn_result$res

pvals <- dunn_df$P.adj
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

group_letters <- multcompLetters(pvals)$Letters

letter_df <- data.frame(
  Treatment = names(group_letters),
  Letter = group_letters
)

y_pos <- d10settle %>%
  group_by(Treatment) %>%
  summarise(Settled_perc = max(Settled_perc)) %>%
  left_join(letter_df, by = "Treatment") %>%
  mutate(Settled_perc = Settled_perc + 5)

ggplot(d10settle, aes(x = Treatment, y = Settled_perc)) +
  geom_boxplot(stat = "boxplot", width = 0.5) +
  geom_point(aes(shape = Family), size = 2) +
  geom_text(data = y_pos, aes(label = Letter, y = Settled_perc), vjust = 0) +
  labs(x = "", y = "Settlement (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = .5),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )
