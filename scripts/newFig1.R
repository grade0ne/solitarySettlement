library(tidyverse)
library(multcompView)
library(FSA)

#################################################
# Data import

data <- read.csv("data/gregariousTestExp.csv")

data_percent <- data %>%
  mutate(
    Family = as.factor(Family),
    Treatment = as.factor(Treatment),
    Treatment = fct_recode(Treatment,
        "Adult Tube" = "Adult"),
    Treatment = fct_relevel(Treatment, "Adult Tube", "Mussel", "Control"),
    Settlement = (Settled / Total) * 100
  )

#################################################
# Kruskal-Wallis & Holm-corrected Dunn

kruskal.test(Settlement ~ Treatment, data = data_percent)

dunn_result <- dunnTest(Settlement ~ Treatment, data = data_percent, method = "holm")

#################################################
# Box plot

dunn_df <- dunn_result$res

pvals <- dunn_df$P.adj
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

group_letters <- multcompLetters(pvals)$Letters

letter_df <- data.frame(
  Treatment = names(group_letters),
  Letter = group_letters
)

y_pos <- data_percent %>%
  group_by(Treatment) %>%
  summarise(Settlement = max(Settlement)) %>%
  left_join(letter_df, by = "Treatment") %>%
  mutate(Settlement = Settlement + 5)

ggplot(data_percent, aes(x = Treatment, y = Settlement)) +
  geom_boxplot(stat = "boxplot", width = 0.5) +
  geom_point(aes(shape = Family), size = 2) +
  geom_text(data = y_pos, aes(label = Letter, y = Settlement), vjust = 0) +
  labs(x = "", y = "Settlement (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = .5),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )
