library(car)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(moments)
library(multcomp)

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

# friedman.test(Settlement ~ Treatment | Family, data = data_percent)
# 
# pairwise.wilcox.test(
#   x = data_percent$Settlement,
#   g = data_percent$Treatment,
#   paired = TRUE,
#   p.adjusted.method = "bonferroni"
# )

model <- lmer(Settlement ~ Treatment + (1|Family), data = data_percent)
summary(model)

emmeans(model, pairwise ~ Treatment, adjust = "tukey")

tukey_glht <- glht(model, linfct = mcp(Treatment = "Tukey"))

letters <- cld(tukey_glht)

letters_df <- data.frame(
  Treatment = names(letters$mcletters$Letters),
  Letter = letters$mcletters$Letters
)

y_pos <- aggregate(Settlement ~ Treatment, data = data_percent, max)

plot_letters <- merge(letters_df, y_pos, by = "Treatment")
plot_letters$Settlement <- plot_letters$Settlement + 5


ggplot(data_percent, aes(x = Treatment, y = Settlement)) +
  geom_boxplot(stat = "boxplot", width = 0.5) +
  geom_point(aes(shape = Family), size = 2) +
  geom_text(data = plot_letters, aes(label = Letter, y = Settlement), vjust = 0) +
  labs(x = "", y = "Settlement (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = .5),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )
