library(tidyverse)
library(emmeans)
library(multcompView)
library(multcomp)
library(FSA)
library(lme4)
library(lmerTest)
library(car)
library(moments)

#################################################
# Data import

data <- read.csv("data/ficoSettlementData.csv")

data$Treatment <- factor(data$Treatment, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("Conspecific", "Nat Biof", "Lab Biof", "Control"))
data$Score <- factor(data$Score,
                     levels = c(0, 1, 2, 3, 4, 5),
                     labels = c("Missing", "Juvenile", "Swimming", "Stuck", "Dead", "Part Meta"))

data$Score <- factor(data$Score, levels = c("Juvenile", "Part Meta", "Swimming", "Stuck", "Missing", "Dead"))

data10dpf <- data %>%
  filter(DPF == "10") %>%
  group_by(Treatment, Family) %>%
  summarise(Total_Larvae = length(Family),
            Juvenile = sum(Score == "Juvenile"),
            Settlement = (Juvenile / Total_Larvae) * 100)

data10dpf$Family <- as.factor(data10dpf$Family)

#################################################
# KW Test

kruskal.test(Settlement ~ Treatment, data = data10dpf)

dunn_result <- dunnTest(Settlement ~ Treatment, data = data10dpf, method = "holm")

dunn_df <- dunn_result$res

pvals <- dunn_df$P.adj
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

group_letters <- multcompLetters(pvals)$Letters

letter_df <- data.frame(
  Treatment = names(group_letters),
  Letter = group_letters
)

y_pos <- data10dpf %>%
  group_by(Treatment) %>%
  summarise(Settlement = max(Settlement)) %>%
  left_join(letter_df, by = "Treatment") %>%
  mutate(Settlement = Settlement + 5)

ggplot(data10dpf, aes(x = Treatment, y = Settlement)) +
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

#################################################
# LMM

model <- lmer(Settlement ~ Treatment + (1|Family), data = data10dpf)

qqnorm(resid(model))
qqline(resid(model))

qqp(resid(model), distribution = "norm")

hist(resid(model), breaks = 20)

skewness(resid(model))

fitted_val = fitted(model)

plot(fitted_val, resid(model),
     abline(h=0, lty = 2))

shapiro.test(resid(model))

summary(model)
anova(model)

emmeans(model, pairwise ~ Treatment, adjust = "tukey")

#################################################
# Box plot

emm <- emmeans(model, pairwise ~ Treatment, adjust = "tukey")

cld_results <- multcomp::cld(emm[[1]], Letters = letters)

unique_letters <- sort(unique(stringr::str_trim(cld_results$.group)), decreasing = TRUE)
reversed_letters <- setNames(letters[seq_along(unique_letters)], unique_letters)

cld_results$.group <- stringr::str_trim(cld_results$.group)
cld_results$.group <- reversed_letters[cld_results$.group]


letter_df_lmm <- cld_results %>%
  dplyr::select(Treatment, .group) %>%
  dplyr::rename(Letter = .group) %>%
  dplyr::mutate(Letter = stringr::str_trim(Letter))

y_pos_lmm <- data10dpf %>%
  group_by(Treatment) %>%
  summarise(Settlement = max(Settlement)) %>%
  left_join(letter_df_lmm, by = "Treatment") %>%
  mutate(Settlement = Settlement + 5)

ggplot(data10dpf, aes(x = Treatment, y = Settlement)) +
  geom_boxplot(width = 0.5) +
  geom_point(aes(shape = Family), size = 2) +
  geom_text(data = y_pos_lmm, aes(label = Letter, y = Settlement), vjust = 0) +
  labs(x = "", y = "Settlement (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = .5),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )


#################################################
# Bar plot

data_means <- data10dpf %>%
  group_by(Treatment) %>%
  summarise(mean = mean(Settlement),
            se = sd(Settlement) / sqrt(n()),
            ci = qt(0.975, df = n() - 1) * se)

ggplot(data_means, aes(x = Treatment, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkgrey", color = "black", width = 0.5) +
  geom_errorbar(aes(ymin = pmax(mean - ci, 0), ymax = pmin(mean + ci, 100)), width = 0.35) +
  geom_text(data = y_pos_lmm, aes(label = Letter, y = Settlement), vjust = 0) +
  labs(x = "", y = "Cumulative settlement at 10 dpf (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = .5),
    axis.title.y = element_text(margin = margin(r = 8))
  )
