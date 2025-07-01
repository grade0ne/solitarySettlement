library(tidyverse)
library(multcompView)
library(FSA)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(moments)

#################################################
# Data import

data <- read.csv("data/gregariousTestExp.csv")

data_percent <- data %>%
  mutate(
    Family = as.factor(Family),
    Treatment = as.factor(Treatment),
    Treatment = fct_recode(Treatment, "Conspecific" = "Adult"),
    Treatment = fct_relevel(Treatment, "Conspecific", "Mussel", "Control"),
    Settlement = (Settled / Total) * 100
  )

#################################################
# Kruskal-Wallis & Holm-corrected Dunn

kruskal.test(Settlement ~ Treatment, data = data_percent)

dunn_result <- dunnTest(Settlement ~ Treatment, data = data_percent, method = "holm")

#################################################
# t-test

data_noctrl_wide <- data_percent %>%
  filter(Treatment != "Control") %>%
  droplevels() %>%
  select(-Settled, -Total) %>%
  pivot_wider(names_from = Treatment, values_from = Settlement)

t.test(data_noctrl_wide$`Adult Tube`, data_noctrl_wide$Mussel, paired = TRUE)

#################################################
# LMM

model <- lmer(Settlement ~ Treatment + (1|Family), data = data_percent)

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
# Box plot (KW w/ Dunn)

dunn_df <- dunn_result$res

pvals <- dunn_df$P.adj
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

group_letters <- multcompLetters(pvals)$Letters
group_letters <- setNames(rev(letters), names(letters))

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



ggsave("Figure 1.TIFF", plot = bar_plot,
       width = 8.4, height = 8.4, units = "cm", dpi = 1200)


#################################################
# Box plot (t-test)

data_ttest <- data_percent %>%
  filter(Treatment != "Control") %>%
  droplevels()

ggplot(data_ttest, aes(x = Treatment, y = Settlement)) +
  geom_boxplot(stat = "boxplot", width = 0.5) +
  geom_point(aes(shape = Family), size = 2) +
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
# Bar of means with CI

data_means <- data_percent %>%
  group_by(Treatment) %>%
  summarise(mean = mean(Settlement),
            se = sd(Settlement) / sqrt(n()),
            ci = qt(0.975, df = n() - 1) * se)

emm <- emmeans(model, pairwise ~ Treatment, adjust = "tukey")
cld_results <- multcomp::cld(emm[[1]], Letters = letters)

unique_letters <- sort(unique(stringr::str_trim(cld_results$.group)), decreasing = TRUE)
reversed_letters <- setNames(letters[seq_along(unique_letters)], unique_letters)

cld_results$.group <- stringr::str_trim(cld_results$.group)
cld_results$.group <- reversed_letters[cld_results$.group]

letter_df_lmm <- cld_results %>%
  dplyr::select(Treatment, .group) %>%
  dplyr::rename(Letter = .group)

y_pos_lmm <- data_means %>%
  left_join(letter_df_lmm, by = "Treatment") %>%
  mutate(y_pos = pmin(mean + ci, 100) + 5)

ggplot(data_means, aes(x = Treatment, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", fill = "gray92", color = "black", width = 0.5) +
  geom_point(data = data_percent, aes(x = Treatment, y = Settlement, shape = Family), position = position_jitter(width = 0.075, height = 0), size = 2, fill = "black", color = "black") +
  scale_shape_manual(values = c(1, 2, 16, 17)) +
  geom_errorbar(aes(ymin = pmax(mean - ci, 0), ymax = pmin(mean + ci, 100)), width = 0.35) +
  geom_text(data = y_pos_lmm, aes(x = Treatment, y = y_pos, label = Letter), vjust = 0) +
  labs(x = "", y = "Settlement at 5 dpf (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = .5),
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()
  )

bar_plot <- ggplot(data_means, aes(x = Treatment, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", fill = "gray92", color = "black", width = 0.5, linewidth = 0.3) +
  geom_point(data = data_percent,
             aes(x = Treatment, y = Settlement, shape = Family),
             position = position_jitter(width = 0.075, height = 0),
             size = 1, fill = "black", color = "black") +  
  scale_shape_manual(values = c(1, 2, 16, 17)) +
  geom_errorbar(aes(ymin = pmax(mean - ci, 0), ymax = pmin(mean + ci, 100)), width = 0.35, linewidth = 0.3) +
  geom_text(data = y_pos_lmm, aes(x = Treatment, y = y_pos, label = Letter),
            vjust = 0, size = 3) +  
  labs(x = "", y = "Settlement at 5 dpf (%)") +
  theme_minimal(base_size = 12) +  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = 0.3),
    axis.title.y = element_text(size = 9, margin = margin(r = 3)),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.3, "lines"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_blank()
  )



ggsave("Figure 1.TIFF", plot = bar_plot,
       width = 8.4, height = 8.4, units = "cm", dpi = 1200, compression = 'lzw')
