
library(tidyverse)

# data import

data <- read.csv("../data/ficoSettlementData.csv")

data$Treatment <- factor(data$Treatment, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("Conspecific", "Biofilm", "Lab Biof", "Control"))

# filter out lab biofilm
data <- data %>%
  filter(Treatment != "Lab Biof")


data$Score <- factor(data$Score,
                     levels = c(0, 1, 2, 3, 4, 5),
                     labels = c("Missing", "Juvenile", "Swimming", "Stuck", "Dead", "Part Meta"))

data$Score <- factor(data$Score, levels = c("Juvenile", "Part Meta", "Swimming", "Stuck", "Missing", "Dead"))

Larva <- paste(data$Family, data$Plate, data$Well, sep="-")
data <- cbind(data, Larva)

# data setup

fig1Data <- data %>%
  group_by(Family, DPF, Treatment) %>%
  summarise(
    settlement = (sum(Score == "Juvenile")/n()) * 100
  )

first_family <- unique(fig1Data$Family)[1]

label_data <- fig1Data %>%
  filter(Family == first_family) %>%
  group_by(Treatment) %>%
  arrange(DPF) %>%
  slice(4) %>%
  ungroup()

label_data <- label_data %>%
  mutate(label_y = settlement + 10)

# fig

fig1 <- ggplot(fig1Data, aes(x = DPF, y = settlement, color = Treatment, shape = Treatment, linetype = Treatment, fill = Treatment)) +
  geom_line(stat = "identity", linewidth = 1) +
  geom_point(stat = "identity", size = 2, stroke = 1) +
  scale_shape_manual(values = c(
    "Control" = 21,
    "Conspecific" = 16,
    "Biofilm" = 17
  )) +
  scale_color_manual(values = c(
    "Control" = "grey60",
    "Conspecific" = "black",
    "Biofilm" = "grey50"
  )) +
  scale_fill_manual(values = c(
    "Control" = "white",       # white fill for solid circle
    "Conspecific" = "black",
    "Biofilm" = "grey50"
  )) +
  scale_linetype_manual(values = c(
    "Control" = "22",
    "Conspecific" = "solid",
    "Biofilm" = "solid"
  )) +
  geom_text(
    data = label_data,
    aes(x = DPF, y = label_y, label = Treatment),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  facet_wrap(~ Family, labeller = labeller(Family = function(x) paste("Family", x))) +
  labs(
    x = "Days post-fertilization",
    y = "Cumulative settlement (%)",
    color = "Substrate",
    shape = "Substrate",
    linetype = "Substrate"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.spacing.x = unit(.6, "cm"),
    strip.text = element_text(size = 10)
  )

print(fig1)

# save plot

ggplot2::ggsave("Figure_2_sicb.TIFF", plot = fig1,
       width = 16.9, height = 11.8,
       units = "cm", dpi = 1200,
       compression = "lzq")
