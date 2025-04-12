# --- INDEX ---
# 0. Setup & factor leveling
# 1. Total number of larvae, and larvae per family
# 2. Larvae per family in each scoring category:
#       0	- Missing
#       1	- Juvenile
#       2	- Swimming
#       3	- Stuck
#       4	- Dead
#       5	- Partially Metamorphosed ('Part Meta')
# 3. Breakdown of each scoring category by treatment for each family
# 4. How often do part meta larvae become juveniles?
# -------------

# -------------
# 0

library(tidyverse)
library(ggplot2)
library(dplyr)

data <- read.csv("data/ficoSettlementData.csv")
glimpse(data)

data$Treatment <- factor(data$Treatment, 
                         levels = c(1, 2, 3, 4), 
                         labels = c("Conspecific", "Nat Biof", "Lab Biof", "Control"))
data$Score <- factor(data$Score,
                     levels = c(0, 1, 2, 3, 4, 5),
                     labels = c("Missing", "Juvenile", "Swimming", "Stuck", "Dead", "Part Meta"))

data$Score <- factor(data$Score, levels = c("Juvenile", "Part Meta", "Swimming", "Stuck", "Missing", "Dead"))

# -------------
# 1, 2, 3

    # Score summary for 10 DPF, grouped by Treatment and Family

scores10DPF <- data %>%
  filter(DPF == "10") %>%
  group_by(Treatment, Family) %>%
  summarise('Total Larvae' = length(Family),
            Juvenile = sum(Score == "Juvenile"),
            PartMeta = sum(Score == "Part Meta"),
            Swimming = sum(Score == "Swimming"),
            Stuck = sum(Score == "Stuck"),
            Dead = sum(Score == "Dead"),
            Missing = sum(Score == "Missing"))



    # Stacked bar for just 10 DPF:

graphData10 <- data %>%
  filter(DPF == "10") %>%
  group_by(Family, Treatment)

ggplot(data = graphData10, aes(x = Treatment, fill = Score)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Juvenile" = "#CC6677", 
                               "Part Meta" = "#E0AC3F",
                               "Swimming" = "#88CCEE",
                               "Stuck" = "#44AA99",
                               "Missing" = "#117733",
                               "Dead" = "#332288")) +
  scale_x_discrete(labels = c("Cs", "Fb", "Lb", "Ct")) +
  labs(fill = "Larvae score") +
  facet_wrap(~Family) +
  theme_minimal() +
  theme(panel.grid = element_blank())

    # Full score summary for 5-10 DPF, grouped by Treatment and Family

scoresFull <- data %>%
  group_by(DPF, Treatment, Family) %>%
  summarise(Total = length(Family),
            Juvenile = sum(Score == "Juvenile"),
            PartMeta = sum(Score == "Part Meta"),
            Swimming = sum(Score == "Swimming"),
            Stuck = sum(Score == "Stuck"),
            Dead = sum(Score == "Dead"),
            Missing = sum(Score == "Missing"))

    # Stacked bar for 5-10 DPF

graphDataFull <- data %>%
  group_by(DPF, Family, Treatment)

ggplot(data = graphDataFull, aes(x = Treatment, fill = Score)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Juvenile" = "#CC6677", 
                               "Part Meta" = "#E0AC3F",
                               "Swimming" = "#88CCEE",
                               "Stuck" = "#44AA99",
                               "Missing" = "#117733",
                               "Dead" = "#332288")) +
  scale_x_discrete(labels = c("Cs", "Fb", "Lb", "Ct")) +
  facet_grid(Family ~ DPF) +
  labs(fill = "Larvae score") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

# -------------
# 4

Larva <- paste(data$Family, data$Plate, data$Well, sep="-")
data <- cbind(data, Larva)

data <- data %>%
  group_by(Larva) %>%
  mutate(change1d = Score == "Part Meta" & lead(Score) == "Juvenile") %>%
  ungroup()

metaLarvae <- data %>%
  filter(Score=="Part Meta", DPF != 10) %>%
  group_by(Treatment, DPF) %>%
  summarize('Part Meta Larvae' = n(),
            Success = sum(change1d),
            Failure = sum(!change1d),
            'Perc Success' = sum(change1d) / n() * 100,
            .groups = "drop")
