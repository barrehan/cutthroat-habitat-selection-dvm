library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmerTest)
library(tidyr)
library(vegan)


df <- read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv")

# Preprocess timestamps and create fish_day ID
df <- df %>%
  mutate(
    date.time = as.POSIXct(date.time, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles"),
    date = as.Date(date.time),
    hour = format(date.time, "%H:00"),
    fish_day = paste(ibutton.id, date, sep = "_")
  )%>%
  filter(date >= as.Date("2021-08-01") & date <= as.Date("2021-08-07"))  # <- FILTER for August 1–7

# Filter to used habitat only (case == 1)
used <- df %>%
  filter(case == 1)

# Create wide matrix of mean depth per fish_day × hour
depth_matrix <- used %>%
  group_by(fish_day, hour) %>%
  summarize(mean_depth = mean(depth, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = hour, values_from = mean_depth)

# Drop incomplete rows
depth_matrix_clean <- depth_matrix %>%
  drop_na()

# Run PCA on depth patterns
pca_input <- depth_matrix_clean[, -1]
depth_pca <- prcomp(pca_input, scale. = TRUE)
summary(depth_pca)

# Get PCA scores and reattach metadata
pca_scores <- as.data.frame(depth_pca$x) %>%
  mutate(fish_day = depth_matrix_clean$fish_day) %>%
  separate(fish_day, into = c("ibutton.id", "date"), sep = "_", remove = FALSE) %>%
  left_join(df %>% distinct(fish_day, site), by = "fish_day")

# Convert site names to proper labels
pca_scores$site <- factor(pca_scores$site,
                          levels = c("norwood", "blue.ruin"),
                          labels = c("Norwood", "Blue Ruin"))

# Create PCA plot
p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = site)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Norwood" = "#A5236E", "Blue Ruin" = "#FF5050")) +
  labs(x = "PC1", y = "PC2", color = "Site") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_blank(),
    text = element_text(size = 20, family = "serif")
  )

ggsave(p, filename = "results/figures/ibutton/PCA.analysis.scatterplot.png",
       width = 35, height = 30, units = "cm")

summary(depth_pca)

round(depth_pca$rotation[, 1:2], 3)

# Run PERMANOVA on the scaled PCA input matrix
adonis_result <- adonis2(pca_input ~ site, data = pca_scores, method = "euclidean", permutations = 999)
print(adonis_result)

# Principal component analysis (PCA) revealed that fish-day depth profiles were structured 
# along two primary behavioral gradients: overall depth use (PC1, 68% of variance explained) 
# and diel vertical migration strength (PC2, 10%). Fish at Blue Ruin scored higher on PC2, 
# consistent with stronger DVM behavior, while Norwood fish exhibited deeper overall depth 
# use (higher PC1 scores). Blue Ruin fish-days were also more widely distributed in PCA 
# space, indicating greater behavioral variability across individuals or days. These 
# patterns were supported by PCA loadings and a PERMANOVA (R² = 0.07, p = 0.001), 
# suggesting that site explains a small but significant portion of the multivariate 
# variation.
