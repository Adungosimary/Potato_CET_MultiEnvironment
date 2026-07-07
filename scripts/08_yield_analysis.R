# ============================================================
# 08_yield_analysis.R
# Final Yield Distribution and Dry Matter Analysis
# Potato CET: Combined Multi-Environment Publication Figures
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
project_dir   <- path.expand("~/Potato_CET_MultiEnvironment")
processed_dir <- file.path(project_dir, "data", "processed")
tables_dir    <- file.path(project_dir, "outputs", "tables")
figures_dir   <- file.path(project_dir, "outputs", "figures")

# ------------------------------------------------------------
# Load analysis-ready data (same source as Notebooks 03, 04, 06, 07)
# ------------------------------------------------------------
master <- read.csv(file.path(processed_dir, "analysis_ready.csv"))

master$trial <- as.factor(master$trial)
master$population <- as.factor(master$population)
master$location <- as.factor(master$location)

cat("Rows:", nrow(master), " Columns:", ncol(master), "\n")

# ------------------------------------------------------------
# Theme
# ------------------------------------------------------------
theme_pub <- function() {
  theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10)
    )
}

# ------------------------------------------------------------
# Figure: Yield distribution by trial and population
# ------------------------------------------------------------
p_yield <- master %>%
  filter(!is.na(weight_g_total)) %>%
  ggplot(aes(x = location, y = weight_g_total / 1000, fill = population)) +
  geom_boxplot(outlier.alpha = 0.4, width = 0.6) +
  facet_wrap(~population) +
  scale_fill_manual(values = c("EA21" = "#1B9E77", "LBHT" = "#D95F02")) +
  labs(
    title = "Tuber Yield Distribution Across Environments",
    subtitle = "Combined multi-environment trial, 2025 season",
    x = "Location",
    y = "Total Yield (kg per plot)"
  ) +
  theme_pub()

p_yield

ggsave(file.path(figures_dir, "Figure_YieldDistribution_ByLocation.png"),
       plot = p_yield, width = 10, height = 6, dpi = 600, bg = "white")

# ------------------------------------------------------------
# Figure: Dry matter — Njoro EA21 only (structural limitation)
# ------------------------------------------------------------
dm_data <- master %>% filter(!is.na(dm))

cat("\nDry matter available in trial(s):\n")
print(table(dm_data$trial))

p_dm <- ggplot(dm_data, aes(x = dm)) +
  geom_histogram(bins = 25, fill = "#7570B3", colour = "black") +
  labs(
    title = "Dry Matter Content Distribution",
    subtitle = "Recorded only in KE25NJO-EA21 — not available for cross-environment comparison",
    x = "Dry Matter (%)",
    y = "Count"
  ) +
  theme_pub()

p_dm

ggsave(file.path(figures_dir, "Figure_DryMatter_Distribution.png"),
       plot = p_dm, width = 8, height = 6, dpi = 600, bg = "white")

# ------------------------------------------------------------
# Summary table: yield by trial
# ------------------------------------------------------------
yield_summary <- master %>%
  group_by(trial, location, population) %>%
  summarise(
    N_plots = n(),
    N_yield_obs = sum(!is.na(weight_g_total)),
    Mean_yield_kg = mean(weight_g_total, na.rm = TRUE) / 1000,
    SD_yield_kg = sd(weight_g_total, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

print(yield_summary)

write.csv(yield_summary, file.path(tables_dir, "yield_summary_by_trial.csv"), row.names = FALSE)

cat("\nSaved figures and yield_summary_by_trial.csv\n")