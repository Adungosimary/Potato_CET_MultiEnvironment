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

# ------------------------------------------------------------
# Plot-size correction: EA21 uses 20-plant plots, LBHT uses 10-plant plots
# Raw weight_g_total is NOT comparable across populations without normalizing
# ------------------------------------------------------------

# Check ntp consistency within each population first
cat("ntp by population and trial:\n")
print(table(master$population, master$ntp, useNA = "always"))

# Njoro-LBHT has ntp missing — fill with LBHT's known constant (10),
# since ntp is consistent across all other LBHT trials
master$ntp[is.na(master$ntp) & master$population == "LBHT"] <- 10
master$ntp[is.na(master$ntp) & master$population == "EA21"] <- 20

# Per-plant yield — the fair cross-population metric
master$yield_per_plant_g <- master$weight_g_total / master$ntp

cat("\nPlot size confirmed:\n")
print(aggregate(ntp ~ population, data = master, FUN = function(x) unique(x)))


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
# Figure: Yield distribution by trial and population (RAW, plot-level)
# Kept for within-population reference only — NOT valid for
# cross-population magnitude comparison (see per-plant figure below)
# ------------------------------------------------------------
p_yield <- master %>%
  filter(!is.na(weight_g_total)) %>%
  ggplot(aes(x = location, y = weight_g_total / 1000, fill = population)) +
  geom_boxplot(outlier.alpha = 0.4, width = 0.6) +
  facet_wrap(~population) +
  scale_fill_manual(values = c("EA21" = "#1B9E77", "LBHT" = "#D95F02")) +
  labs(
    title = "Tuber Yield Distribution Across Environments (Raw, Plot-Level)",
    subtitle = "Combined multi-environment trial, 2025 season \u2014 not corrected for plot size",
    x = "Location",
    y = "Total Yield (kg per plot)"
  ) +
  theme_pub()

p_yield

ggsave(file.path(figures_dir, "Figure_YieldDistribution_ByLocation.png"),
       plot = p_yield, width = 10, height = 6, dpi = 600, bg = "white")

# ------------------------------------------------------------
# Figure: Per-plant yield — plot-size-corrected cross-population comparison
# EA21 = 20 plants/plot, LBHT = 10 plants/plot; raw totals are NOT
# comparable across populations without this normalization
# ------------------------------------------------------------
p_yield_perplant <- master %>%
  filter(!is.na(yield_per_plant_g)) %>%
  ggplot(aes(x = location, y = yield_per_plant_g, fill = population)) +
  geom_boxplot(outlier.alpha = 0.4, width = 0.6) +
  facet_wrap(~population) +
  scale_fill_manual(values = c("EA21" = "#1B9E77", "LBHT" = "#D95F02")) +
  labs(
    title = "Per-Plant Tuber Yield Across Environments",
    subtitle = "Normalized for plot size (EA21: 20 plants/plot; LBHT: 10 plants/plot)",
    x = "Location", y = "Yield per Plant (g)"
  ) +
  theme_pub()

p_yield_perplant

ggsave(file.path(figures_dir, "Figure_YieldPerPlant_ByLocation.png"),
       plot = p_yield_perplant, width = 10, height = 6, dpi = 600, bg = "white")

# Corrected summary table — this is the one to quote in the abstract/poster
# for any cross-population yield magnitude comparison
yield_perplant_summary <- master %>%
  group_by(trial, location, population) %>%
  summarise(
    N_plots = n(),
    Mean_yield_per_plant_g = mean(yield_per_plant_g, na.rm = TRUE),
    SD_yield_per_plant_g = sd(yield_per_plant_g, na.rm = TRUE),
    .groups = "drop"
  )

print(yield_perplant_summary)

write.csv(yield_perplant_summary, file.path(tables_dir, "yield_perplant_summary_by_trial.csv"), row.names = FALSE)

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
# Summary table: raw yield by trial (plot-level, within-population use only)
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

cat("\nSaved figures and tables:\n")
cat(" - Figure_YieldDistribution_ByLocation.png (raw, within-population reference)\n")
cat(" - Figure_YieldPerPlant_ByLocation.png (plot-size-corrected, for cross-population use)\n")
cat(" - Figure_DryMatter_Distribution.png\n")
cat(" - yield_summary_by_trial.csv (raw)\n")
cat(" - yield_perplant_summary_by_trial.csv (corrected — quote this one for cross-population claims)\n")