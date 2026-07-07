# ============================================================
# 05_gxe_visualization.R
# Publication figure: Variance Components of the Pooled
# Multi-Environment Models (EA21 vs LBHT)
# ============================================================

#install.packages("tidyverse")

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
project_dir <- path.expand("~/Potato_CET_MultiEnvironment")
tables_dir  <- file.path(project_dir, "outputs", "tables")
figures_dir <- file.path(project_dir, "outputs", "figures")

# ------------------------------------------------------------
# Load variance components from Notebook 04 — no hardcoded numbers
# ------------------------------------------------------------
variance_summary <- read.csv(file.path(tables_dir, "combined_met_variance_components.csv"))
print(variance_summary)

# Reshape to long format for plotting
variance_df <- variance_summary %>%
  select(Population, Var_G, Var_GE, Var_E) %>%
  pivot_longer(
    cols = c(Var_G, Var_GE, Var_E),
    names_to = "Component",
    values_to = "Variance"
  ) %>%
  mutate(
    Component = recode(Component,
                       "Var_G" = "Genotype",
                       "Var_GE" = "Genotype × Environment",
                       "Var_E" = "Residual"),
    Component = factor(Component,
                       levels = c("Genotype", "Genotype × Environment", "Residual"))
  )

variance_pct <- variance_df %>%
  group_by(Population) %>%
  mutate(Total = sum(Variance), Percent = 100 * Variance / Total)

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
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10)
    )
}

component_colors <- c(
  "Genotype" = "#2E8B57",
  "Genotype × Environment" = "#E69F00",
  "Residual" = "#8F8F8F"
)

# ------------------------------------------------------------
# Panel A — Absolute variance
# ------------------------------------------------------------
p1 <- ggplot(variance_df, aes(Component, Variance / 1e6, fill = Component)) +
  geom_col(width = 0.65, colour = "black") +
  facet_wrap(~Population) +
  geom_text(aes(label = round(Variance / 1e6, 1)), vjust = -0.4, fontface = "bold", size = 4) +
  scale_fill_manual(values = component_colors) +
  labs(title = "A. Absolute Variance Components", x = "", y = "Variance (×10^6)") +
  theme_pub()

# ------------------------------------------------------------
# Panel B — Relative contribution
# ------------------------------------------------------------
p2 <- ggplot(variance_pct, aes(Population, Percent, fill = Component)) +
  geom_col(width = 0.65, colour = "black") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_stack(vjust = 0.5), colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = component_colors) +
  labs(title = "B. Relative Contribution", x = "", y = "Percent of Total Variance") +
  theme_pub()

# ------------------------------------------------------------
# Combine panels (requires patchwork)
# ------------------------------------------------------------
# install.packages("patchwork")  # run once if needed
library(patchwork)

h2_ea21 <- variance_summary$H2[variance_summary$Population == "EA21"]
h2_lbht <- variance_summary$H2[variance_summary$Population == "LBHT"]

figure1 <- p1 + p2 +
  plot_annotation(
    title = "Variance Components of the Pooled Multi-environment Mixed Models",
    subtitle = paste0("EA21 (H² = ", h2_ea21, "): G×E exceeded genotypic variance.  ",
                      "LBHT (H² = ", h2_lbht, "): genotypic variance predominated."),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  )

figure1

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------
ggsave(filename = file.path(figures_dir, "Figure1_VarianceComponents_Combined.png"),
       plot = figure1, width = 12, height = 6, dpi = 600, bg = "white")

ggsave(filename = file.path(figures_dir, "Figure1_VarianceComponents_Combined.pdf"),
       plot = figure1, width = 12, height = 6)

cat("\nSaved figure to:", file.path(figures_dir, "Figure1_VarianceComponents_Combined.png"), "\n")