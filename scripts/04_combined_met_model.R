# ============================================================
# 04_combined_met_model.R
# Combined Multi-Environment Model — AI-REML via sommer
# Potato CET: EA21 and LBHT populations, pooled across environments
# ============================================================

# Run once if not already installed:
# install.packages("sommer")

library(sommer)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
project_dir   <- path.expand("~/Potato_CET_MultiEnvironment")
processed_dir <- file.path(project_dir, "data", "processed")
tables_dir    <- file.path(project_dir, "outputs", "tables")
figures_dir   <- file.path(project_dir, "outputs", "figures")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load analysis-ready data (same file Notebook 03 used in Python)
# ------------------------------------------------------------
master <- read.csv(file.path(processed_dir, "analysis_ready.csv"))

master$trial <- as.factor(master$trial)
master$geno  <- as.factor(master$geno)
master$rep   <- as.factor(master$rep)

cat("Rows:", nrow(master), " Columns:", ncol(master), "\n")
str(master[, c("trial", "geno", "population", "weight_g_total")])

# ------------------------------------------------------------
# Combined multi-environment AI-REML model, per population
# ------------------------------------------------------------
results <- list()

for (pop in c("EA21", "LBHT")) {
  
  df_pop <- droplevels(subset(master, population == pop & !is.na(weight_g_total)))
  
  fit <- mmer(
    weight_g_total ~ trial,
    random = ~ geno + geno:trial,
    rcov   = ~ units,
    data   = df_pop
  )
  
  vc <- summary(fit)$varcomp
  var_g  <- vc["geno.weight_g_total-weight_g_total", "VarComp"]
  var_ge <- vc["geno:trial.weight_g_total-weight_g_total", "VarComp"]
  var_e  <- vc["units.weight_g_total-weight_g_total", "VarComp"]
  
  n_env <- length(unique(df_pop$trial))
  h2 <- var_g / (var_g + var_ge / n_env + var_e / n_env)
  
  cat("\n===", pop, "===\n")
  cat("Var_G  =", var_g, "\n")
  cat("Var_GE =", var_ge, "\n")
  cat("Var_E  =", var_e, "\n")
  cat("H2     =", round(h2, 3), "\n")
  
  results[[pop]] <- list(fit = fit, var_g = var_g, var_ge = var_ge,
                         var_e = var_e, n_env = n_env, h2 = h2)
}

# ------------------------------------------------------------
# Save variance component summary table
# ------------------------------------------------------------
variance_summary <- data.frame(
  Population = names(results),
  Var_G  = sapply(results, function(x) x$var_g),
  Var_GE = sapply(results, function(x) x$var_ge),
  Var_E  = sapply(results, function(x) x$var_e),
  Environments = sapply(results, function(x) x$n_env),
  H2 = round(sapply(results, function(x) x$h2), 3)
)

write.csv(
  variance_summary,
  file.path(tables_dir, "combined_met_variance_components.csv"),
  row.names = FALSE
)

print(variance_summary)
cat("\nSaved to:", file.path(tables_dir, "combined_met_variance_components.csv"), "\n")