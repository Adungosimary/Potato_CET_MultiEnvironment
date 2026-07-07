# %%
from pathlib import Path
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

PROJECT_DIR = Path.home() / "Potato_CET_MultiEnvironment"
PROCESSED_DIR = PROJECT_DIR / "data" / "processed"
TABLES_DIR = PROJECT_DIR / "outputs" / "tables"
FIGURES_DIR = PROJECT_DIR / "outputs" / "figures"

master = pd.read_csv(PROCESSED_DIR / "analysis_ready.csv")
print(f"Rows: {master.shape[0]:,}, Columns: {master.shape[1]}")

# %%
correlation_traits = [
    "weight_g_total",
    "calculated_total_count",
    "dm",
    "late_blight_60_days",
    "late_blight_90_days",
]

for t in correlation_traits:
    present = t in master.columns
    n_avail = master[t].notna().sum() if present else 0
    print(f"{t:25s} present={present}  n_available={n_avail}")
# %%
# %%
# Diagnostic: where does dry matter and blight data actually come from?

count_cols = [c for c in master.columns if "count" in c.lower() or "tuber" in c.lower() or "ntp" in c.lower()]
print("Candidate tuber-count columns:", count_cols)

print("\nDry matter (dm) availability by trial:")
print(master.groupby("trial")["dm"].apply(lambda x: x.notna().sum()))

print("\nLate blight 90-day availability by trial:")
print(master.groupby("trial")["late_blight_90_days"].apply(lambda x: x.notna().sum()))
# %%
# %%
print(master.groupby("trial")["ntp"].apply(lambda x: x.notna().sum()))
print(master.groupby("trial")["ntp"].describe()[["min", "max", "mean"]])