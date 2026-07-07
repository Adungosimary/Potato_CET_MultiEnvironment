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
# %%
# %%
MARKETABLE_THRESHOLD_MM = 30  # >=28mm convention (KALRO), rounded to nearest bin boundary

bin_lower_bounds = {
    "weight_g_0_10": 0, "weight_g_10_30": 10, "weight_g_30_35": 30,
    "weight_g_35_40": 35, "weight_g_40_45": 40, "weight_g_45_50": 45,
    "weight_g_50_55": 50, "weight_g_55_60": 55, "weight_g_60_65": 60,
    "weight_g_65_70": 65, "weight_g_70": 70
}

marketable_cols = [c for c, lb in bin_lower_bounds.items() if lb >= MARKETABLE_THRESHOLD_MM]
print("Marketable size-class columns:", marketable_cols)

master["marketable_yield_g"] = master[marketable_cols].sum(axis=1, skipna=True)
print(master[["weight_g_total", "marketable_yield_g"]].describe())
# %%
# %%
trait_list = {
    "weight_g_total": "Total_Yield",
    "marketable_yield_g": "Marketable_Yield",
    "late_blight_90_days": "Late_Blight_90d",
    "overall_impression": "Overall_Impression"
}

all_trait_blups = []

for trait_col, trait_label in trait_list.items():
    for population in ["EA21", "LBHT"]:

        df_pop = master.loc[
            (master["population"] == population) &
            (master[trait_col].notna())
        ].copy()

        df_pop["trial"] = df_pop["trial"].astype("category")
        df_pop["rep"] = df_pop["rep"].astype("category")

        try:
            model = smf.mixedlm(f"{trait_col} ~ trial + rep", data=df_pop, groups=df_pop["geno"])
            fit = model.fit(reml=True)
            re = fit.random_effects
            for geno, effect in re.items():
                all_trait_blups.append({
                    "Population": population,
                    "geno": geno,
                    "Trait": trait_label,
                    "BLUP": effect.iloc[0]
                })
            print(f"{population} - {trait_label}: converged, n={len(df_pop)}")
        except Exception as e:
            print(f"{population} - {trait_label}: FAILED — {e}")

trait_blup_df = pd.DataFrame(all_trait_blups)
trait_blup_df.head()
# %%
# %%
trait_wide = trait_blup_df.pivot_table(
    index=["Population", "geno"],
    columns="Trait",
    values="BLUP"
).reset_index()

print(trait_wide.shape)
trait_wide.head()
# %%
# %%
import seaborn as sns
import matplotlib.pyplot as plt

trait_cols = ["Total_Yield", "Marketable_Yield", "Late_Blight_90d", "Overall_Impression"]

fig, axes = plt.subplots(1, 2, figsize=(14, 6))

for ax, population in zip(axes, ["EA21", "LBHT"]):
    df_pop = trait_wide.loc[trait_wide["Population"] == population, trait_cols]
    corr = df_pop.corr()

    sns.heatmap(corr, annot=True, fmt=".2f", cmap="RdBu_r", center=0,
                vmin=-1, vmax=1, ax=ax, cbar=True, square=True)
    ax.set_title(f"{population} — Multi-Trait BLUP Correlations")

plt.tight_layout()
plt.savefig(FIGURES_DIR / "multitrait_blup_correlations.png", dpi=300, bbox_inches="tight")
plt.show()
# %%
# %%
print(trait_wide.loc[trait_wide["Population"] == "LBHT", ["Total_Yield", "Marketable_Yield"]].corr())
# %%
# %%
trait_wide.to_csv(TABLES_DIR / "multitrait_blups_wide.csv", index=False)

corr_ea21 = trait_wide.loc[trait_wide["Population"] == "EA21", trait_cols].corr()
corr_lbht = trait_wide.loc[trait_wide["Population"] == "LBHT", trait_cols].corr()

corr_ea21.to_csv(TABLES_DIR / "correlation_matrix_EA21.csv")
corr_lbht.to_csv(TABLES_DIR / "correlation_matrix_LBHT.csv")

print("Saved: multitrait_blups_wide.csv")
print("Saved: correlation_matrix_EA21.csv")
print("Saved: correlation_matrix_LBHT.csv")

# %%
