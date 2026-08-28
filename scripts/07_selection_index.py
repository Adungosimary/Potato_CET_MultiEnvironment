# %%
from pathlib import Path
import numpy as np
import pandas as pd

PROJECT_DIR = Path.home() / "Potato_CET_MultiEnvironment"
TABLES_DIR = PROJECT_DIR / "outputs" / "tables"
FIGURES_DIR = PROJECT_DIR / "outputs" / "figures"

trait_wide = pd.read_csv(TABLES_DIR / "multitrait_blups_wide.csv")
print(trait_wide.shape)
trait_wide.head()
# %%
# %%
trait_cols = ["Total_Yield", "Marketable_Yield", "Late_Blight_90d", "Overall_Impression"]

def zscore(s):
    return (s - s.mean()) / s.std()

for col in trait_cols:
    trait_wide[f"z_{col}"] = trait_wide.groupby("Population")[col].transform(zscore)

trait_wide[[f"z_{c}" for c in trait_cols]].describe()
# %%
# %%
weights = {
    "z_Total_Yield": 0.35,
    "z_Marketable_Yield": 0.35,
    "z_Late_Blight_90d": -0.15,   # negative: lower blight severity is better
    "z_Overall_Impression": 0.15
}

trait_wide["Selection_Index"] = sum(
    trait_wide[col] * w for col, w in weights.items()
)

trait_wide[["Population", "geno", "Selection_Index"]].sort_values(
    ["Population", "Selection_Index"], ascending=[True, False]
).head(10)
# %%
# %%
missing_summary = trait_wide[trait_cols].isna().sum()
print("Missing BLUPs per trait:\n", missing_summary)

n_complete = trait_wide[trait_cols].notna().all(axis=1).sum()
n_total = len(trait_wide)
print(f"\nGenotypes with all 4 traits: {n_complete} / {n_total}")

n_missing_selection_index = trait_wide["Selection_Index"].isna().sum()
print(f"Genotypes silently dropped from ranking (NaN Selection_Index): {n_missing_selection_index}")
# %%
# %%
dropped = trait_wide.loc[trait_wide["Selection_Index"].isna(), ["Population", "geno"] + trait_cols]
print("Genotypes excluded from selection index (missing Total_Yield BLUP):")
dropped
# %%
# %%
# %% Refresh Token

checks = ["Shangi", "Sherekea", "Unica", "Wanjiku", "Sagitta", "NARI002", "NARI004", "NARI005"]

trait_wide["Type"] = np.where(trait_wide["geno"].isin(checks), "Commercial Check", "Elite Clone")

top20 = (
    trait_wide.dropna(subset=["Selection_Index"])
    .sort_values(["Population", "Selection_Index"], ascending=[True, False])
    .groupby("Population")
    .head(20)
)

for pop, group in top20.groupby("Population"):
    print("="*70)
    print(pop)
    print(group[["geno", "Type", "Selection_Index"]].to_string(index=False))
# %%
# %%
trait_wide.to_csv(TABLES_DIR / "selection_index_full.csv", index=False)
top20.to_csv(TABLES_DIR / "top20_selection_index.csv", index=False)

print("Saved: selection_index_full.csv")
print("Saved: top20_selection_index.csv")
# %%
# %%
import matplotlib.pyplot as plt

fig, axes = plt.subplots(1, 2, figsize=(14, 8))

for ax, population in zip(axes, ["EA21", "LBHT"]):
    df_pop = top20.loc[top20["Population"] == population].sort_values("Selection_Index")
    colors = ["#D95F02" if t == "Commercial Check" else "#1B9E77" for t in df_pop["Type"]]

    ax.barh(df_pop["geno"], df_pop["Selection_Index"], color=colors, edgecolor="black")
    ax.set_title(f"{population} — Top 20 Selection Index")
    ax.set_xlabel("Selection Index (weighted z-score)")

plt.tight_layout()
plt.savefig(FIGURES_DIR / "selection_index_top20.png", dpi=300, bbox_inches="tight")
plt.show()
# %%
