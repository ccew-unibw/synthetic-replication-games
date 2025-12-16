import pandas as pd
import numpy as np

data = pd.read_stata('material/repdata.dta')
dedup = data.drop_duplicates(subset="CaseID")
cols = dedup.columns
dedup = dedup[[cols[0]] + list(cols[15:])]

dedup["FB09"] = dedup["FB09"]*100
dedup["ppstaten"] = dedup["ppstaten"].str.upper()
dedup["fisexp2"] = dedup["fisexp2"].replace({1: "low", 2:"high"})

dedup = dedup.dropna(subset=["W1_Q1", "W1_Q2", "W1_Q3", "W1_Q4", "W1_Q5", "W1_Q6", "W1_Q7",	"W1_Q8a"])

occ_cols = ["OCC_GA", "OCC_WA", "OCC_NU", "OCC_CC", "OCC_JA",
            "OCC_CO", "OCC_FA", "OCC_RS", "OCC_DR", "OCC_CP", "OCC_TE"]

occ_label = {
    "OCC_GA": "gardener",
    "OCC_WA": "waiter",
    "OCC_NU": "nurse",
    "OCC_CC": "child care provider",
    "OCC_JA": "janitor",
    "OCC_CO": "construction",
    "OCC_FA": "financial analyst",
    "OCC_RS": "research scientist",
    "OCC_DR": "doctors",
    "OCC_CP": "computer programmers",
    "OCC_TE": "teacher",  
}


mask = dedup[occ_cols].eq(1)

dedup["occupation"] = np.where(
    mask.any(axis=1),
    mask.idxmax(axis=1).map(occ_label),
    pd.NA
)


dedup = dedup.drop(columns=occ_cols)
dedup = dedup.drop(columns=["fisexp1"])
dedup = dedup.astype("object")
dedup = dedup.fillna("No response")

dedup.to_excel("dedup_df.xlsx", index=False)