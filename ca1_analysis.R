# =============================================================
# CA1 - Data Exploration & Preparation (Energy domain)
# Dataset: Our World in Data (OWID) energy dataset
# Source:  https://github.com/owid/energy-data
# Research question (draft): What distinguishes high-renewable
#   electricity systems from fossil-dependent ones?
# Run with the working directory set to the CA1_DE folder.
# =============================================================

library(tidyverse)

# ---------- SECTION 1: LOAD AND INSPECT (Part i) -------------

# 1.1 Load the raw data
raw <- read_csv("data/owid-energy-data.csv")

# 1.2 Size and structure of the raw dataset
dim(raw)          # rows, columns
glimpse(raw)      # column names and types

# 1.3 OWID mixes real countries with aggregates (World, regions,
#     "(Ember)" groups). Real countries have an iso_code.
raw %>% filter(is.na(iso_code)) %>% distinct(country) %>% head(20)

# 1.4 Keep country rows only, from 2000 onward
df <- raw %>%
  filter(!is.na(iso_code), year >= 2000)
dim(df)

# 1.5 Percentage of missing values per column (highest first)
missing <- df %>%
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))
print(missing, n = 30)

# --------------- NOTES - SECTION 1 ----------------------------
# # About half of the columns (64 of 130) are more than 50% missing;
# The worst are the derived "*_cons_change_pct" growth-rate columns (e.g. nuclear
# 86.4%, biofuel 80.5%), so I plan to drop them. The columns I need for my question are
# well populated: the electricity-share columns (renewables, fossil, coal, solar, wind,
# nuclear, hydro), carbon_intensity_elec and population are all under 8% missing,
# either drop or impute.

