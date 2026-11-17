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



# ---------- SECTION 2: CLEAN THE COLUMNS (Part i) -------------

# 2.1 Drop columns with more than 50% missing values
#     (mostly derived growth-rate columns, see Section 1 notes)
cols_to_drop <- missing %>% filter(pct_missing > 50) %>% pull(column)
length(cols_to_drop)                       # how many columns dropped
df_clean <- df %>% select(-all_of(cols_to_drop))
dim(df_clean)                              # size after dropping

# 2.2 Check for duplicates: each country-year should appear only once
df_clean %>% count(country, year) %>% filter(n > 1) %>% nrow()

# 2.3 Missing values left in the remaining columns (count per column)
colSums(is.na(df_clean)) %>% sort(decreasing = TRUE) %>% head(15)

# 2.4 How many rows survive if we drop every row that still has a missing value?
nrow(drop_na(df_clean))                           # keeping gdp
nrow(drop_na(select(df_clean, -gdp)))             # dropping gdp first

# 2.5 DECISION: drop the fuel-production columns and gdp-based columns.
#     Dropping every row with any NA would leave only 2,326 rows (< 3,000 required),
#     mainly because production columns are empty for non-producing countries
#     and gdp / energy_per_gdp are about 32% missing.
df_model <- df_clean %>%
  select(-matches("_prod_|_production$"),   # coal/gas/oil production columns
         -gdp, -energy_per_gdp) %>%         # gdp and its ratio
  drop_na()                                 # remove rows still containing NA

# 2.6 Check the result against the brief (at least 3,000 rows and 7 columns)
dim(df_model)
n_distinct(df_model$country)

# ------------------------- NOTES - SECTION 2 ---------------------------
# Dropping all rows with missing values after removing the >50% empty columns left only
# 2,326 rows (below the 3,000 minimum). Dropping gdp alone did not help (still 2,326),
# because coal/gas/oil production are blank for non-producing countries.
# I therefore dropped the production columns along with gdp and energy_per_gdp, leaving
# 4,322 complete rows (184 countries, 2000-2024) with no NAs. gdp is also ~32% missing
# and less relevant, since my question focuses on the electricity mix, not national wealth.

