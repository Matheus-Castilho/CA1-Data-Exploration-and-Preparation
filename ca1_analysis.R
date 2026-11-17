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



# ---------- SECTION 3: OUT-OF-RANGE VALUES AND OUTLIERS (Part i) -------------

# 3.1 Out-of-range check: shares are percentages, so they must be between 0 and 100,
#     and energy/emission/population values cannot be negative
share_cols <- names(df_model) %>% str_subset("_share_")
df_model %>%
  summarise(across(all_of(share_cols), list(min = min, max = max))) %>%
  pivot_longer(everything(), names_to = c("column", ".value"),
               names_pattern = "(.*)_(min|max)") %>%
  filter(min < 0 | max > 100)               # rows listed here are out of range

# 3.2 Count outliers per numeric column using the IQR rule (1.5 x IQR)
count_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
}
outlier_counts <- df_model %>%
  select(where(is.numeric), -year) %>%
  summarise(across(everything(), count_outliers)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_outliers") %>%
  mutate(pct_outliers = round(n_outliers / nrow(df_model) * 100, 1)) %>%
  arrange(desc(n_outliers))
print(outlier_counts, n = 15)

# 3.3 Boxplots of a few key variables (saved for the poster)
dir.create("figures", showWarnings = FALSE)
p <- df_model %>%
  select(renewables_share_elec, fossil_share_elec, carbon_intensity_elec,
         energy_per_capita) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Outlier check - key variables", x = NULL, y = NULL)
print(p)   # show the plot in the RStudio Plots pane
ggsave("figures/outliers_boxplot.png", p, width = 8, height = 5)

# 3.4 log check: compare raw vs log1p(energy_per_capita) distribution
p_skew <- df_model %>%
  transmute(raw = energy_per_capita, log_transformed = log1p(energy_per_capita)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Skewness check: energy_per_capita (raw vs log1p)", x = NULL, y = "Count")
print(p_skew)
ggsave("figures/energy_log_check.png", p_skew, width = 8, height = 4)

# ------------------------- NOTES - SECTION 3 ---------------------------
# I decided to keep all the outliers because they represent real country-level values
# rather than data entry errors (for example, small high-income or industrial nations
# legitimately have very high energy_per_capita). Removing them would drop valid cases
# from the analysis. Instead, I will apply a log transformation to skewed variables
# before running PCA so extreme values do not dominate the components.

# ---------- SECTION 4: EXPLORATORY DATA ANALYSIS (Part ii) -------------

# Q1. How does the renewable share of electricity vary across countries,
#     and how has the global median changed from 2000 to 2023?

# 4.1a Variation: distribution of renewables_share_elec across all country-years
p_q1a <- ggplot(df_model, aes(x = renewables_share_elec)) +
  geom_histogram(bins = 30) +
  labs(title = "Q1: Distribution of renewable share of electricity",
       x = "Renewables share of electricity (%)", y = "Count of country-years")
print(p_q1a)
ggsave("figures/q1_renewables_hist.png", p_q1a, width = 8, height = 5)

# 4.1b Change over time: yearly median (line) with the middle 50% of countries (ribbon)
# NOTE: 2024 has only 55 countries (vs ~180 in other years) after removing rows with
#       missing values, so its median is not comparable. We stop the trend at 2023.
q1_yearly <- df_model %>%
  filter(year <= 2023) %>%
  group_by(year) %>%
  summarise(median_share = median(renewables_share_elec),
            q25 = quantile(renewables_share_elec, 0.25),
            q75 = quantile(renewables_share_elec, 0.75),
            n_countries = n())
p_q1b <- ggplot(q1_yearly, aes(x = year)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3) +
  geom_line(aes(y = median_share), linewidth = 1) +
  labs(title = "Q1: Median renewable share of electricity, 2000-2023",
       subtitle = "Line = median across countries, band = middle 50% of countries",
       x = "Year", y = "Renewables share of electricity (%)")
print(p_q1b)
ggsave("figures/q1_renewables_trend.png", p_q1b, width = 8, height = 5)

# 4.1c Key numbers to quote in the notes
q1_yearly %>% filter(year %in% c(2000, 2023))

# ------------------------- NOTES - Q1 ---------------------------
# Renewable share is polarised across country-years: about a third of observations are
# below 5% and about 15% are above 80% (possibly hydro-heavy countries, to be checked in Q2).
# The yearly median roughly doubled from 14.2% in 2000 to 27.3% in 2023, driven mostly by
# the bottom of the distribution: the lower quartile rose from 0% to 7.6%, while the upper
# quartile stayed around 62-63%. Even so, in 2023 half of countries still produced less than
# about 27% of their electricity from renewables. 2024 was excluded because only 55
# countries remained after cleaning, so its median is not comparable.
