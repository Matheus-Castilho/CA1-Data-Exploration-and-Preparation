# =============================================================
# CA1 - Data Exploration & Preparation (Energy domain)
# Dataset: Our World in Data (OWID) energy dataset
# Source:  https://github.com/owid/energy-data
# Research question: What distinguishes high-renewable
#   electricity systems from fossil-dependent ones?
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
# About half of the columns (64 of 130) are more than 50% missing;
# The worst are the derived "*_cons_change_pct" growth-rate columns (e.g. nuclear
# 86.4%, biofuel 80.5%), so I plan to drop them. The columns I need for my question are
# well populated: the electricity-share columns (renewables, fossil, coal, solar, wind,
# nuclear, hydro), carbon_intensity_elec and population are all under 8% missing,
# either drop or impute.



# ---------- SECTION 2: CLEAN THE COLUMNS (Part i) -------------

# 2.1 Drop columns with more than 50% missing values
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
# So I dropped the production columns along with gdp and energy_per_gdp, leaving
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

# 3.4 Skewness check: compare raw vs log1p(energy_per_capita)
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


# Q2. How does the electricity mix differ between the most and least
#     renewable countries?

# 4.2a Build one row per country: average over the latest full years (2019-2023)
#      (2024 is excluded, see the note under Q1)
q2_mix <- c("coal_share_elec", "gas_share_elec", "oil_share_elec", "nuclear_share_elec",
            "hydro_share_elec", "wind_share_elec", "solar_share_elec",
            "other_renewables_share_elec")

q2_country <- df_model %>%
  filter(year %in% 2019:2023) %>%
  group_by(country) %>%
  summarise(renewables = mean(renewables_share_elec),
            avg_generation = mean(electricity_generation),   # TWh
            across(all_of(q2_mix), mean)) %>%
  # Many tiny territories sit at exactly 0% or 100% (e.g. 0.01 TWh islands), which would
  # make the top/bottom 10 arbitrary, so we keep countries generating at least 5 TWh.
  filter(avg_generation >= 5)
nrow(q2_country)                              # countries kept

# 4.2b Top 10 and bottom 10 countries by renewable share
q2_groups <- bind_rows(
  q2_country %>% slice_max(renewables, n = 10, with_ties = FALSE) %>%
    mutate(group = "Top 10 renewable"),
  q2_country %>% slice_min(renewables, n = 10, with_ties = FALSE) %>%
    mutate(group = "Bottom 10 renewable")
)

# 4.2c Plot the mix of each country as a stacked bar
q2_plot_data <- q2_groups %>%
  pivot_longer(all_of(q2_mix), names_to = "source", values_to = "share") %>%
  mutate(source = str_remove(source, "_share_elec"),
         country = fct_reorder(country, renewables),
         group = factor(group, levels = c("Top 10 renewable", "Bottom 10 renewable")))

p_q2 <- ggplot(q2_plot_data, aes(x = share, y = country, fill = source)) +
  geom_col() +
  facet_wrap(~ group, scales = "free_y") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Q2: Electricity mix of the 10 most and 10 least renewable countries",
       subtitle = "Average 2019-2023, countries generating at least 5 TWh",
       x = "Share of electricity (%)", y = NULL, fill = "Source")
print(p_q2)
ggsave("figures/q2_mix_top_bottom.png", p_q2, width = 11, height = 5)

# 4.2d Average mix per group (numbers to quote in the notes)
q2_groups %>%
  group_by(group) %>%
  summarise(across(all_of(q2_mix), ~ round(mean(.x), 1)))

# ------------------------- NOTES - Q2 ---------------------------
# Among countries generating at least 5 TWh (2019-2023 average), the Top 10 renewable
# countries are overwhelmingly dominated by hydro (averaging 90.5%), with wind (2.5%)
# and solar (1.3%) playing much smaller supporting roles. In contrast, the Bottom 10
# rely almost entirely on fossil fuels, mostly gas (83.8%) and oil (9.3%), along with
# coal (6.4%), and many are hydrocarbon-producing countries. This suggests that
# "high-renewable" grids among the top 10 mostly reflect favourable geography for
# hydropower rather than recent solar and wind expansion.

# Q3. Is higher energy use per person linked to a higher or lower renewable share?

# 4.3a One row per country: average 2019-2023, same 5 TWh filter as Q2
q3_data <- df_model %>%
  filter(year %in% 2019:2023) %>%
  group_by(country) %>%
  summarise(renewables = mean(renewables_share_elec),
            energy_per_capita = mean(energy_per_capita),
            avg_generation = mean(electricity_generation)) %>%
  filter(avg_generation >= 5)

# 4.3b Strength of the relationship. Spearman (rank) correlation is used because
#      energy_per_capita is heavily skewed (see Section 3).
cor(log10(q3_data$energy_per_capita), q3_data$renewables, method = "spearman")

# 4.3c Scatter plot with a smooth trend line (log scale on the x-axis)
p_q3 <- ggplot(q3_data, aes(x = energy_per_capita, y = renewables)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Q3: Energy use per person vs renewable share of electricity",
       subtitle = "One point per country, average 2019-2023, countries generating at least 5 TWh",
       x = "Energy per capita (log scale)", y = "Renewables share of electricity (%)")
print(p_q3)
ggsave("figures/q3_energy_vs_renewables.png", p_q3, width = 8, height = 5)

# 4.3d Median renewable share by energy-use band (numbers to quote in the notes)
q3_data %>%
  mutate(energy_band = cut(energy_per_capita, c(0, 10000, 30000, 60000, Inf),
                           labels = c("<10k", "10-30k", "30-60k", ">60k"))) %>%
  group_by(energy_band) %>%
  summarise(n = n(), median_renewables = round(median(renewables), 1))

# ------------------------- NOTES - Q3 ---------------------------
# There is a weak negative relationship between energy use per person and renewable share
# (Spearman r = -0.33, 101 countries generating at least 5 TWh). The median renewable share
# is highest in the lowest-use band (59.0% for <10k) and lowest in the top band (1.1% for
# >60k, only 11 countries), though the middle bands are similar (33-37%), so the decline
# is not smooth. At the very top end, countries split into two extremes: oil/gas-producing
# countries such as Qatar, Bahrain and Kuwait near 0% renewables, and hydro-rich nations
# such as Iceland and Norway near 100%. This is a correlation, not proof that energy use
# drives the renewable share.


# Q4. Which electricity-source shares move together?

# 4.4a Variables: the individual source shares, the two summary shares and carbon intensity
q4_vars <- c("coal_share_elec", "gas_share_elec", "oil_share_elec", "nuclear_share_elec",
             "hydro_share_elec", "wind_share_elec", "solar_share_elec",
             "other_renewables_share_elec", "renewables_share_elec", "fossil_share_elec",
             "carbon_intensity_elec")

# 4.4b Spearman correlation matrix (rank-based, because several variables are skewed)
q4_cor <- cor(df_model[q4_vars], method = "spearman")
round(q4_cor, 2)

# 4.4c Heatmap of the correlation matrix
q4_long <- as.data.frame(as.table(q4_cor)) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
  mutate(var1 = str_remove(var1, "_elec|_share"),
         var2 = str_remove(var2, "_elec|_share"))
p_q4 <- ggplot(q4_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       limits = c(-1, 1)) +
  labs(title = "Q4: Spearman correlation between electricity-source shares",
       subtitle = "All country-years, 2000-2024 (n = 4,322)", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_q4)
ggsave("figures/q4_correlation_heatmap.png", p_q4, width = 9, height = 7)

# 4.4d Strongest pairs (each pair listed once), to quote in the notes
as.data.frame(as.table(q4_cor)) %>%
  filter(as.character(Var1) < as.character(Var2)) %>%
  mutate(correlation = round(Freq, 2)) %>%
  arrange(desc(abs(correlation))) %>%
  select(Var1, Var2, correlation) %>%
  head(10)

# ------------------------- NOTES - Q4 ---------------------------
# The strongest correlations are mostly compositional trade-offs: hydro and fossil share
# are strongly negatively correlated (r = -0.91), since electricity shares sum to 100% and
# countries with large hydro resources need less fossil generation. Wind and solar share are
# moderately positively correlated (r = 0.48), suggesting countries tend to deploy both.
# Unexpectedly, wind and coal are also positively correlated (r = 0.43) rather than negative;
# one possible explanation is that large industrialised grids use both, but this was not tested.
# Summary variables (renewables, fossil) are built from the others, so their strong pairs are
# partly by construction.


# Q5. Are low-carbon electricity grids really the high-renewable ones?

# 4.5a One row per country: average 2019-2023, same 5 TWh filter as Q2 and Q3
q5_data <- df_model %>%
  filter(year %in% 2019:2023) %>%
  group_by(country) %>%
  summarise(renewables = mean(renewables_share_elec),
            fossil = mean(fossil_share_elec),
            nuclear = mean(nuclear_share_elec),
            carbon_intensity = mean(carbon_intensity_elec),   # gCO2 per kWh
            avg_generation = mean(electricity_generation)) %>%
  filter(avg_generation >= 5)

# 4.5b Correlation of carbon intensity with renewable share and with fossil share
cor(q5_data$carbon_intensity, q5_data$renewables, method = "spearman")
cor(q5_data$carbon_intensity, q5_data$fossil, method = "spearman")

# 4.5c Scatter: carbon intensity vs renewable share, coloured by nuclear share.
#      Nuclear is the missing piece: it is low-carbon but not renewable.
p_q5 <- ggplot(q5_data, aes(x = renewables, y = carbon_intensity, colour = nuclear)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(data = filter(q5_data, country %in% c("France", "Belgium")),
            aes(label = country), vjust = -1, colour = "black", size = 3) +
  scale_colour_viridis_c(name = "Nuclear share (%)") +
  labs(title = "Q5: Carbon intensity of electricity vs renewable share",
       subtitle = "One point per country, average 2019-2023, countries generating at least 5 TWh",
       x = "Renewables share of electricity (%)", y = "Carbon intensity (gCO2 per kWh)")
print(p_q5)
ggsave("figures/q5_carbon_vs_renewables.png", p_q5, width = 8, height = 5.5)

# 4.5d Low-carbon grids (under 150 gCO2/kWh) that are NOT mostly renewable
q5_data %>%
  filter(carbon_intensity < 150, renewables < 50) %>%
  arrange(carbon_intensity) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# ------------------------- NOTES - Q5 ---------------------------
# There is a strong negative correlation between renewable share and carbon intensity
# (Spearman r = -0.88), so higher renewable shares are associated with much cleaner grids;
# fossil share is an even stronger correlate (r = 0.96), partly by construction since
# carbon intensity is calculated from fossil generation. The only two countries with a
# low-carbon grid (under 150 gCO2/kWh) but a renewable share below 50% are France and
# Belgium (about 23% and 26% renewable). Colouring by nuclear share explains why: nuclear
# supplies about 67% and 45% of their electricity, which is low-carbon but not renewable.


# Q6. Which countries are unusual, and where does Ireland fit?

# 4.6a Change in renewable share between 2000 and 2023 (percentage points).
#      Countries need data in both years and at least 5 TWh generation in 2023.
q6_change <- df_model %>%
  filter(year %in% c(2000, 2023)) %>%
  select(country, year, renewables_share_elec, wind_share_elec, solar_share_elec,
         electricity_generation) %>%
  pivot_wider(names_from = year,
              values_from = c(renewables_share_elec, wind_share_elec, solar_share_elec,
                              electricity_generation)) %>%
  drop_na() %>%
  filter(electricity_generation_2023 >= 5) %>%
  mutate(change = renewables_share_elec_2023 - renewables_share_elec_2000,
         wind_change = wind_share_elec_2023 - wind_share_elec_2000,
         solar_change = solar_share_elec_2023 - solar_share_elec_2000)
nrow(q6_change)                                # countries compared
median(q6_change$change)                       # typical change

# 4.6b Biggest gainers (bar chart, Ireland highlighted)
q6_top <- q6_change %>% slice_max(change, n = 10, with_ties = FALSE)
p_q6a <- ggplot(q6_top, aes(x = change, y = fct_reorder(country, change),
                            fill = country == "Ireland")) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("grey60", "darkgreen")) +
  labs(title = "Q6: Biggest increases in renewable share of electricity, 2000 to 2023",
       subtitle = "Percentage-point change, countries generating at least 5 TWh in 2023",
       x = "Change in renewables share (percentage points)", y = NULL)
print(p_q6a)
ggsave("figures/q6_biggest_gains.png", p_q6a, width = 8, height = 5)

# 4.6c What drove the gains? (wind and solar change for the top 10) and the biggest falls
q6_top %>%
  transmute(country, change = round(change, 1), wind = round(wind_change, 1),
            solar = round(solar_change, 1))
q6_change %>% slice_min(change, n = 4, with_ties = FALSE) %>%
  transmute(country, share_2000 = round(renewables_share_elec_2000, 1),
            share_2023 = round(renewables_share_elec_2023, 1), change = round(change, 1))

# 4.6d Dublin angle: Ireland vs the median country, 2000-2023
q6_ireland <- df_model %>%
  filter(country == "Ireland", year <= 2023) %>%
  select(year, `Ireland: renewables` = renewables_share_elec,
         `Ireland: wind only` = wind_share_elec) %>%
  left_join(select(q1_yearly, year, `Median country: renewables` = median_share),
            by = "year") %>%
  pivot_longer(-year, names_to = "series", values_to = "share")
p_q6b <- ggplot(q6_ireland, aes(x = year, y = share, colour = series)) +
  geom_line(linewidth = 1) +
  labs(title = "Q6: Ireland's renewable and wind share vs the median country",
       x = "Year", y = "Share of electricity (%)", colour = NULL)
print(p_q6b)
ggsave("figures/q6_ireland_trend.png", p_q6b, width = 8, height = 5)

# ------------------------- NOTES - Q6 ---------------------------
# Compared to the typical country (median change of +2.5 percentage points between 2000 and
# 2023), the top 10 gainers saw large shifts, led by Denmark (+70.9 pp). Most were driven by
# wind and solar expansion, with exceptions such as Sudan, whose gain was mostly hydro.
# Ireland ranks #7 of 91 (+40.2 pp), and nearly all of its growth came from wind (+36.5 pp);
# it moved ahead of the median country from 2011 onwards. On the flip side, some formerly
# hydro-dominated countries such as Congo (100% to 23.5%) saw their renewable share drop,
# possibly because demand grew faster than hydro capacity or fossil generation was expanded
# (not tested). Because this compares only two years, rainfall variation in hydro output
# may exaggerate some changes.


# ---------- SECTION 5: FEATURE ENGINEERING AND ENCODING (Part iii) -------------

library(countrycode)

# 5.1 Add a continent column from the ISO country code (nominal categorical variable)
df_model <- df_model %>%
  mutate(continent = countrycode(iso_code, origin = "iso3c", destination = "continent"))

# 5.2 Check that every country was matched to a continent
sum(is.na(df_model$continent))                       # should be 0
df_model %>% filter(is.na(continent)) %>% distinct(country, iso_code)
table(df_model$continent)                            # rows per continent
df_model %>% group_by(continent) %>% summarise(n_countries = n_distinct(country))

# 5.3 Add an ordered renewable_group column (ordinal categorical variable)
#     low: up to 20%, medium: above 20% and up to 60%, high: above 60%
#     NOTE: derived from renewables_share_elec, so it must NOT be used as a PCA input.
#     It is only used to colour and interpret the PCA results.
df_model <- df_model %>%
  mutate(renewable_group = cut(renewables_share_elec,
                               breaks = c(-Inf, 20, 60, Inf),
                               labels = c("low", "medium", "high"),
                               ordered_result = TRUE))

# 5.4 Check group sizes
table(df_model$renewable_group)
round(prop.table(table(df_model$renewable_group)) * 100, 1)

# ------------------------- NOTES - SECTION 5 ---------------------------
# I added `continent` as a nominal grouping variable to see whether regional geography
# explains clusters in the later PCA, and `renewable_group` as an ordinal label to colour
# and compare low-, medium- and high-renewable systems without double-counting
# renewables_share_elec in the PCA inputs. The 20% and 60% cut-offs are a judgement call:
# 20% is roughly the overall median (so the low group is about half the data), and the
# cut-offs give three usable groups (about 50%, 26% and 24% of rows) separating
# low-renewable grids from mixed grids (20-60%) and renewable-dominant systems (>60%).


# ---------- SECTION 6: ENCODING SCHEMES (Part iii) -------------

# 6.1 One-hot encoding of continent (nominal: the continents have no natural order).
#     model.matrix() creates one 0/1 column per continent ("- 1" keeps all five columns).
continent_onehot <- model.matrix(~ continent - 1, data = df_model) %>%
  as.data.frame()
names(continent_onehot) <- str_replace(names(continent_onehot), "^continent", "continent_")

# 6.2 Check the one-hot columns: every row must contain exactly one 1,
#     and the column totals must match the continent counts from Section 5
table(rowSums(continent_onehot))
colSums(continent_onehot)
table(df_model$continent)

# 6.3 Ordinal (label) encoding of renewable_group (ordered: low < medium < high)
#     as.integer() on an ordered factor gives low = 1, medium = 2, high = 3
df_encoded <- bind_cols(df_model, continent_onehot) %>%
  mutate(renewable_group_num = as.integer(renewable_group))

# 6.4 Check the ordinal encoding and look at the encoded dataset
table(df_encoded$renewable_group, df_encoded$renewable_group_num)
dim(df_encoded)
df_encoded %>%
  select(country, year, continent, starts_with("continent_"),
         renewable_group, renewable_group_num) %>%
  head()

# ------------------------- NOTES - SECTION 6 ---------------------------
# One-hot encoding fits `continent` because continents are nominal categories with no
# natural order; assigning numbers like 1 to 5 would imply a false ranking or distance
# between regions. Ordinal (label) encoding suits `renewable_group` because low < medium
# < high is ordered, so mapping it to 1, 2, 3 preserves that order in a single column.


# ---------- SECTION 7: PREPARING THE DATA FOR PCA (Part iii) -------------

# 7.1 Choose the PCA input variables (13 numeric columns).
#     Included: the eight electricity-source shares, carbon intensity, and size / usage variables.
#     Excluded: renewables_share_elec, fossil_share_elec and low_carbon_share_elec (they are
#     sums of the source shares, so they would duplicate information; renewables_share_elec
#     also defines renewable_group), net imports and the *_change_* columns (negative values),
#     and all the categorical / ID columns (country, iso_code, year, continent, renewable_group).
pca_vars <- c("coal_share_elec", "gas_share_elec", "oil_share_elec", "nuclear_share_elec",
              "hydro_share_elec", "wind_share_elec", "solar_share_elec",
              "other_renewables_share_elec", "carbon_intensity_elec",
              "energy_per_capita", "per_capita_electricity",
              "electricity_generation", "greenhouse_gas_emissions")

# 7.2 Skewness of each variable before transforming (0 = symmetric, >1 = strongly right-skewed)
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
skew_before <- sapply(df_model[pca_vars], skew)

# 7.3 Apply log1p to the strongly skewed variables (skewness > 1).
#     log1p(x) = log(1 + x) is safe here because all values are zero or positive.
stopifnot(all(df_model[pca_vars] >= 0))
skewed_vars <- names(skew_before[skew_before > 1])
pca_data <- df_model %>%
  select(all_of(pca_vars)) %>%
  mutate(across(all_of(skewed_vars), log1p))

# 7.4 Compare skewness before and after the transformation
data.frame(variable = pca_vars,
           skew_before = round(skew_before, 2),
           skew_after = round(sapply(pca_data, skew), 2),
           log1p_applied = pca_vars %in% skewed_vars,
           row.names = NULL)

# 7.5 Standardise: mean 0 and standard deviation 1 for every column, so that no variable
#     dominates the PCA just because it is measured on a larger scale
pca_scaled <- scale(pca_data)
round(colMeans(pca_scaled), 6)             # all should be 0
round(apply(pca_scaled, 2, sd), 6)         # all should be 1
dim(pca_scaled)                            # 4,322 rows x 13 columns

# ------------------------- NOTES - SECTION 7 ---------------------------
# I included the 8 individual electricity-source shares alongside carbon intensity and
# size/usage metrics (13 numeric variables), and excluded aggregate shares such as
# `renewables_share_elec` and `fossil_share_elec` because they are sums of the source shares
# (redundant) and `renewables_share_elec` defines `renewable_group` (double-counting). Net
# imports and the *_change_* columns were excluded because they contain negative values.
# For strongly right-skewed variables (skewness > 1) I applied `log1p()` so extreme country
# values do not dominate the principal components, while safely handling zeros. This worked
# well for size and usage variables (e.g. energy_per_capita skewness 6.1 to -0.4) but wind,
# solar, nuclear and coal shares remain skewed because many countries have exactly 0%.
# Finally, I standardised all inputs (mean = 0, SD = 1) because PCA is variance-based and
# variables like `electricity_generation` have much larger numeric scales than percentage shares.


# ---------- SECTION 8: RUNNING THE PCA (Part iii) -------------

# 8.1 Run PCA on the standardised data (already centred and scaled in Section 7)
pca_fit <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)

# 8.2 Variance explained by each principal component
pca_var <- tibble(
  component = factor(paste0("PC", 1:ncol(pca_scaled)), levels = paste0("PC", 1:ncol(pca_scaled))),
  eigenvalue = pca_fit$sdev^2,                              # variance of each component
  variance_pct = eigenvalue / sum(eigenvalue) * 100,        # % of total variance
  cumulative_pct = cumsum(variance_pct)                     # running total
)
print(pca_var %>% mutate(across(where(is.numeric), ~ round(.x, 2))), n = 13)

# 8.3 How many components to keep? Three common rules:
sum(pca_var$eigenvalue > 1)                    # Kaiser rule: eigenvalue above 1
which(pca_var$cumulative_pct >= 80)[1]         # first component reaching 80% cumulative variance
# Scree "elbow": look for where the line in the scree plot flattens (next plot)

# 8.4 Scree plot (eigenvalues) with the Kaiser line at 1
p_scree <- ggplot(pca_var, aes(x = component, y = eigenvalue, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "PCA scree plot", subtitle = "Dashed line: Kaiser rule (eigenvalue = 1)",
       x = NULL, y = "Eigenvalue")
print(p_scree)
ggsave("figures/pca_scree.png", p_scree, width = 8, height = 5)

# 8.5 Cumulative variance explained, with an 80% reference line
p_cum <- ggplot(pca_var, aes(x = component, y = cumulative_pct, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "red") +
  labs(title = "PCA: cumulative variance explained", subtitle = "Dashed line: 80%",
       x = NULL, y = "Cumulative variance explained (%)")
print(p_cum)
ggsave("figures/pca_cumulative.png", p_cum, width = 8, height = 5)

# ------------------------- NOTES - SECTION 8 ---------------------------
# I decided to keep the first 4 principal components using the Kaiser rule (eigenvalue > 1,
# i.e. each component explains more variance than a single standardised variable), which is
# close to the elbow in the scree plot where the curve flattens. Together these 4 components
# explain 73.9% of the total variance, giving a compact summary of the 13 variables, although
# about 26% of the variance is left out; reaching the stricter 80% threshold would require
# 5 components (80.6%).


# ---------- SECTION 9: INTERPRETING THE PCA (Part iii) -------------

# 9.1 Loadings: how strongly each original variable contributes to each component.
#     Large positive or negative values mean the variable drives that component.
pca_loadings <- as.data.frame(pca_fit$rotation[, 1:4]) %>%
  rownames_to_column("variable")
pca_loadings %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# 9.2 Plot the loadings of the first four components (the ones we kept)
p_load <- pca_loadings %>%
  pivot_longer(starts_with("PC"), names_to = "component", values_to = "loading") %>%
  ggplot(aes(x = loading, y = fct_reorder(variable, loading), fill = loading > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ component, nrow = 1) +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(title = "PCA loadings of the first four components",
       x = "Loading", y = NULL)
print(p_load)
ggsave("figures/pca_loadings.png", p_load, width = 11, height = 5)

# 9.3 Component scores for every country-year, joined with the categorical columns.
#     (df_model rows are in the same order as pca_scaled, so the rows line up.)
pca_scores <- as_tibble(pca_fit$x[, 1:4]) %>%
  mutate(country = df_model$country, year = df_model$year,
         continent = df_model$continent, renewable_group = df_model$renewable_group)
pc_label <- function(i) paste0("PC", i, " (", round(pca_var$variance_pct[i], 1), "% of variance)")

# 9.4 PC1 vs PC2, coloured by renewable_group
p_pca_group <- ggplot(pca_scores, aes(x = PC1, y = PC2, colour = renewable_group)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA: PC1 vs PC2 coloured by renewable group",
       x = pc_label(1), y = pc_label(2), colour = "Renewable group")
print(p_pca_group)
ggsave("figures/pca_by_renewable_group.png", p_pca_group, width = 8, height = 6)

# 9.5 PC1 vs PC2, coloured by continent
p_pca_cont <- ggplot(pca_scores, aes(x = PC1, y = PC2, colour = continent)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA: PC1 vs PC2 coloured by continent",
       x = pc_label(1), y = pc_label(2), colour = "Continent")
print(p_pca_cont)
ggsave("figures/pca_by_continent.png", p_pca_cont, width = 8, height = 6)

# 9.6 Average scores per group (numbers to quote in the notes)
pca_scores %>%
  group_by(renewable_group) %>%
  summarise(across(c(PC1, PC2), ~ round(mean(.x), 2)), n = n())
pca_scores %>%
  group_by(continent) %>%
  summarise(across(c(PC1, PC2), ~ round(mean(.x), 2)), n = n())

# ------------------------- NOTES - SECTION 9 ---------------------------
# PC1 mainly captures the scale and intensity of electricity systems, with strong positive
# loadings on electricity_generation, greenhouse_gas_emissions, per_capita_electricity and
# energy_per_capita (plus gas, coal and nuclear shares) and a negative loading on oil share.
# PC2 contrasts hydro-based grids (hydro_share_elec) with carbon-intensive, oil-heavy grids
# (carbon_intensity_elec, oil_share_elec). renewable_group separates along PC2, with mean
# scores rising from low-renewable (-1.26) to medium (0.42) to high-renewable grids (2.18);
# this is partly by construction, since the group is built from renewable share and hydro
# drives PC2, so it confirms the Q2 finding. Continents separate mainly along PC1 instead:
# Europe has the highest mean score (2.54) and Africa (-1.49) and Oceania (-1.27) the lowest,
# suggesting regional differences in system size and energy use rather than in the clean-vs-
# carbon-intensive mix, while mean PC2 differs little between continents (-0.76 to 0.60).
