# Libraries
library(pacman)
p_load(tidyverse, countrycode)

# Load data
agr_pat <- read.csv("DR/patents_data/csv/agri_patents.csv")
dat_pat <- read.csv("DR/patents_data/csv/data_patents.csv")
min_pat <- read.csv("DR/patents_data/csv/mine_patents.csv")
ene_pat <- read.csv("DR/patents_data/csv/energy_patents.csv")
all_pat <- read.csv("DR/patents_data/csv/all_patents.csv")
comtrade <- read.csv("DR/comtrade_sum.csv")

# Merge patents data
pats <- agr_pat %>% # Merge all patents of interest using year and country as keys, keep each patent_count with the name of its df of source
  rename(agr_pat = patent_count) %>%
  full_join(dat_pat %>% rename(dat_pat = patent_count), by = c("pub_year", "country_name")) %>%
  full_join(min_pat %>% rename(min_pat = patent_count), by = c("pub_year", "country_name")) %>%
  full_join(ene_pat %>% rename(ene_pat = patent_count), by = c("pub_year", "country_name")) %>%
  mutate(all_pat = rowSums(select(., agr_pat, dat_pat, min_pat, ene_pat), na.rm = TRUE)) %>% # Create a column with the sum of all patents, ignoring non-valid values
  select(pub_year, country_name, agr_pat, dat_pat, min_pat, ene_pat, all_pat) %>% # Select only the columns of interest
  rename(country = country_name, year = pub_year) # Rename columns to match ITS_dataset


# Harmonize country codes (harmonized, hence the "h" in the dataset's name)
comtrade_h <- comtrade %>% mutate(country = countrycode(sourcevar = country, origin = "country.name", destination = "country.name"))
pats_h <- pats %>% mutate(country = countrycode(sourcevar = country, origin = "country.name", destination = "country.name"))


# Compare the number of unique countries in each dataset to check for missing data
# Number of unique countries before harmonization
unique_countries_before <- list(
  comtrade = comtrade %>% pull(country) %>% unique() %>% length(),
  pats = pats %>% pull(country) %>% unique() %>% length()
)

# Number of unique countries after harmonization
unique_countries_after <- list(
  comtrade_h = comtrade_h %>% pull(country) %>% unique() %>% length(),
  pats_h = pats_h %>% pull(country) %>% unique() %>% length()
)

# Values missing in the data after harmonization
missing_countries <- list(
  comtrade = setdiff(comtrade %>% pull(country) %>% unique(), comtrade_h %>% pull(country) %>% unique()),
  pats = setdiff(pats %>% pull(country) %>% unique(), pats_h %>% pull(country) %>% unique())
)

list(
  unique_countries_before = unique_countries_before,
  unique_countries_after = unique_countries_after,
  missing_countries = missing_countries
)


# Merge all datasets
ITS <- pats_h %>% # First version of the dataset with data harmonized programmatically
  full_join(comtrade_h, by = c("year", "country")) %>%
  filter(!is.na(year) & country != "") %>% # Filter out rows with missing values in year or country
  filter(year >= 1990 & year <= 2021 & !is.na(country)) # Filter out years outside the range of interest and rows with missing country values

# Check unique countries
unique_countries_ITS <- ITS %>% pull(country) %>% unique() %>% length()


# Save the dataset

# write.csv(ITS, "DP/ITS_v1.csv", row.names = FALSE)
# print("ITS saved as ITS_v1.csv")

write.csv(ITS, "DP/ITS_v2.csv", row.names = FALSE)
print("ITS saved as ITS_v2.csv")
