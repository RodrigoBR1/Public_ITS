# Libraries
library(pacman)
p_load(tidyverse, ggplot2, countrycode, haven)

# Load data
agr_pat <- read.csv("data/agri_patents.csv")
comtrade <- read.csv("data/comtrade_sum.csv")
dat_pat <- read.csv("data/data_patents.csv")
min_pat <- read.csv("data/mine_patents.csv")
fdi <- read.csv("data/world_net_GFCF.csv")
ECI <- read_dta("data/ECI_rankings.dta")

# Aux function


# Data cleaning - Year and Country codes harmonization through analysis of which country code format was used in each dataset
ITS_dataset <- comtrade %>% # Base dataset will be COMTRADE
  rename(year = period, country = reporterDesc, main_tech_capital_source = most_frequent_imp_partner) %>%
  select(year, country, IDIT, IDIT_China, IDIT_USA, main_tech_capital_source) # Main keys will be year and country

pats <- agr_pat %>% # Merge all patents of interest using year and country as keys, keep each patent_count with the name of its df of source
  rename(agr_pat = patent_count) %>%
  full_join(dat_pat %>% rename(dat_pat = patent_count), by = c("pub_year", "country_name")) %>%
  full_join(min_pat %>% rename(min_pat = patent_count), by = c("pub_year", "country_name")) %>%
  select(pub_year, country_name, agr_pat, dat_pat, min_pat) %>% # Select only the columns of interest
  rename(country = country_name, year = pub_year) # Rename columns to match ITS_dataset

fdi <- fdi %>% # Harmonize data
  dplyr::filter(Reporter_class == "Country") %>% # Filter only countries
  rename(country = Reporter, year = Year, Net_GFCF = Net_Gross_Fixed_Capital_Formation) %>%
  select(year, country, Net_GFCF) # Select only the columns of interest

ECI <- ECI %>% # Harmonize data
  rename(country = country_id, ECI = hs_eci, ECI_rank = hs_eci_rank) %>% # ECI = Economic Complexity Index, developed by Growth Lab in Harvard
  select(year, country, ECI, ECI_rank) # Select only the columns of interest

# Harmonize country codes (harmonized, hence the "h" in the dataset's name)
ITS_dataset_h <- ITS_dataset %>% mutate(country = countrycode(sourcevar = country, origin = "country.name", destination = "country.name"))
pats_h <- pats %>% mutate(country = countrycode(sourcevar = country, origin = "country.name", destination = "country.name"))
fdi_h <- fdi %>% mutate(country = countrycode(sourcevar = country, origin = "country.name", destination = "country.name"))
ECI_h <- ECI %>% mutate(country = countrycode(sourcevar = country, origin = "un", destination = "country.name"))

# Compare the number of unique countries in each dataset to check for missing data
# Number of unique countries before harmonization
unique_countries_before <- list(
  ITS_dataset = ITS_dataset %>% pull(country) %>% unique() %>% length(),
  pats = pats %>% pull(country) %>% unique() %>% length(),
  fdi = fdi %>% pull(country) %>% unique() %>% length(),
  ECI = ECI %>% pull(country) %>% unique() %>% length()
)

# Number of unique countries after harmonization
unique_countries_after <- list(
  ITS_dataset_h = ITS_dataset_h %>% pull(country) %>% unique() %>% length(),
  pats_h = pats_h %>% pull(country) %>% unique() %>% length(),
  fdi_h = fdi_h %>% pull(country) %>% unique() %>% length(),
  ECI_h = ECI_h %>% pull(country) %>% unique() %>% length()
)

# Values missing in the data after harmonization
missing_countries <- list(
  ITS_dataset = setdiff(ITS_dataset %>% pull(country) %>% unique(), ITS_dataset_h %>% pull(country) %>% unique()),
  pats = setdiff(pats %>% pull(country) %>% unique(), pats_h %>% pull(country) %>% unique()),
  fdi = setdiff(fdi %>% pull(country) %>% unique(), fdi_h %>% pull(country) %>% unique()),
  ECI = setdiff(ECI %>% pull(country) %>% unique(), ECI_h %>% pull(country) %>% unique())
)

list(
  unique_countries_before = unique_countries_before,
  unique_countries_after = unique_countries_after,
  missing_countries = missing_countries
)

# Merge all datasets
ITS_dfh_a1 <- ITS_dataset_h %>% # First version of the dataset with data harmonized programmatically
  left_join(pats_h, by = c("year", "country")) %>%
  left_join(fdi_h, by = c("year", "country")) %>%
  left_join(ECI_h, by = c("year", "country")) %>%
  dplyr::filter(!is.na(year) & country != "") # Filter out rows with missing values in year or country

# Save the dataset
write.csv(ITS_dfh_a1, "data/ITS_v1.csv", row.names = FALSE)
print("ITS_dfh_a1 saved as ITS_v1.csv")
