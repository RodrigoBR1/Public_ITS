# Library load

library(pacman)
p_load(tidyverse, ggplot2, plotly, countrycode)

## Data load and aux functions
patents <- read_csv("DR/patents_data/csv/raw-patents_v1.csv")
patents2 <- read_csv("DR/patents_data/csv/raw-patents_v2.csv")

# Using countrycode to convert country 2-digit code to full name in english
patents <- patents %>%
  mutate(country_name = countrycode(country_code, "iso2c", "country.name"))

patents2 <- patents2 %>%
    mutate(country_name = countrycode(country_code, "iso2c", "country.name"))

# Agriculture patents
agri_patents <- patents %>%
    filter(cpc_broad_class == "Agriculture related") %>%
    group_by(pub_year) %>%
    mutate(country_yearly_relative_position = ntile(patent_count, 4)) %>%
    mutate(country_yearly_relative_position = case_when(
        country_yearly_relative_position == 1 ~ "Low tech development",
        country_yearly_relative_position == 2 ~ "Low-med tech development",
        country_yearly_relative_position == 3 ~ "High-med tech development",
        country_yearly_relative_position == 4 ~ "High tech development"
    )) %>%
    ungroup() %>%
    select(pub_year, country_name, patent_count, country_yearly_relative_position)

# Mining patents
mine_patents <- patents %>%
    filter(cpc_broad_class == "Mining related") %>%
    group_by(pub_year) %>%
    mutate(country_yearly_relative_position = ntile(patent_count, 4)) %>%
    mutate(country_yearly_relative_position = case_when(
        country_yearly_relative_position == 1 ~ "Low tech development",
        country_yearly_relative_position == 2 ~ "Low-med tech development",
        country_yearly_relative_position == 3 ~ "High-med tech development",
        country_yearly_relative_position == 4 ~ "High tech development"
    )) %>%
    ungroup() %>%
    select(pub_year, country_name, patent_count, country_yearly_relative_position)

# Data process patents
data_patents <- patents %>%
    filter(cpc_broad_class == "Data process related") %>%
    group_by(pub_year) %>%
    mutate(country_yearly_relative_position = ntile(patent_count, 4)) %>%
    mutate(country_yearly_relative_position = case_when(
        country_yearly_relative_position == 1 ~ "Low tech development",
        country_yearly_relative_position == 2 ~ "Low-med tech development",
        country_yearly_relative_position == 3 ~ "High-med tech development",
        country_yearly_relative_position == 4 ~ "High tech development"
    )) %>%
    ungroup() %>%
    select(pub_year, country_name, patent_count, country_yearly_relative_position)

# Energy  patents
energy_patents <- patents2 %>%
    filter(cpc_broad_class == "Energy related") %>%
    group_by(pub_year) %>%
    mutate(country_yearly_relative_position = ntile(patent_count, 4)) %>%
    mutate(country_yearly_relative_position = case_when(
        country_yearly_relative_position == 1 ~ "Low tech development",
        country_yearly_relative_position == 2 ~ "Low-med tech development",
        country_yearly_relative_position == 3 ~ "High-med tech development",
        country_yearly_relative_position == 4 ~ "High tech development"
    )) %>%
    ungroup() %>%
    select(pub_year, country_name, patent_count, country_yearly_relative_position)

# All patents
all_patents <- patents %>%
    bind_rows(patents2) %>%
    group_by(pub_year) %>%
    mutate(country_yearly_relative_position = ntile(patent_count, 4)) %>%
    mutate(country_yearly_relative_position = case_when(
        country_yearly_relative_position == 1 ~ "Low tech development",
        country_yearly_relative_position == 2 ~ "Low-med tech development",
        country_yearly_relative_position == 3 ~ "High-med tech development",
        country_yearly_relative_position == 4 ~ "High tech development"
    )) %>%
    ungroup() %>%
    select(pub_year, country_name, patent_count, country_yearly_relative_position)
    
# Save data
write.csv(all_patents, "DR/patents_data/csv/all_patents.csv", row.names = FALSE)
write.csv(agri_patents, "DR/patents_data/csv/agri_patents.csv", row.names = FALSE)
write.csv(mine_patents, "DR/patents_data/csv/mine_patents.csv", row.names = FALSE)
write.csv(data_patents, "DR/patents_data/csv/data_patents.csv", row.names = FALSE)
write.csv(energy_patents, "DR/patents_data/csv/energy_patents.csv", row.names = FALSE)
