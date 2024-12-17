# Libraries
library(pacman)
p_load(tidyverse, mice, countrycode, ggplot2, plotly, brolgar, corrplot)

# Load data
ITS <- read.csv("data/ITS_v1.csv") %>%
 mutate(pats = agr_pat + min_pat + dat_pat)


# Aux functions
normalize <- function(x) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Data exploration with brolgar package - definition of plots and countries of interest
G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", "Saudi Arabia", "South Africa", "South Korea", "Turkey", "United Kingdom", "United States")
subsample_LAS <- c("Brazil", "Chile", "Argentina", "Bolivia", "Paraguay", "Colombia", "Peru", "Mexico")

# Full correlation matrix of numeric variables
ITS_corr_full <- ITS %>%
  select(-c(year, country, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")
corrplot_ITS <- corrplot(ITS_corr_full, method = "color", order = "AOE", type = "lower", addCoef.col = "black")

# Missing data analysis
missdata_ITS <- md.pattern(ITS, plot = TRUE, rotate.names = TRUE)

# Impute missing data
imputed <- mice(ITS %>%
                      select(-main_tech_capital_source),
                    method = "pmm", m = 5, maxit = 50, seed = 123)

# Complete the imputation
ITS_imputed <- complete(imputed)

# Optionally, reattach the imputed data to the original dataset (if needed)
ITS_imp <- ITS %>%
    select(year, country, IDIT, Net_GFCF, pats, ECI) %>%
    rename(IDIT_og = IDIT, Net_GFCF_og = Net_GFCF, pats_og = pats, ECI_og = ECI) %>%
    left_join(ITS_imputed %>% select(year, country, IDIT, Net_GFCF, pats, ECI), by = c("year", "country")) %>%
    rename(IDIT_imp = IDIT, Net_GFCF_imp = Net_GFCF, pats_imp = pats, ECI_imp = ECI)

# Density plots to compare if original and imputed data follow the same distribution
GFCF_dens <- ggplot() +
                geom_density(data = ITS_imp, aes(x = Net_GFCF_og), fill = "blue", alpha = 0.5) +
                geom_density(data = ITS_imp, aes(x = Net_GFCF_imp), fill = "red", alpha = 0.5) +
                labs(title = "Density Plot: Net_GFCF (Original vs Imputed)",
                     subtitle = "Similar distribution (overlaping curves) is suggested to be interpreted as good imputation",
                    x = "Net_GFCF", fill = "Density",
                    caption = "Source: Own elaboration") +
                theme_minimal()

IDIT_dens <- ggplot() +
                geom_density(data = ITS_imp, aes(x = IDIT_og), fill = "blue", alpha = 0.5) +
                geom_density(data = ITS_imp, aes(x = IDIT_imp), fill = "red", alpha = 0.5) +
                labs(title = "Density Plot: IDIT (Original vs Imputed)",
                     subtitle = "Similar distribution (overlaping curves) is suggested to be interpreted as good imputation",
                    x = "TIDR value", fill = "Density",
                    caption = "Source: Own elaboration.") +
                theme_minimal()

PATS_dens <- ggplot() +
                geom_density(data = ITS_imp, aes(x = pats_og), fill = "blue", alpha = 0.5) +
                geom_density(data = ITS_imp, aes(x = pats_imp), fill = "red", alpha = 0.5) +
                labs(title = "Density Plot: pats (Original vs Imputed)",
                     subtitle = "Similar distribution (overlaping curves) is suggested to be interpreted as good imputation",
                    x = "Count of patents", fill = "Density",
                    caption = "Source: Own elaboration.") +
                theme_minimal()

ECI_dens <- ggplot() +
                geom_density(data = ITS_imp, aes(x = ECI_og), fill = "blue", alpha = 0.5) +
                geom_density(data = ITS_imp, aes(x = ECI_imp), fill = "red", alpha = 0.5) +
                labs(title = "Density Plot: ECI (Original vs Imputed)",
                     subtitle = "Similar distribution (overlaping curves) is suggested to be interpreted as good imputation",
                    x = "ECI value", fill = "Density",
                    caption = "Source: Own elaboration") +
                theme_minimal()

# Normalize imputed data
ITS_imp$IDIT_norm <- normalize(ITS_imp$IDIT_imp)
ITS_imp$Net_GFCF_norm <- normalize(ITS_imp$Net_GFCF_imp)
ITS_imp$pats_norm <- normalize(ITS_imp$pats_imp)
ITS_imp$ECI_norm <- normalize(ITS_imp$ECI_imp)

# Indexation in imputed dataset
ITS_imp <- ITS_imp %>%
  group_by(year, country) %>%
  mutate(
    pats_inv = 1 - pats_norm, # Invert and normalize `pats`
    ITS_index = (IDIT_norm + Net_GFCF_norm + pats_inv) / 3, # Calculate the index
    continent = countrycode(country, "country.name", "continent")) 

# Plot ITS_index per continent
ITS_index_full <- ggplot(ITS_imp, 
                    aes(x = year, y = ITS_index, group = country, color = continent)) +
                        scale_x_continuous(breaks = seq(min(ITS_imp$year), max(ITS_imp$year), by = 5)) +
                        scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
                        geom_line(data = subset(ITS_imp, !country %in% G20_countries), alpha = 0.3) +
                        geom_line(data = subset(ITS_imp, country %in% G20_countries), alpha = 0.7, size = 1) +
                        geom_text(data = subset(ITS_imp, country %in% c("United States", "China") & year == 2018, color = "black"), 
                            aes(label = country), 
                            vjust = -1, 
                            size = 3) +
                        labs(title = "ITS Index by Continent",
                                x = "Year", y = "ITS Index", color = "Continent",
                                caption = "Source: Calculation based on selected and imputed data | G20 economies highlighted") +
                        theme_minimal() +
                        theme(legend.position = "right")

ITS_index_faceted <- ggplot(ITS_imp,
                        aes(x = year, y = ITS_index, group = country, color = continent)) +
                        facet_wrap(~continent) +
                        scale_x_continuous(breaks = seq(min(ITS_imp$year), max(ITS_imp$year), by = 5)) +
                        scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
                        geom_line(data = subset(ITS_imp, !country %in% G20_countries), alpha = 0.3) +
                        geom_line(data = subset(ITS_imp, country %in% G20_countries), alpha = 0.7, size = 1) +
                        geom_text(data = subset(ITS_imp, country %in% c("United States", "China") & year == 2018, color = "black"), 
                            aes(label = country), 
                            vjust = -1, 
                            size = 3) +
                        labs(title = "ITS Index by Continent",
                                x = "Year", y = "ITS Index", color = "Continent",
                                caption = "Source: Calculation based on selected and imputed data | G20 economies highlighted") +
                        theme_minimal() +
                        theme(legend.position = "right")

ITS_LAS <- ggplot(ITS_imp %>% filter(country %in% subsample_LAS),
                    aes(x = year, y = ITS_index, group = country, color = country)) +
                        scale_x_continuous(breaks = seq(min(ITS_imp$year), max(ITS_imp$year), by = 5)) +
                        scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
                        geom_line(alpha = 0.7, size = 1) +
                        geom_text(data = subset(ITS_imp, country %in% subsample_LAS & year == 2018, color = "black"), 
                            aes(label = country), 
                            vjust = -1, 
                            size = 3) +
                        labs(title = "ITS Index by Country (Latin America)",
                                x = "Year", y = "ITS Index", color = "Country",
                                caption = "Source: Calculation based on selected and imputed data") +
                        theme_minimal() +
                        theme(legend.position = "right")

ITS_ECI_LAS <- ggplot(ITS_imp %>% filter(country %in% subsample_LAS), # Plot double Y axis ECI_norm vs ITS_index
          aes(x = year)) +
            facet_wrap(~country) +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1)) + # Both values are normalized
            geom_line(aes(y = ECI_norm, color = "ECI"), alpha = 0.7, size = 1, color = "blue") +
            geom_line(aes(y = ITS_index, color = "ITS Index"), alpha = 0.7, size = 1, color = "red") +
            labs(title = "ITS Index and ECI by Country (Latin America)",
                x = "Year", y = "Value", color = "Indicator",
                caption = "Source: Calculation based on selected and imputed data") +
            theme_minimal() +
            theme(legend.position = "right")

# Save plots
ggsave("plots/index-v1/ITS_index_full.png", ITS_index_full, width = 12, height = 8, dpi = 300)
ggsave("plots/index-v1/ITS_index_faceted.png", ITS_index_faceted, width = 12, height = 8, dpi = 300)
ggsave("plots/index-v1/ITS_LAS.png", ITS_LAS, width = 12, height = 8, dpi = 300)
ggsave("plots/index-v1/ITS_ECI_LAS.png", ITS_ECI_LAS, width = 12, height = 8, dpi = 300)
ggsave("plots/index-v1/GFCF_dens.png", GFCF_dens, width = 8, height = 6, dpi = 300)
ggsave("plots/index-v1/IDIT_dens.png", IDIT_dens, width = 8, height = 6, dpi = 300)
ggsave("plots/index-v1/PATS_dens.png", PATS_dens, width = 8, height = 6, dpi = 300)
ggsave("plots/index-v1/ECI_dens.png", ECI_dens, width = 8, height = 6, dpi = 300)

# Save the correlation plot - General then per continent
png("plots/index-v1/corplots/corplot_ITS_General.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS <- corrplot(ITS_corr_full, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device

ITS_corr_Africa <- ITS %>% # Africa plot
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(continent == "Africa") %>%
  select(-c(year, country, continent, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")

png("plots/index-v1/corplots/corplot_ITS_Africa.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS_Africa <- corrplot(ITS_corr_Africa, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device

ITS_corr_Americas <- ITS %>% # Americas plot
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(continent == "Americas") %>%
  select(-c(year, country, continent, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")

png("plots/index-v1/corplots/corplot_ITS_Americas.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS_Americas <- corrplot(ITS_corr_Americas, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device


ITS_corr_Asia <- ITS %>% # Asia plot
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(continent == "Asia") %>%
  select(-c(year, country, continent, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")

png("plots/index-v1/corplots/corplot_ITS_Asia.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS_Asia <- corrplot(ITS_corr_Asia, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device

ITS_corr_Europe <- ITS %>% # Europe plot
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(continent == "Europe") %>%
  select(-c(year, country, continent, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")

png("plots/index-v1/corplots/corplot_ITS_Europe.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS_Europe <- corrplot(ITS_corr_Europe, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device

ITS_corr_Oceania <- ITS %>% # Oceania plot
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(continent == "Oceania") %>%
  select(-c(year, country, continent, main_tech_capital_source, min_pat, agr_pat, dat_pat, ECI_rank, IDIT_USA, IDIT_China)) %>%
  cor(use = "pairwise.complete.obs")

png("plots/index-v1/corplots/corplot_ITS_Oceania.png", width = 800, height = 600)  # Adjust dimensions if needed
corplot_ITS_Oceania <- corrplot(ITS_corr_Oceania, method = "color", order = "AOE", type = "lower", addCoef.col = "black")
dev.off()  # Close the PNG device

# Save the missing data pattern plot
png("missdata_ITS.png", width = 800, height = 600)  # Adjust dimensions if needed
missdata_ITS <- md.pattern(ITS %>% select(-c(min_pat, agr_pat, dat_pat)), plot = TRUE, rotate.names = TRUE)
dev.off()  # Close the PNG device

# Save the imputed dataset
write.csv(ITS_imp, "data/ITS_imp-v1.csv", row.names = FALSE)