# Libraries
library(pacman)
p_load(tidyverse, maps, geosphere, ggplot2, plotly, brolgar, htmlwidgets, countrycode, forecast)

# Load data and clean
ITS <- read.csv("data/ITS_v1.csv") %>%
    mutate(pats = agr_pat + min_pat + dat_pat)
ITS$continent <- countrycode(ITS$country, "country.name", "continent")

# Make sure every country & year is unique
ITS <- ITS %>% 
    distinct(country, year, .keep_all = TRUE)

# Data exploration with brolgar package - definition of plots and countries of interest
G20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", "Saudi Arabia", "South Africa", "South Korea", "Turkey", "United Kingdom", "United States")
subsample_LAS <- c("Brazil", "Chile", "Argentina", "Bolivia", "Paraguay", "Colombia", "Peru", "Mexico", "Venezuela", "Nicaragua", "Uruguay")

# Subsample LAS in ITS dataset
ITS_LAS <- ITS %>% 
    filter(country %in% subsample_LAS) %>% 
    mutate(pats = agr_pat + min_pat + dat_pat)

# Define unique colors and simple shapes for each country
colors_LAS <- c("Brazil" = "blue", "Chile" = "red", "Argentina" = "green", "Bolivia" = "purple", "Paraguay" = "orange", "Colombia" = "brown", "Peru" = "pink", "Mexico" = "brown", "Venezuela" = "cyan", "Nicaragua" = "magenta", "Uruguay" = "darkgreen")
shapes_LAS <- c("Brazil" = 16, "Chile" = 17, "Argentina" = 18, "Bolivia" = 15, "Paraguay" = 3, "Colombia" = 4, "Peru" = 16, "Mexico" = 17, "Venezuela" = 18, "Nicaragua" = 15, "Uruguay" = 3)


# Technologies Import Dependency Ratio (TIDR), in general and relative to China and USA, per country and year in subsample of Latin American countries

TIDR_plot_LAS <- ggplot(ITS_LAS, aes(x = year, y = IDIT, group = country, color = country, shape = country)) +
    geom_line() +
    geom_point(size = 3) +
    scale_color_manual(values = colors_LAS) +
    scale_shape_manual(values = shapes_LAS) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
    geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
    geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
    geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
    labs(
        title = "Technologies Import Dependency Ratio (TIDR) in Latin American economies",
        x = "Year",
        y = "TIDR",
        caption = "Source: UNCTADSTAT | Subsample of Latin American economies",
        color = "Country",
        shape = "Country",
        subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)"
    ) +
    theme_minimal()

# Save as ggplotly in "plots/plots-v2" folder
# Convert ggplot to ggplotly for interactivity
TIDR_plot_LAS_interactive <- ggplotly(TIDR_plot_LAS)

# Save interactive plot as HTML
htmlwidgets::saveWidget(TIDR_plot_LAS_interactive, "plots/plots-v2/TIDR_plot_LAS_interactive.html")

    # Technologies Import Dependency Ratio (TIDR) relative to China in Latin American subsample
    TIDR_China_plot_LAS <- ggplot(ITS_LAS, aes(x = year, y = IDIT_China, group = country, color = country, shape = country)) +
        geom_line() +
        geom_point(size = 3) +
        scale_color_manual(values = colors_LAS) +
        scale_shape_manual(values = shapes_LAS) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
        geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
        geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
        geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
        labs(
            title = "Technologies Import Dependency Ratio (TIDR) relative to China in Latin American economies",
            x = "Year",
            y = "TIDR relative to China",
            caption = "Source: UNCTADSTAT | Subsample of Latin American economies",
            color = "Country",
            shape = "Country",
            subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)"
        ) +
        theme_minimal()

    # Save as ggplotly in "plots/plots-v2" folder
    # Convert ggplot to ggplotly for interactivity
    TIDR_China_plot_LAS_interactive <- ggplotly(TIDR_China_plot_LAS)

    # Save interactive plot as HTML
    htmlwidgets::saveWidget(TIDR_China_plot_LAS_interactive, "plots/plots-v2/TIDR_China_plot_LAS_interactive.html")

    # Technologies Import Dependency Ratio (TIDR) relative to USA in Latin American subsample
    TIDR_USA_plot_LAS <- ggplot(ITS_LAS, aes(x = year, y = IDIT_USA, group = country, color = country, shape = country)) +
        geom_line() +
        geom_point(size = 3) +
        scale_color_manual(values = colors_LAS) +
        scale_shape_manual(values = shapes_LAS) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
        geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
        geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
        geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
        labs(
            title = "Technologies Import Dependency Ratio (TIDR) relative to USA in Latin American economies",
            x = "Year",
            y = "TIDR relative to USA",
            caption = "Source: UNCTADSTAT | Subsample of Latin American economies",
            color = "Country",
            shape = "Country",
            subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)"
        ) +
        theme_minimal()

    # Save as ggplotly in "plots/plots-v2" folder
    # Convert ggplot to ggplotly for interactivity
    TIDR_USA_plot_LAS_interactive <- ggplotly(TIDR_USA_plot_LAS)

    # Save interactive plot as HTML
    htmlwidgets::saveWidget(TIDR_USA_plot_LAS_interactive, "plots/plots-v2/TIDR_USA_plot_LAS_interactive.html")

# Net GFCF in Latin American subsample against ECI
GFCF_plot_LAS <- ggplot(ITS_LAS, 
                aes(x = year, y = Net_GFCF, group = country, color = country)) +
                geom_line() +
                labs(title = "Net Gross Fixed Capital Formation (GFCF) in Latin American economies",
                    x = "Year",
                    y = "Net GFCF",
                    caption = "Source: UNCTADSTAT | Subsample of Latin American economies") +
                theme_minimal()

# Patents in Latin American subsample against ECI
pats_plot_LAS <- ggplot(ITS_LAS, 
                aes(x = year, y = pats, group = country, color = country)) +
                geom_line() +
                labs(title = "Patents in Latin American economies",
                    x = "Year",
                    y = "Patents",
                    caption = "Source: UNCTADSTAT | Subsample of Latin American economies") +
                theme_minimal()

ECI_plot_LAS <- ggplot(ITS_LAS, 
                aes(x = year, y = ECI, group = country, color = country)) +
                geom_line() +
                labs(title = "Economic Complexity Index (ECI) in Latin American economies",
                    x = "Year",
                    y = "ECI",
                    caption = "Source: UNCTADSTAT | Subsample of Latin American economies") +
                theme_minimal()

# Save plots and table
ggsave("plots/plots-v2/TIDR_plot_LAS.png", TIDR_plot_LAS, width = 12, height = 8)
ggsave("plots/plots-v2/GFCF_plot_LAS.png", GFCF_plot_LAS, width = 12, height = 8)
ggsave("plots/plots-v2/pats_plot_LAS.png", pats_plot_LAS, width = 12, height = 8)
ggsave("plots/plots-v2/ECI_plot_LAS.png", ECI_plot_LAS, width = 12, height = 8)

# Identify top 10 countries with lowest mean IDIT throughout the years
top_10_lowest_IDIT <- ITS %>% 
    group_by(country) %>%
    summarize(mean_IDIT = mean(IDIT)) %>%
    arrange(mean_IDIT) %>%
    head(10)

# Identify top 10 countries with highest mean IDIT throughout the years
top_10_highest_IDIT <- ITS %>% 
    group_by(country) %>%
    summarize(mean_IDIT = mean(IDIT)) %>%
    arrange(desc(mean_IDIT)) %>%
    head(10)

# Vectors for top10 countries IDIT
color_IDIT_top10 = c("China" = "red", "Japan" = "purple", "United States" = "blue", "Italy" = "green", "Germany" = "#b6b600", "United Kingdom" = "darkblue", "France" = "#e47b8d", "Belgium" = "black", "Netherlands" = "orange", "Sweden" = "turquoise") 
shape_IDIT_top10 = as.factor(c("China" = 15, "Japan" = 15, "United States" = 17, "Italy" = 17, "Germany" = 17, "United Kingdom" = 16, "France" = 16, "Belgium" = 16, "Netherlands" = 16, "Sweden" = 16))


# Plot top 10 countries, identified through their mean IDIT value (that is, their "mean position" in the international logisitics network of technologies)
IDIT_top10 <- ITS %>%
    mutate(top10_cat =
    as.factor(case_when(
        country == "China" ~ 15,
        country == "Japan" ~ 15,
        country == "United States" ~ 17,
        country == "Italy" ~ 17,
        country == "Germany" ~ 17,
        country == "United Kingdom" ~ 16,
        country == "France" ~ 16,
        country == "Belgium" ~ 16,
        country == "Netherlands" ~ 16,
        country == "Sweden" ~ 16,
        TRUE ~ NA_real_
    ))) %>%
    filter(country %in% top_10_lowest_IDIT$country) %>%
    ggplot(aes(x = year, y = IDIT, group = country, color = country, shape = top10_cat)) +
    scale_color_manual(values = color_IDIT_top10) + 
    geom_point(size = 2) +  
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
    geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
    geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
    geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
    labs(title = "Top 10 countries with lowest mean IDIT /TIDR",
        subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)",
        x = "Year",
        y = "IDIT / TIDR",
        caption = "Source: UNCTADSTAT | Top 10 countries with the lowest mean IDIT value (that is, least subordinate through time)") +
    guides(color = guide_legend(override.aes = list(shape = shape_IDIT_top10))) +

# Identify top 10 countries with highest mean patents ownership throughout the years
top_10_highest_pats <- ITS %>% 
    group_by(country) %>%
    summarize(mean_pats = mean(pats)) %>%
    arrange(desc(mean_pats)) %>%
    head(10)

# Vectors for top10 countries
color_top10_pats = c("United States" = "blue", "China" = "red", "South Korea" = "darkblue", "Japan" = "purple", "Canada" = "brown", "Australia" = "green", "Russia" = "black", "Brazil" = "darkgreen", "Austria" = "orange", "Spain" = "darkred")
top10_pats_cat <- as.factor(c("United States" = 15, "China" = 15, "South Korea" = 17, "Japan" = 17, "Canada" = 17, "Australia" = 16, "Russia" = 16, "Brazil" = 16, "Austria" = 16, "Spain" = 16))

 # Plot top 10 countries, identified through their mean patents value
pats_top10 <- ITS %>%
  mutate(top10_cat = as.factor(case_when(
    country == "United States" ~ 15,
    country == "China" ~ 15,
    country == "South Korea" ~ 17,
    country == "Japan" ~ 17,
    country == "Canada" ~ 17,
    country == "Australia" ~ 16,
    country == "Brazil" ~ 16,
    country == "Austria" ~ 16,
    country == "Spain" ~ 16,
    TRUE ~ NA_real_
  ))) %>%
  filter(country %in% top_10_highest_pats$country) %>%
  ggplot(aes(x = year, y = pats, group = country, color = country, shape = top10_cat)) +
  scale_color_manual(values = color_top10_pats) +
  scale_shape() +
  geom_point(size = 3) +  
  geom_line() +
    labs(
        title = "Top 10 countries with highest mean patents",
        x = "Year",
        y = "Patents",
        caption = "Source: UNCTADSTAT | Top 10 countries with the highest patents ownership through time (mean patent count per year)"
    ) +
    guides(color = guide_legend(override.aes = list(shape = top10_pats_cat)))


# Save plots 06/12/2024
ggsave("plots/plots-v2/IDIT_top10_exporters.png", IDIT_top10, width = 12, height = 8)
ggsave("plots/plots-v2/pats_top10.png", pats_top10, width = 12, height = 8)

##### LAST MINUTE IDEA: FUTURE OF TECHNOLOGICAL DEVELOPMENT #####

# Load data
world_trade_by_sector <- read.csv("data/last_minute_idea/US_IntraTrade.csv/US_IntraTrade.csv")
wtbs <- world_trade_by_sector # Rename for simplicity

# Create tibble for data availability of wtbs
goods_by_tech <- wtbs %>%
select(Year, Economy.Label, Partner.Label, Product.Label, Flow.Label, US..at.current.prices.in.millions, Percentage.by.destination) %>%
rename(
    year = Year,
    region_reporter = Economy.Label,
    region_partner = Partner.Label,
    product = Product.Label,
    flow = Flow.Label,
    value_mmUSD = US..at.current.prices.in.millions,
    percentage_by_dest = Percentage.by.destination
    ) %>%
filter(product %in% unique(product)[355:375])

# Create dataframe to calculate IDIT / TIDR for each region
IDIT_future <- goods_by_tech %>%
    group_by(year, region_reporter, region_partner) %>%
    mutate(
        TIDR_USD = sum(value_mmUSD[flow == "Imports"]) - sum(value_mmUSD[flow == "Exports"]),
        TIDR_pct = sum(percentage_by_dest[flow == "Imports"]) - sum(percentage_by_dest[flow == "Exports"]),
        ) %>%
    filter(region_reporter %in% unique(goods_by_tech$region_reporter)[2:31]) %>%
    select(year, region_reporter, region_partner, TIDR_USD, TIDR_pct)

# Plot IDIT / TIDR for each region
IDIT_future <- IDIT_future %>%
    filter(region_reporter %in% c("Northern America", "Central America", "South America", "Europe", "Africa", "Asia", "Oceania")) %>%
    group_by(year, region_reporter) %>%
    summarize(
        mean_TIDR_USD = mean(TIDR_USD), 
        mean_TIDR_pct = mean(TIDR_pct)
        ) 
    
        
# Plot IDIT / TIDR for each region
IDIT_pct_future_plot <- IDIT_future %>% # As pct
    ggplot(aes(x = year, y = mean_TIDR_pct, group = region_reporter, color = region_reporter, shape = region_reporter)) +
    geom_line(size = 1) +
    labs(
        title = "Technologies Import Dependency Ratio (TIDR) in sample of world regions",
        x = "Year",
        y = "TIDR (calculated as % of imports - % of exports)",
        caption = "Source: UNCTADSTAT | Sample of world regions",
        color = "Region"
        shape = "Region"
    ) +
    theme_minimal()

IDIT_USD_future_plot <- IDIT_future %>% # As USD
    ggplot(aes(x = year, y = mean_TIDR_USD, group = region_reporter, color = region_reporter, shape = region_reporter)) +
    geom_line(size = 1) +
    labs(
        title = "Technologies Import Dependency Ratio (TIDR) in sample of world regions",
        x = "Year",
        y = "TIDR (calculated as USD value of imports - USD value of exports)",
        caption = "Source: UNCTADSTAT | Sample of world regions",
        color = "Region",
        shape = "Region"
    ) +
    theme_minimal()

# Save plots   
ggsave("plots/plots-v2/IDIT_pct_future_plot.png", IDIT_pct_future_plot, width = 12, height = 8)
ggsave("plots/plots-v2/IDIT_USD_future_plot.png", IDIT_USD_future_plot, width = 12, height = 8)
