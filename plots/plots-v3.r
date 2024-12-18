# Libraries
library(pacman)
p_load(tidyverse, maps, geosphere, ggplot2, plotly, ggrepel, htmlwidgets, countrycode, flextable)

# Load data and clean
ITS <- read.csv("data/ITS_v1.csv") %>%
    mutate(pats = agr_pat + min_pat + dat_pat)
ITS$continent <- countrycode(ITS$country, "country.name", "continent")

# Make sure every country & year is unique
ITS <- ITS %>% 
    distinct(country, year, .keep_all = TRUE)

# Data exploration - definition of plots and countries of interest
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

# All family of patents in force per country and year in LAS
pat_plot_LAS <- ggplot(ITS_LAS, 
    aes(x = year, y = pats, group = country, color = country, shape = country)) +
    geom_line() +
    scale_color_manual(values = colors_LAS) +
    scale_shape_manual(values = shapes_LAS) +
    labs(title = "All family of patents of interest in force in LAS",
        x = "Year",
        y = "Count of all family of patents of interest in force",
        caption = "Source: Own elaboration based on Google Public Patents Dataset") +
    theme_minimal()

# Net GFCF per country and year in LAS
GFCF_plot_LAS <- ggplot(ITS_LAS, 
    aes(x = year, y = Net_GFCF, group = country, color = country, shape = country)) +
    geom_line() +
    scale_color_manual(values = colors_LAS) +
    scale_shape_manual(values = shapes_LAS) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
    geom_hline(yintercept = -100, linetype = "dotted", color = "#000e88") + # Net exporter of investment in capital
    geom_hline(yintercept = 100, linetype = "dotted", color = "#880000") + # Net importer of investment in capital
    labs(title = "Net Gross Fixed Capital Formation (GFCF) in LAS",
        x = "Year",
        y = "Net GFCF",
        caption = "Source: Own elaboration based on UNCTADSTAT") +
    ylim(-1000, 1000) + # Values outside 1000 percent are outliers not considered in this plot
    theme_minimal()

# Test correlation of ECI with TIDR in LAS
correlation_results_LAS <- ITS_LAS %>%
  group_by(country) %>%
  summarise(correlation_ECI_TIDR = cor(ECI, IDIT, use = "complete.obs"))

# Create an ECI plot for each unique country in ITS_LAS
plots_list <- list()
for (country in unique(ITS_LAS$country)) {
    country_data <- ITS_LAS %>% filter(country == !!country)
    correlation_value <- correlation_results_LAS %>% filter(country == !!country) %>% pull(correlation_ECI_TIDR) %>% round(2)
    plot <- ggplot(country_data, aes(x = year)) +
        scale_y_continuous(
            name = "TIDR",
            limits = c(-300, 200),
            breaks = seq(-300, 200, by = 50),
            sec.axis = sec_axis(
                trans = ~ . / 153.85,
                breaks = seq(-1.3, 1.3, by = 0.2),
                name = "ECI"
            )
        ) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
        geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
        geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
        geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
        geom_line(aes(y = IDIT, color = "TIDR"), alpha = 1) +
        geom_line(aes(y = IDIT_China, color = "TIDR relative to China"), alpha = 0.3) +
        geom_line(aes(y = IDIT_USA, color = "TIDR relative to USA"), alpha = 0.3) +
        geom_line(aes(y = ECI * 153.85, color = "ECI", shape = "ECI"), alpha = 1) +
        labs(
            title = paste("Economic Complexity Index (ECI) and TIDR in", country),
            subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)",
            color = "Indicator",
            x = "Year",
            y = "TIDR",
            caption = paste("Source: Own elaboration based on The Observatory of Economic Complexity and ITS indicators | Pearson R between ECI and TIDR:", correlation_value)
        ) +
        theme_minimal() +
        theme(plot.subtitle = element_text(size = 7))

    plots_list[[country]] <- plot
}

# Save each plot in plots-v3 folder
for (country in names(plots_list)) {
  ggsave(
    paste0("plots/plots-v3/ECI-plots/ECI_TIDR_", country, ".png"),
    plots_list[[country]],
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
}

# Plot of ECI and TIDR in LAS, also test correlation
ITS_LAS_SUM <- ITS_LAS %>%
    group_by(year) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    select(-country, -main_tech_capital_source, -continent)

ECI_TIDR_LAS <- ggplot(ITS_LAS_SUM, aes(x = year)) +
    scale_y_continuous(
        name = "TIDR",
        limits = c(-300, 200),
        breaks = seq(-300, 200, by = 50),
        sec.axis = sec_axis(
            transform = ~ . / 153.85,
            breaks = seq(-1.3, 1.3, by = 0.2),
            name = "ECI"
        )
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
    geom_hline(yintercept = 50, linetype = "dotted", color = "#b81c00") + # Medium import dependency
    geom_hline(yintercept = -100, linetype = "dotted", color = "#035000") + # High export capability
    geom_hline(yintercept = -200, linetype = "dotted", color = "#00791a") + # Very high import capability
    geom_line(aes(y = IDIT, color = "TIDR"), alpha = 1) +
    geom_line(aes(y = IDIT_China, color = "TIDR relative to China"), alpha = 0.3) +
    geom_line(aes(y = IDIT_USA, color = "TIDR relative to USA"), alpha = 0.3) +
    geom_line(aes(y = ECI * 153.85, color = "ECI"), alpha = 1) +
    labs(
        title = "Economic Complexity Index (ECI) and TIDR in Latin American economies",
        subtitle = "Dotted lines represent: Autarky (0), Medium import dependency (50), High export capability (-100), Very high import capability (-200)",
        color = "Indicator",
        x = "Year",
        y = "TIDR",
        caption = paste0("Source: Own elaboration based on The Observatory of Economic Complexity and ITS indicators | Mean Pearson R between ECI and TIDR in LAS: ", 
    round(mean(correlation_results_LAS$correlation_ECI_TIDR, na.rm = TRUE), 2))
    ) +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 7))

    # Save plots in plots-v3 folder
ggsave("plots/plots-v3/TIDR_plot_LAS.png", TIDR_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/TIDR_China_plot_LAS.png", TIDR_China_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/TIDR_USA_plot_LAS.png", TIDR_USA_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/pat_plot_LAS.png", pat_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/GFCF_plot_LAS.png", GFCF_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/ECI_plot_LAS.png", ECI_plot_LAS, width = 10, height = 6, units = "in", dpi = 300)
ggsave("plots/plots-v3/ECI_TIDR_LAS.png", ECI_TIDR_LAS, width = 10, height = 6, units = "in", dpi = 300)