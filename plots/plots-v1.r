# Libraries
library(pacman)
p_load(tidyverse, countrycode, ggplot2, plotly, flextable, ggrepel, ggpubr, webshot, brolgar)

# Load data and clean
ITS <- read.csv("data/ITS_v1.csv") %>%
    mutate(pats = agr_pat + min_pat + dat_pat)
ITS$continent <- countrycode(ITS$country, "country.name", "continent")

# Make sure every country & year is unique
ITS <- ITS %>% 
    distinct(country, year, .keep_all = TRUE)

# Subsample countries
# Identify top 10 countries with lowest mean IDIT throughout the years
top_10_lowest_TIDR <- ITS %>% 
    group_by(country) %>%
    summarize(mean_TIDR = mean(IDIT)) %>%
    arrange(mean_TIDR) %>%
    head(10)

# Identify top 10 countries with highest mean PATS throughout the years
top_10_highest_PATS <- ITS %>% 
    group_by(country) %>%
    summarize(mean_pats = mean(pats)) %>%
    arrange(desc(mean_pats)) %>%
    head(10)

# Identify top 10 countries with lowest mean Net_GFCF throughout the years
top_10_lowest_Net_GFCF <- ITS %>% 
    group_by(country) %>%
    summarize(mean_Net_GFCF = mean(Net_GFCF)) %>%
    arrange(mean_Net_GFCF) %>%
    head(10)

# Subsample LAS    
subsample_LAS <- c("Brazil", "Chile", "Argentina", "Bolivia", "Paraguay", "Colombia", "Peru", "Mexico")


# Technologies Import Depdency Ratio (TIDR) per country and year in the world
TIDR_plot <- ggplot(ITS, 
        aes(x = year, y = IDIT, group = country, color = continent)) +
        geom_line(data = subset(ITS, !country %in% top_10_lowest_TIDR$country), alpha = 0.2) +
        geom_line(data = subset(ITS, country %in% top_10_lowest_TIDR$country), alpha = 1) +
        geom_hline(yintercept = 50, linetype = "dotted", color = "black") + # Medium-high import dependency
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
        geom_hline(yintercept = -100, linetype = "dotted", color = "#880000") + # High export capability
        geom_hline(yintercept = -400, linetype = "dotted", color = "red") + # Very high export capability
        geom_label_repel(data = subset(ITS, country %in% top_10_lowest_TIDR$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                 aes(label = country, color = continent),
                 size = 3, 
                 nudge_x = 0, 
                 nudge_y = -10, 
                 direction = "y", 
                 hjust = 1, 
                 segment.color = "grey50", 
                 show.legend = FALSE) +
        facet_wrap(~continent) +
        labs(title = "Technologies Import Dependency Ratio (TIDR) in the world",
          subtitle = "Top 5 lowest TIDR countries in 2018 include labels",
          x = "Year",
          y = "TIDR",
          color = "Continent",
          caption = "Source: Own elaboration based on UNCTADSTAT | Top 10 lowest TIDR (least dependant) countries highlighted") +
        theme_minimal()

# TIDR relative to China per country and year in the world
TIDR_China_plot <- ggplot(ITS, 
                aes(x = year, y = IDIT_China, group = country, color = continent)) +
                geom_line(data = subset(ITS, !country %in% top_10_lowest_TIDR$country), alpha = 0.2) +
                geom_line(data = subset(ITS, country %in% top_10_lowest_TIDR$country), alpha = 1) +
                geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
                geom_hline(yintercept = -100, linetype = "dotted", color = "#880000") + # High export capability
                geom_hline(yintercept = -400, linetype = "dotted", color = "red") + # Very high export capability
                geom_label_repel(data = subset(ITS, country %in% top_10_lowest_TIDR$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                          aes(label = country, color = continent),
                          size = 3, 
                          nudge_x = 0, 
                          nudge_y = -10, 
                          direction = "y", 
                          hjust = 1, 
                          segment.color = "grey50", 
                          show.legend = FALSE) +
                facet_wrap(~continent) +
                labs(title = "Technologies Import Dependency Ratio (TIDR) relative to China in the world",
                    subtitle = "Top 5 lowest TIDR (relative to China) countries in 2018 include labels",
                    x = "Year",
                    y = "TIDR relative to China",
                    caption = "Source: Own elaboration based on UNCTADSTAT | Top 10 lowest TIDR (least dependant) countries highlighted") +
                theme_minimal()

# TIDR relative to the United States per country and year in the world
TIDR_USA_plot <- ggplot(ITS, 
                aes(x = year, y = IDIT_USA, group = country, color = continent)) +
                geom_line(data = subset(ITS, !country %in% top_10_lowest_TIDR$country), alpha = 0.2) +
                geom_line(data = subset(ITS, country %in% top_10_lowest_TIDR$country), alpha = 1) +
                geom_hline(yintercept = 50, linetype = "dotted", color = "black") + # Medium-high import dependency
                geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
                geom_hline(yintercept = -100, linetype = "dotted", color = "#880000") + # High export capability
                geom_hline(yintercept = -400, linetype = "dotted", color = "red") + # Very high export capability
                geom_label_repel(data = subset(ITS, country %in% top_10_lowest_TIDR$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                          aes(label = country, color = continent),
                          size = 3, 
                          nudge_x = 0, 
                          nudge_y = -10, 
                          direction = "y", 
                          hjust = 1, 
                          segment.color = "grey50", 
                          show.legend = FALSE) +
                facet_wrap(~continent) +
                labs(title = "Technologies Import Dependency Ratio (TIDR) relative to the United States in the world",
                    subtitle = "Top 5 lowest TIDR (relative to China) countries in 2018 include labels",
                    x = "Year",
                    y = "TIDR relative to the United States",
                    color = "Continent",
                    caption = "Source: Own elaboration based on UNCTADSTAT | Top 10 lowest TIDR (least dependant) countries highlighted") +
                theme_minimal()

# Agriculture family of patents in force per country and year in the world
agrpat_plot <- ggplot(ITS, 
                aes(x = year, y = agr_pat, group = country, color = continent)) +
                geom_line(data = subset(ITS, !country %in% top_10_highest_PATS$country), alpha = 0.2) +
                geom_line(data = subset(ITS, country %in% top_10_highest_PATS$country), alpha = 1) +
                geom_label_repel(data = subset(ITS, country %in% top_10_highest_PATS$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                          aes(label = country, color = continent),
                          size = 3, 
                          nudge_x = 0, 
                          nudge_y = -10, 
                          direction = "y", 
                          hjust = 1, 
                          segment.color = "grey50", 
                          show.legend = FALSE) +
                labs(title = "Agriculture related intellectual property in the world",
                    subtitle = "Top 5 highest patent count (most innovative) countries in 2018 include labels",
                    x = "Year",
                    y = "Count of agriculture related family of patents in force",
                    caption = "Source: Own elaboration based on Google Public Patents Dataset | Top 10 highets patent count (most innovative) countries highlighted") +
                theme_minimal()

# Mining family of patents in force per country and year in the world (min_pat)
minpat_plot <- ggplot(ITS, 
        aes(x = year, y = min_pat, group = country, color = continent)) +
        geom_line(data = subset(ITS, !country %in% G20_countries), alpha = 0.2) +
        geom_line(data = subset(ITS, country %in% G20_countries), alpha = 1) +        
                geom_label_repel(data = subset(ITS, country %in% top_10_highest_PATS$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                          aes(label = country, color = continent),
                          size = 3, 
                          nudge_x = 0, 
                          nudge_y = -10, 
                          direction = "y", 
                          hjust = 1, 
                          segment.color = "grey50", 
                          show.legend = FALSE) +
        labs(title = "Mining related intellectual property in the world",
            subtitle = "Top 5 highest patent count (most innovative) countries in 2018 include labels",
            x = "Year",
            y = "Count of mining related family of patents in force",
            caption = "Source: Own elaboration based on Google Public Patents Dataset | Top 10 highets patent count (most innovative) countries highlighted") +
        theme_minimal()

# Data processing family of patents in force per country and year in the world (dat_pat)
datpat_plot <- ggplot(ITS, 
        aes(x = year, y = dat_pat, group = country, color = continent)) +
        geom_line(data = subset(ITS, !country %in% top_10_highest_PATS$country), alpha = 0.2) +
        geom_line(data = subset(ITS, country %in% top_10_highest_PATS$country), alpha = 1) +        facet_wrap(~continent) +
                geom_label_repel(data = subset(ITS, country %in% top_10_highest_PATS$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                          aes(label = country, color = continent),
                          size = 3, 
                          nudge_x = 0, 
                          nudge_y = -10, 
                          direction = "y", 
                          hjust = 1, 
                          segment.color = "grey50", 
                          show.legend = FALSE) +
        labs(title = "Data processing related intellectual property in the world",
            subtitle = "Top 5 highest patent count (most innovative) countries in 2018 include labels",
            x = "Year",
            y = "Count of data related family of patents in force",
            caption = "Source: Own elaboration based on Google Public Patents Dataset | Top 10 highets patent count (most innovative) countries highlighted") +
        theme_minimal()

# All family of patents in force per country and year in the world
pat_plot <- ggplot(ITS, 
        aes(x = year, y = agr_pat + min_pat + dat_pat, group = country, color = continent)) +
        geom_line(data = subset(ITS, !country %in% top_10_highest_PATS$country), alpha = 0.2) +
        geom_line(data = subset(ITS, country %in% top_10_highest_PATS$country), alpha = 1) +        facet_wrap(~continent) +
        geom_label_repel(data = subset(ITS, country %in% top_10_highest_PATS$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                aes(label = country, color = continent),
                size = 3, 
                nudge_x = 0, 
                nudge_y = -10, 
                direction = "y", 
                hjust = 1, 
                segment.color = "grey50", 
                show.legend = FALSE) +
        labs(title = "All family of patents of interest in force in the world",
            subtitle = "Top 5 highest patent count (most innovative) countries in 2018 include labels",
            x = "Year",
            y = "Count of all family of patents of interest in force",
            caption = "Source: Own elaboation based on Google Public Patents Dataset | Top 10 highets patent count (most innovative) countries highlighted") +
        theme_minimal()

# Net GFCF per country and year in the world
GFCF_plot <- ggplot(ITS, 
        aes(x = year, y = Net_GFCF, group = country, color = continent)) +
        geom_line(data = subset(ITS, !country %in% top_10_lowest_Net_GFCF$country), alpha = 0.2) +
        geom_line(data = subset(ITS, country %in% top_10_lowest_Net_GFCF$country), alpha = 1) +        facet_wrap(~continent) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") + # Autarky
        geom_hline(yintercept = -100, linetype = "dotted", color = "#000e88") + # Net exporter of investment in capital
        geom_hline(yintercept = 100, linetype = "dotted", color = "#880000") + # Net importer of investment in capital
        geom_label_repel(data = subset(ITS, country %in% top_10_highest_Net_GFCF$country[1:5] & year == 2018), # Top 5 lowest TIDR countries in 2018
                aes(label = country, color = continent),
                size = 3, 
                nudge_x = 0, 
                nudge_y = -10, 
                direction = "y", 
                hjust = 1, 
                segment.color = "grey50", 
                show.legend = FALSE) +
        labs(title = "Net Gross Fixed Capital Formation (GFCF) in the world",
            x = "Year",
            y = "Net GFCF",
            caption = "Source: Own elaboration based on UNCTADSTAT | Top 10 highest financing (most economically present) countries highlighted") +
        ylim(-1000, 1000) + # Values outside 1000 percent are outliers not considered in this plot
        theme_minimal()


# Test correlation of ECI with TIDR
correlation_results <- ITS %>%
  filter(country %in% top_10_lowest_TIDR$country) %>%
  group_by(country) %>%
  summarise(correlation_ECI_TIDIR = cor(ECI, IDIT, use = "complete.obs")) %>%
  flextable()

# Economic Complexity Index (ECI) for top 10 countries with lowest TIDR, the most export-capable countries in sectors of interest
ECI_plot <- ggplot(data = subset(ITS, country %in% top_10_lowest_TIDR$country), 
  aes(x = year)) +
  geom_line(aes(y = ECI, color = "ECI"), alpha = 1) +
  geom_line(aes(y = IDIT, color = "TIDR"), alpha = 1) +
  geom_line(aes(y = IDIT_China, color = "TIDR relative to China"), alpha = 0.5) +
  geom_line(aes(y = IDIT_USA, color = "TIDR relative to USA"), alpha = 0.5) +
  facet_wrap(~country) +
  labs(title = "Economic Complexity Index (ECI) and TIDR in the world",
      subtitle = "Plot per country, only top 10 lowest TIDR countries (biggest net exporters). Lighter lights are TIDR relative to China and the United States.",
      color = "Indicator",
      x = "Year",
      y = "ECI",
      caption = "Source: Own elaboration based on The Observatory of Economic Complexity and ITS indicators | Only top 10 lowest TIDR countries (logistically dominant)") +
  scale_y_continuous(
      name = "ECI",
      sec.axis = sec_axis(~ ., name = "IDIT")
  ) +
  theme_minimal()


# Table with top 10 and bottom 10 countries for each indicator and year, as well as the world average
# Create a summary table
summary_table <- ITS %>%
  group_by(year) %>%
  summarise(
    TIDR = paste0(
      "Top five countries: ", paste(head(country[order(-IDIT)], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-IDIT)], 5), collapse = ", ")
    ),
    TIDR_China = paste0(
      "Top five countries: ", paste(head(country[order(-IDIT_China)], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-IDIT_China)], 5), collapse = ", ")
    ),
    TIDR_USA = paste0(
      "Top five countries: ", paste(head(country[order(-IDIT_USA)], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-IDIT_USA)], 5), collapse = ", ")
    ),
    pats_of_interest = paste0(
      "Top five countries: ", paste(head(country[order(-(agr_pat + min_pat + dat_pat))], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-(agr_pat + min_pat + dat_pat))], 5), collapse = ", ")
    ),
    Net_GFCF = paste0(
      "Top five countries: ", paste(head(country[order(-Net_GFCF)], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-Net_GFCF)], 5), collapse = ", ")
    ),
    ECI = paste0(
      "Top five countries: ", paste(head(country[order(-ECI)], 5), collapse = ", "), 
      "; Bottom five countries: ", paste(tail(country[order(-ECI)], 5), collapse = ", ")
    )
  ) %>%
  arrange(desc(year)) %>%
  flextable()

# Save plots and table
ggsave("plots/plots-v1/TIDR_plot.png", TIDR_plot, width = 12, height = 8)
ggsave("plots/plots-v1/TIDR_China_plot.png", TIDR_China_plot, width = 12, height = 8)
ggsave("plots/plots-v1/TIDR_USA_plot.png", TIDR_USA_plot, width = 12, height = 8)
ggsave("plots/plots-v1/agrpat_plot.png", agrpat_plot, width = 12, height = 8)
ggsave("plots/plots-v1/minpat_plot.png", minpat_plot, width = 12, height = 8)
ggsave("plots/plots-v1/datpat_plot.png", datpat_plot, width = 12, height = 8)
ggsave("plots/plots-v1/GFCF_plot.png", GFCF_plot, width = 12, height = 8)
ggsave("plots/plots-v1/ECI_plot.png", ECI_plot, width = 12, height = 8)
ggsave("plots/plots-v1/pat_plot.png", pat_plot, width = 12, height = 8)
save_as_image(summary_table, "plots/summary_table.png")
save_as_image(correlation_results, "plots/correlation_results.png")



