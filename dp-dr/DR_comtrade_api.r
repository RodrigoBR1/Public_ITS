# Load libraries

library(pacman)
p_load(tidyverse, dplyr, comtradr, httr, countrycode)

# HS codes and descriptions
hs_codes_v3 <- tibble( # Codes for Energy-related products (non-productive commodities)
    HS_Code = c(),

    Description = c(),

    Category = c()
)

# Setting parameters for API call in UN COMTRADE (UNCT)
cmdCodes <- hs_codes_v1$HS_Code
countries <- comtradr::country_codes %>%
    filter(is.na(exit_year) & group == FALSE & reporter == TRUE) %>%
    pull(iso_3) %>%
    unique()
period <- 1962:2023

# API keys used in this section are provided by the researcher and volunteers providing alternatives
pkey_UNCT <- "" # Free suscription to UN COMTRADE's API
alt1_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt2_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt3_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt4_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt5_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt6_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt7_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt8_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt9_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt10_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API
alt11_pkey_UNCT <- "" # Alternative free suscription to UN COMTRADE's API

combinations <- expand.grid(
            reporter = split(countries, ceiling(seq_along(countries) / 6)),
            partner = split(countries, ceiling(seq_along(countries) / 6)),
            commodity_code = split(cmdCodes, ceiling(seq_along(cmdCodes) / 8)),
            start_date = seq(from = min(period), to = max(period) - 11, by = 12),
            end_date = seq(from = min(period) + 11, to = max(period), by = 12),
            flow_direction = c("import", "export")
        ) %>%
        filter((start_date < end_date) & (end_date - start_date == 11)) %>%
        mutate(
                reporter = rev(reporter),
                commodity_code = rev(commodity_code),
                start_date = rev(start_date),
                end_date = rev(end_date),
                flow_direction = rev(flow_direction)
            )
            

# AUX FUNCTIONS
# Function to make API calls iteratively using comtradr and save results to CSV files <----- MODIFIED 18/11/2024 TO MAKE KEY ARGUMENT A LIST OF KEYS AND KEEP THE DATA RETRIEVAL GOING FOR A LONG TIME
make_api_calls <- function(start_comb = 1, keys = c(pkey_UNCT, pkey2, pkey3)) {
    total_combinations <- nrow(combinations)
    num_keys <- length(keys)
    key_index <- 1  # Start with the first key
    api_call_counter <- 0  # Initialize call counter

    for (comb in start_comb:total_combinations) {
        # Extract parameters for the current combination
        current_reporters <- combinations$reporter[[comb]]
        current_cmd_codes <- combinations$commodity_code[[comb]]
        start_date <- combinations$start_date[[comb]]
        end_date <- combinations$end_date[[comb]]
        flow <- combinations$flow_direction[comb]
        combs_remaining <- total_combinations - comb

        # Switch the key after every 499 calls
        if (api_call_counter %% 499 == 0 && api_call_counter > 0) {
            key_index <- (key_index %% num_keys) + 1  # Move to the next key
            message("Switching to new key: Key ", keys[key_index])
        }
        set_primary_comtrade_key(keys[key_index])

        # Make the API call
        message("Making API call ", comb, " of ", total_combinations,
                " for flow: ", flow,
                ", reporters: ", paste(current_reporters, collapse = ", "),
                ", commodities: ", paste(current_cmd_codes, collapse = ", "),
                ", period: ", start_date, "-", end_date, " using key ", key_index)
        message("API calls remaining:", combs_remaining)
        api_call_counter <- api_call_counter + 1

        data <- tryCatch(
            ct_get_data(
                reporter = current_reporters,
                partner = "all_countries",
                commodity_code = current_cmd_codes,
                start_date = start_date,
                end_date = end_date,
                flow_direction = flow
            ),
            error = function(e) {
                if (grepl("Waiting \\d+s for retry backoff", e$message)) {
                    wait_time <- as.numeric(gsub(".*Waiting (\\d+)s for retry backoff.*", "\\1", e$message))
                    message("Waiting for ", wait_time, " seconds before retrying...")
                    Sys.sleep(wait_time)
                    data <- ct_get_data(
                        reporter = current_reporters,
                        partner = "all_countries",
                        commodity_code = current_cmd_codes,
                        start_date = start_date,
                        end_date = end_date,
                        flow_direction = flow
                    )
                } else {
                    message("Error: ", e$message)
                    stop(e)
                }
            }
        )

        # Save the results to a CSV file
        if (!is.null(data)) {
            file_name <- paste0("DR/comtrade_data-v2/", flow, "_", comb, ".csv")
            write.csv(data, file_name, row.names = FALSE)
            message("Data saved to ", file_name)
        }
    }
}
    
integrate_data <- function() {
    
    # Initialize empty data frames to store the results
    imports <- data.frame()
    exports <- data.frame()

    # Merge all individual CSV files into one for each flow
    import_files <- list.files("DR/comtrade_data-v2", pattern = "^import_.*\\.csv$", full.names = TRUE)
    export_files <- list.files("DR/comtrade_data-v2", pattern = "^export_.*\\.csv$", full.names = TRUE)

    if (length(import_files) > 0) {
        imports <- bind_rows(lapply(import_files, read.csv))
        write.csv(imports, "DR/comtrade_data-v2/all_imports.csv", row.names = FALSE)
        message("All import data merged into comtrade_data/all_imports.csv")
    }

    if (length(export_files) > 0) {
        exports <- bind_rows(lapply(export_files, read.csv))
        write.csv(exports, "DR/comtrade_data-v2/all_exports.csv", row.names = FALSE)
        message("All export data merged into comtrade_data/all_exports.csv")
    }
}

# Make API calls and integrate the data
