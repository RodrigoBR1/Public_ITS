#Libraries and aux functions
library(pacman)
p_load(tidyverse, arrow, dtplyr, DBI, RSQLite, data.table)


# Identify folder paths for data
folder_paths <- c("DR/comtrade_data_v1/","DR/comtrade_data_v2/")

# Function to map how many "import_[000].csv" and "export_[000].csv" files are in each folder
map_files <- function(folder_paths, batch_size = 1000) {
  file_paths <- list()
  for (folder_path in folder_paths) {
    import_files <- list.files(path = folder_path, pattern = "import_.*.csv", full.names = TRUE)
    export_files <- list.files(path = folder_path, pattern = "export_.*.csv", full.names = TRUE)
    all_files <- c(import_files, export_files)
    file_paths <- c(file_paths, all_files)
  }

  return(file_paths)
  # Sort the file paths in ascending order
  file_paths <- sort(file_paths)
}

# Map files
file_paths <- map_files(folder_paths)

# Columns of interest
comtrade_cols <- c("period", "cmdCode", "flowDesc", "reporterDesc", "partnerDesc")

# Load HS codes for categorization
hs_codes <- read_csv("DR/hs_codes.csv")


# Create COMTRADE database
conn <- dbConnect(RSQLite::SQLite(), dbname = "DR/comtrade.sqlite") # Create / Connect database
dbWriteTable(conn, "comtrade_data", data.frame(), overwrite = TRUE) # Write empty table to database

# Fill database with files in file_path
for (file_path in file_paths) {

  tryCatch({
    dt <- arrow::read_csv_arrow(file_path, col_select = comtrade_cols) # Read only relevant data
    dbWriteTable(conn, "comtrade_data", dt, append = TRUE) # Append data to database
    message("Loaded file: ", file_path)
  }, error = function(e) {
    message("Error loading file: ", file_path)
  })
}

rm(dt) # Remove dt object from memory


# Join "Category" column to "comtrade_data"
dbWriteTable(conn, "hs_codes", hs_codes, overwrite = TRUE) # Create hs_codes as table in database
dbExecute(conn, "ALTER TABLE comtrade_data 
                  ADD COLUMN Category TEXT") # Create empty Category column
dbExecute(conn, "UPDATE comtrade_data 
                  SET Category = (
                    SELECT hs_codes.Category
                    FROM hs_codes
                    WHERE comtrade_data.cmdCode = hs_codes.HS_Code
                    )"
            ) # Fill Category column with values from hs_codes
message("Category column added to comtrade_data")

# Query table for info
table_info <- dbGetQuery(conn, "PRAGMA table_info(comtrade_data)")

# Create a sample table "comtrade_data_sample" with 1000 randomly selected rows
dbExecute(conn, "
  CREATE TABLE comtrade_data_sample AS
  SELECT *
  FROM comtrade_data
  ORDER BY RANDOM()
  LIMIT 1000;
")

# Verificar que la tabla se ha creado y ver las primeras filas (opcional)
sample_data <- dbReadTable(conn, "comtrade_data_sample")
head(sample_data)

# Data summarization and creation of comtrade_sum table in the database
dbExecute(conn, "
  CREATE TABLE comtrade_sum AS
  SELECT 
    period, 
    reporterDesc,
    SUM(CASE WHEN Category = 'Agriculture' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_agr,
    SUM(CASE WHEN Category = 'Agriculture' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_agr,
    SUM(CASE WHEN Category = 'Mining' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_min,
    SUM(CASE WHEN Category = 'Mining' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_min,
    SUM(CASE WHEN Category = 'Logistics' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_log,
    SUM(CASE WHEN Category = 'Logistics' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_log,
    SUM(CASE WHEN Category = 'Nuclear' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_nuc,
    SUM(CASE WHEN Category = 'Nuclear' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_nuc,
    SUM(CASE WHEN Category = 'Wind' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_wind,
    SUM(CASE WHEN Category = 'Wind' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_wind,
    SUM(CASE WHEN Category = 'Solar' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_solar,
    SUM(CASE WHEN Category = 'Solar' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_solar,
    SUM(CASE WHEN Category = 'Biomass' AND flowDesc = 'Import' THEN 1 ELSE 0 END) AS import_count_bio,
    SUM(CASE WHEN Category = 'Biomass' AND flowDesc = 'Export' THEN 1 ELSE 0 END) AS export_count_bio,
    SUM(CASE WHEN flowDesc = 'Import' THEN 1 ELSE 0 END) AS all_imports,
    SUM(CASE WHEN flowDesc = 'Export' THEN 1 ELSE 0 END) AS all_exports
  FROM comtrade_data
  GROUP BY period, reporterDesc
")
message("comtrade_sum table created and data summarized")

# Retrieve comtrade_sum data
comtrade_sum <- dbReadTable(conn, "comtrade_sum")

# Create TIDR columns and values
comtrade_sum <- comtrade_sum %>%
        mutate(
            TIDR_agr = (((import_count_agr - export_count_agr) / import_count_agr) * 100),
            TIDR_min = (((import_count_min - export_count_min) / import_count_min) * 100),
            TIDR_log = (((import_count_log - export_count_log) / import_count_log) * 100),
            TIDR_nuc = (((import_count_nuc - export_count_nuc) / import_count_nuc) * 100),
            TIDR_wind = (((import_count_wind - export_count_wind) / import_count_wind) * 100),
            TIDR_solar = (((import_count_solar - export_count_solar) / import_count_solar) * 100),
            TIDR_bio = (((import_count_bio - export_count_bio) / import_count_bio) * 100),
            TIDR_all = (((all_imports - all_exports) / all_imports) * 100)
        ) %>%
        rename("country" = "reporterDesc", "year" = "period") %>%
        select("year", "country", "TIDR_all", "TIDR_agr", "TIDR_min", "TIDR_log", "TIDR_nuc", "TIDR_wind", "TIDR_solar", "TIDR_bio") # Select relevant columns

# Save data
write_csv(comtrade_sum, "DR/comtrade_sum.csv") # Summarized data for visualization
print("Data written")
