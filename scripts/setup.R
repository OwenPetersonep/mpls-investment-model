# Run this once — install all project dependencies
packages <- c(
  "tidyverse",    # data manipulation + ggplot2
  "tidycensus",   # Census ACS data
  "sf",           # spatial data (shapefiles)
  "janitor",      # column name cleaning
  "scales",       # number formatting
  "lubridate",    # date handling
  "corrplot",     # correlation visualization
  "broom",        # tidy regression output
  "writexl",      # export to Excel
  "jsonlite",     # JSON handling for AI API
  "httr2",        # HTTP requests for Claude API
  "readxl"        # read Excel files back in
)

install.packages(packages)

# Load them all
lapply(packages, library, character.only = TRUE)


census_api_key("3fb5f9b04803299fd418716a1e608f0dd62f0915", install = TRUE)
# Restart R after running this once

