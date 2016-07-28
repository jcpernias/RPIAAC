library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(digest)


# -----------------------------------------------------------------------------
# Utility functions
# Download a file unless a local copy already exists
download_if_absent <- function(url, dest, force = FALSE) {
  absent <- !file.exists(dest)
  if (force && !absent) {
    file.remove(dest)
    absent <- TRUE
  }
  if(absent)
    GET(url, write_disk(dest), progress())
}


# Build a table with details of each variable.
# Retain the PIAAC Name, Label, Level, and Domain.
# REcode Type with codes as those use by the readr package:
# - 'i': Integer variables
# - 'd': floating point variables
# - 'c': character variables
# - '_': ignored variables
build_variables_table <- function (path) {
  # Read codebook sheet 'Variables': Drop columns 'Link to values' and following.
  vars <- read_excel(path, sheet = 'Variables')[1:10]

  # Variables to retain
  idx <- str_detect(vars$Domain, regex("^(Background|Sampling|Scale|Not)"))

  # Variables to drop
  drop_vars <- vars[!idx, ] %>% mutate(Type = "_")

  sel_vars <- vars[idx, ]

  # String variables
  str_vars <- sel_vars %>%
    filter(Type == 'String/character (Unicode)') %>%
    mutate(Type = "c")

  # Integer variables
  int_vars <- sel_vars %>%
    filter(Type == 'Integer', Width < 10) %>%
    mutate(Type = "i")

  # Floating point variables
  dbl_vars <- sel_vars %>%
    filter(Type == 'Numeric/floating point' |
             (Type == "Integer" & Width >= 10)) %>%
    mutate(Type = "d")

  # Merge all dataframes
  bind_rows(drop_vars, str_vars, int_vars, dbl_vars) %>%
    select(Name, Label, Type, Level, Domain)
}

# Return a list with three elements:
# - missing_codes: the codes used for missing values in the csv files.
# - codes: a dataframe with coding schemes
# - variable_code: a table that associates variables and coding schemes
build_values_tables <- function(path) {
  # Read codebook sheet 'Values'
  cb_values <- read_excel(path, sheet = 'Values')

  # Missing value codes -------------------------------------------------
  # The missing values in the CSV files are recorded as the SAS codes
  # without the leading dot.
  missing_codes <- cb_values %>%
    filter(`Value Type` == 'Missing', !is.na(`Value (SAS)`)) %>%
    distinct(`Value (SAS)`) %>%
    transmute(Miss = substr(`Value (SAS)`, 2, 2))

  # Apparently, variable CTRYQUAL also uses Z as a missing value. This is
  # not documented in the codebook
  missing_codes <- c(missing_codes$Miss, 'Z')

  # Variable codings ----------------------------------------------------
  # Filter out missing values
  valids <- cb_values %>%
    filter(`Value Type` != 'Missing') %>%
    select(Name = `Variable Name`, Label = `Value Label`, Value = `Value (SAS)`)

  # Use an environment as a hash table
  tb_env <- new.env(parent = emptyenv())

  # 1. Get the values scheme for a variable.
  # 2. Compute the hash value for that scheme.
  # 3. Add the scheme to the hash table if it is not already included.
  # 4. Return the hash.
  handle_var <- function(var) {
    tb <- valids %>% filter(Name ==  var) %>% select(Label, Value)
    md5 <- digest(tb)
    if(!exists(md5, where = tb_env))
      assign(md5, tb, envir = tb_env)
    return(md5)
  }

  # Compute the hash code for each variable and update if necessary
  # the hashtable.
  coded_vars <- (valids %>% distinct(Name))$Name
  var_hashes <- vapply(coded_vars, handle_var, "", USE.NAMES = FALSE)

  # Get the hashes in the hash table
  hashes <- unique(var_hashes)

  # Associate each var with code scheme
  variable_code <- tibble(Name = coded_vars, ID = match(var_hashes, hashes))

  # Transform the hashtable into a dataframe
  codes <- do.call(bind_rows, lapply(seq_along(hashes), function (i) {
    get(hashes[i], envir = tb_env) %>% mutate(ID = i)
  }))

  list(missing_codes = missing_codes, codes = codes, variable_code = variable_code)
}


# -----------------------------------------------------------------------------
# Missing variables by country

# Returns a dataframe that holds the missing variables
# in each country dataset.
build_missing_variables_table <- function(path) {
  # Read info in the "all data" sheet.
  # There are some extra rows in that sheet: drop them.
  miss_vars <- read_excel(path, sheet = "all data",
                          col_types = rep("text", 35)) %>%
    filter(!is.na(Domain)) %>%
    gather(Country, missing, aut:tur, na.rm = TRUE) %>%
    select(Name = `Variable name`, Country)
}


# PIAAC codebook
cb_url <-
  paste0("http://www.oecd.org/site/piaac/", URLencode(
    "International Codebook_PIAAC Public-use File (PUF) Variables and Values.xlsx"))

# Missing variables by country
miss_vars_url <-
  "http://www.oecd.org/skills/piaac/List_of_missing_variables_in_PUF_by_country.xls"

# Where the local copies are stored
cb_file <- file.path('data-raw', 'codebook.xlsx')
miss_vars_file <- file.path('data-raw', 'missing-vars.xls')

download_if_absent(cb_url, cb_file)
download_if_absent(miss_vars_url, miss_vars_file)

# Build the PIAAC metadata tables
variables <- build_variables_table(cb_file)
values <- build_values_tables(cb_file)
missing_variables <- build_missing_variables_table(miss_vars_file)

# Variable names in missing variables file are lower case.
# Capitalize them as in the codebook.
idx <- match(missing_variables$Name, str_to_lower(variables$Name))
missing_variables$Name <- variables$Name[idx]

# Drop unused variables from metadata tables
idx <- variables$Type == '_'
ignore_vars <- variables$Name[idx]
variables <- variables[!idx, ]
idx <- missing_variables$Name %in% ignore_vars
missing_variables <- missing_variables[!idx, ]
idx <- values$variable_code$Name %in% ignore_vars
values$variable_code <- values$variable_code[!idx, ]
idx <- values$codes$ID %in% unique(values$variable_code$ID)
values$codes <- values$codes[idx, ]

use_data(missing_variables, variables, values, internal = TRUE)

# URL where PIAAC data files are stored
PIAAC_URL <- 'http://vs-web-fs-1.oecd.org/piaac/puf-data/CSV'
csv_names <- html_text(html_nodes(read_html(PIAAC_URL), "a"), trim = TRUE)
csv_names <- csv_names[str_detect(csv_names, regex('\\.csv$'))]

if(!dir.exists('data'))
  dir.create('data')

country_data <- function(csv) {
  # Variables to read and their types
  build_col_types <- function(country) {
    types <- setNames(variables$Type, variables$Name)
    miss_vars <- missing_variables[missing_variables$Country == country, ]$Name
    types[miss_vars] <- "_"
    do.call(cols_only, as.list(types[types != "_"]))
  }

  country <- str_sub(csv, 4, 6)
  url <- paste(PIAAC_URL, csv, sep = "/")
  dest <- file.path('data', paste0(country, ".rda"))

  data <- read_csv(content(GET(url), "raw"),
                   na = values$missing_codes,
                   col_types = build_col_types(country))

  assign(country, data)
  save(list=country, file=dest, compress="xz")
}

for (csv in csv_names) {
  cat(csv, '\n')
  country_data(csv)
}
