library(httr)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(rvest)

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


# -----------------------------------------------------------------------------
# Download metadata files

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


# -----------------------------------------------------------------------------
# Codes for missing values

# Read codebook sheet 'Values'
cb_values <- read_excel(cb_file, sheet = 'Values')

# The missing values in the CSV files are recorded as the SAS codes
# without the leading dot.
miss <- (cb_values %>%
           filter(`Value Type` == 'Missing', !is.na(`Value (SAS)`)) %>%
           select(Missing = `Value (SAS)`) %>%
           distinct(Missing) %>%
           transmute(substr(Missing, 2, 2)))[[1]]

# Apparently, variable CTRYQUAL also uses Z as a missing value. This is
# not documented in the codebook
miss <- c(miss, 'Z')


# -----------------------------------------------------------------------------
# Missing variables by country

# Read info in the "all data" sheet.
# There are some extra rows in that sheet: drop them.
# In the end, we get a data frame with only two
# columns: Country and (variable) Name
miss_vars <- read_excel(miss_vars_file, sheet = "all data",
                        col_types = rep("text", 35)) %>%
  filter(!is.na(Domain)) %>%
  gather(Country, missing, aut:tur, na.rm = TRUE) %>%
  mutate(Name = str_to_upper(`Variable name`)) %>%
  select(Name, Country)


# -----------------------------------------------------------------------------
# Variables to store and their type


# Read codebook sheet 'Variables': Drop columns 'Link to values' and following.
# Also, only use capital letters in variable names.
cb_vars <- read_excel(cb_file, sheet = 'Variables')[1:10] %>%
  mutate(Name = str_to_upper(Name))

# Drop task results variables
task_vars <- (cb_vars %>%
                filter(str_detect(Domain, regex('\\(paper|computer\\)'))) %>%
                mutate(cols = "_") %>%
                select(Name, cols))

# Conversion of PIAAC types to readr column types
type_codes <- function(type, width) {
  # Table to convert PIAAC types to readr type codes
  conv <- c('Numeric/floating point' = 'd',
            'Integer' = 'i',
            'String/character (Unicode)' = 'c')
  code <- conv[type]

  # Don't use interger type if a field width is greater than 9
  idx <- which(code == 'i' & width > 9)
  if (length(idx))
    code[idx] <- 'd'
  code
}

other_vars <- anti_join(cb_vars, task_vars, by = "Name") %>%
  select(Name, Type, Width) %>%
  mutate(cols=type_codes(Type, Width)) %>%
  select(Name, cols)

var_types <- bind_rows(task_vars, other_vars)



var_types2 <- var_types
idx <- which(var_types2$Name %in% (miss_vars %>% filter(Country == "aut"))$Name)
var_types2[idx, 2] <- '_'

col_types <- do.call(cols_only, setNames(as.list(all_vars$cols), all_vars$Name))


# -----------------------------------------------------------------------
# Public use files

# URL where PIAAC data files are stored
PIAAC_URL <- 'http://vs-web-fs-1.oecd.org/piaac/puf-data/CSV'
nodes <- html_nodes(read_html(PIAAC_URL), "a")
puf_names <- html_text(nodes, trim = TRUE)
puf_names <- puf_names[str_detect(puf_names, regex('\\.csv$'))]

if(!dir.exists('data'))
  dir.create('data')

country_data <- function(puf) {
  country <- str_sub(puf, 4, 6)
  url <- paste(PIAAC_URL, puf, sep = "/")
  dest <- file.path('data', paste0(country, ".rda"))
  assign(country, read_csv(content(GET(url), "raw"),
                           na = miss, col_types = col_types))
  save(list=country, file=dest, compress="xz")
}

for (puf in puf_names) {
  cat(puf, '\n')
  country_data(puf)
}
