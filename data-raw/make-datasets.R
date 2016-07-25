library(httr)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(rvest)

#-------------------------------------------------------------
# Codebook
# Download the PIAAC codebook

# The name of the file
cb_name <-
  'International Codebook_PIAAC Public-use File (PUF) Variables and Values.xlsx'

# The place where it lives
cb_url_root <- 'http://www.oecd.org/site/piaac/'

# Where the local copy is stored
cb_file <- file.path('data-raw', 'codebook.xlsx')

# Download unless a local copy already exists
file.exists(cb_file) ||
  GET(paste0(cb_url_root, URLencode(cb_name)),
      write_disk(cb_file), progress())

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


# Read codebook sheet 'Variables': Drop column 'Link to values'
cb_vars <- read_excel(cb_file, sheet = 'Variables')[1:10]

# Drop task results variables
task_vars <- (cb_vars %>%
                filter(str_detect(Domain, regex('\\(paper|computer\\)'))) %>%
                mutate(cols = "_") %>%
                select(Name, cols))

# Conversion of PIAAC types to readr column types
type_codes <- function(type, width) {
  conv <- c('Numeric/floating point' = 'd',
            'Integer' = 'i',
            'String/character (Unicode)' = 'c')
  code <- conv[type]
  idx <- which(code == 'i' & width > 9)
  if (length(idx))
    code[idx] <- 'd'
  code
}

other_vars <- anti_join(cb_vars, task_vars, by = "Name") %>%
  select(Name, Type, Width) %>%
  mutate(cols=type_codes(Type, Width)) %>%
  select(Name, cols)

all_vars <- bind_rows(task_vars, other_vars)
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
