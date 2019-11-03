# general parser functions

library(data.table)
library(stringr)

# get the text of a filing from a link
get_filing <- function(filing_text_link) {
  readLines(filing_text_link) %>% 
    tolower %>% 
    paste0(collapse = "")
}


# list of all filings in a given year 
list_filings <- function(year) {
  list.files(paste0("data/processed/", year), 
             full.names = T, 
             recursive = T, 
             pattern = "crawler.csv")
}

# guess whether it is text or html format
guess_text_html <- function(filing) {
  if(grepl("<tr>", filing) & grepl("</tr>", filing)) {
    "html"
  } else {
    "text"
  }
}

# check the format for each column
# e.g. is is comma-separated numbers,
# dollar denominated, etc.
check_col_format <- function(df) {
  sapply(df, check_format)
}

# parse the tables within a filing
# using the functions above
parse_single_table <- function(x) {
  data <- fix_dollar_placement(x) %>% 
    fix_percent_placement %>% 
    fix_extra_spaces_in_parens %>% 
    strange_characters_to_spaces %>%
    fix_separators %>%
    read_char_table %>% 
    replace_empty %>% 
    remove_na_cols() %>% 
    unique
  
  # grab just the complete data
  data[complete.cases(data)]
  
}

parse_multiple_tables <- function(x) {
  parsed_tbls <- list()
  
  for(i in 1:length(x)) {
    parsed_tbls[[i]] <- parse_single_table(x[[i]])
  }
  
  parsed_tbls
}

# get the format of most of the tables in the list
# often disclosures get embedded in tables
# so do things like the name of the fund
# we don't want that mucking up the format
get_majority_format <- function(df_list) {
  lapply(df_list, check_col_format) %>% 
    sapply(paste0, collapse = ", ") %>% 
    table %>% # get counts
    sort(decreasing = T) %>% 
    head(1) %>% 
    names
}

# filter tables based on the majority format
get_relevant_tables <- function(df_list) {
  format <- get_majority_format(df_list)
  
  Filter(function(x) paste0(check_col_format(x), collapse = ", ") == format, df_list)
}

combine_all_tables_from_filing <- function(filing_html_tables) {
  
  if(is.list(filing_html_tables[[1]])) {
    filing_html_tables <- unlist(filing_html_tables, recursive = F)
  }
  
  parse_multiple_tables(filing_html_tables) %>% 
    get_relevant_tables %>% 
    rbindlist
}


