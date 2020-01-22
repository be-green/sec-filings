# general parser functions

library(data.table)
library(stringr)

# get the text of a filing from a link
get_filing <- function(filing_text_link) {
  filing <- 
    readLines(filing_text_link) %>% 
    tolower 
  
  do.call(paste0("collapse_", guess_text_html(filing)), 
          args = list(filing = filing))

}

get_filing_tables <- function(filing_url) {
  filing <- get_filing(filing_url)
  
  do.call(paste0("get_", guess_text_html(filing), 
                 "_filing_tables"), 
          args = list(filing = filing))
}

# collapse lines of an html document
# line structure is irrelevant and breaks things
# in the existing code
collapse_html <- function(filing) {
  paste0(filing, collapse = "")
}

# critical to preserve line breaks
# otherwise we can't parse the tables at all
collapse_text <- function(filing) {
  paste0(filing, collapse = "\r\n")
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
  if(any(str_detect(string = tolower(filing), fixed("<tr>")))) {
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
    fix_dollar_placement %>% # again, for some reason
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
  filing_html_tables %>% 
    get_relevant_tables %>% 
    rbindlist
}

unlist_until_table <- function(table_list) {
  # while table_list is a list of length > 0
  # unlist until there is at least one element a level down
  # that is a table
  while(is.list(table_list) & 
        !any(sapply(table_list, function(x) "data.frame" %in% class(x))) &
        length(table_list) > 0) {
    table_list <- unlist(table_list, recursive = F)
  }
  
  table_list
}
