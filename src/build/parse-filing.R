library(data.table)
library(stringr)

get_filing <- function(filing_text_link) {
  readLines(filing_text_link) %>% 
    tolower %>% 
    paste0(collapse = "")
}

# returns an xml2 object containing all of the html
# in the relevant filing
# there can be multiple filings per text document,
# and this list will contain all of them
get_filing_html <- function(filing) {
  
  filing_html_list <- filing %>% 
    str_split(pattern = "<body", simplify = T) %>% 
    as.vector

  filing_html_tables <- subset(filing_html_list, str_detect(filing_html_list,
                                                            "</body>"))
  
  ends <- str_locate(filing_html_tables, "/body>")
  
  
  file_list <- list()
  
  for(i in 1:length(filing_html_tables)) {
    if(is.na(ends[i])) {
    } else {
      file_list[[i]] <- paste0("<body>", 
                               str_sub(filing_html_tables[i], 
                                       start = 0, 
                                       end = ends[i]))
    }
  }
  
  file_list <- Filter(function(x) !is.null(x),
                      file_list)
  
  lapply(file_list, xml2::read_html)
  
}

# convert html table to character for cleanup
# finance firms REALLY don't know how to use
# html tables properly
parse_html_table <- function(html_table) {
  html_nodes(html_table, "tr") %>% 
    html_text() %>% 
    paste0(collapse = "\n")
}

parse_filing_html <- function(html_filing) {
   tbls <- xml2::xml_find_all(html_filing, 
                              ".//table")

   lapply(tbls, parse_html_table) %>% 
     Filter(function(x) 
       str_count(x, 
                 "[0-9],[0-9]") > 0,
       .)
}


get_filing_text <- function(full_text_filing) {
  str_extract(full_text_filing,"<TABLE></TABLE>")
}

parse_filing_text <- function(filing) {
  tbls <- str_extract_all(filing, "<TABLE></TABLE>")
  
  # grab tables with number,number
  holdings_tbls <- 
    Filter(function(x) str_count(paste0(x,collapse = ""), "[0-9],[0-9]") > 5,
           tbls)
  
  holdings_tbls
}

parse_filing <- function(filing) {
  do.call(paste0("parse_filing_", guess_text_html(filing)), list(filing = filing))
}


# list of all filings in a given year 
list_filings <- function(year) {
  list.files(paste0("data/processed/", year), 
             full.names = T, 
             recursive = T, 
             pattern = "crawler.csv")
}

guess_text_html <- function(filing) {
  if(grepl("<tr>", filing) & grepl("</tr>", filing)) {
    "html"
  } else {
    "text"
  }
}

# specific functions for fixing common finance stuff
fix_dollar_placement <- function(x) {
  str_replace_all(x, "(?<=\\$)(.*?)(?=[0-9])", "")
}

fix_percent_placement <- function(x) {
  str_replace_all(x, "(?<=[0-9])(.[ \t]+?)(?=%)", "")
}

fix_extra_spaces_in_parens <- function(x) {
  str_replace_all(x, "[ ]+(?=[^()]*\\))", " ")
}

strange_characters_to_spaces <- function(x) {
  str_replace_all(x, "[\\.]{2,}","\t") %>% 
    str_replace_all("[^A-Za-z0-9,\\.%\\$\t\n\"\'&\\(\\) \\/]"," ")
}

fix_separators <- function(x) {
  str_replace_all(x, "(?<=[A-Za-z&\\.0-9])([ -]{1,3}?)(?=[A-Za-z])"," ") %>% 
    str_replace_all("[ ]{2,}","\t") %>% 
    str_replace_all("[\t]{2,}", "\t")
}


read_char_table <- function(x){
  fread(text = x, 
        header = F, 
        fill = T,
        verbose = F,
        sep = "\t")
}

check_col_format <- function(df) {
  sapply(df, check_format)
}


parse_tables <- function(x) {
  data <- fix_dollar_placement(x) %>% 
    fix_percent_placement %>% 
    fix_extra_spaces_in_parens %>% 
    strange_characters_to_spaces %>%
    fix_separators %>%
    read_char_table %>% 
    replace_empty %>% 
    remove_na_cols()
  
  # grab just the complete data
  data[complete.cases(data)]
  
}

get_majority_format <- function(df_list) {
  lapply(df_list, check_col_format) %>% 
    sapply(paste0, collapse = ", ") %>% 
    table %>% 
    sort %>% 
    tail(1) %>% 
    names
}

get_relevant_tables <- function(df_list) {
  format <- get_majority_format(df_list)
  
  Filter(function(x) paste0(check_col_format(x), collapse = ", ") == format, df_list)
}

