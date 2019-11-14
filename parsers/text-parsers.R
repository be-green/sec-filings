# text file parsers

get_filing_text <- function(full_text_filing) {
  str_extract(full_text_filing,"<TABLE></TABLE>")
}

# parse a text filing
# WIP, doesn't work yet
parse_filing_text <- function(filing) {
  tbls <- str_extract_all(filing, "<TABLE></TABLE>")
  
  # grab tables with number,number
  holdings_tbls <- 
    Filter(function(x) str_count(paste0(x,collapse = ""), "[0-9],[0-9]") > 5,
           tbls)
  
  holdings_tbls
}
