library(data.table)
library(stringr)

# returns an xml2 object containing all of the html
# in the relevant filing
# there can be multiple filings per text document,
# and this list will contain all of them
get_filing_html <- function(filing_text_link) {
  filing <- 
    readLines(filing_text_link) %>% 
      paste0(collapse = "") %>% 
      str_split(pattern = "<BODY>", simplify = T)
  
  ends <- str_locate(filing, "</BODY>")[,2]
  
  
  file_list <- list()
  
  for(i in 1:length(filing)) {
    if(is.na(ends[i])) {
    } else {
      file_list[[i]] <- paste0("<BODY>", 
                               str_sub(filing[i], start = 0, end = ends[i]))
    }
  }
  
  file_list <- Filter(function(x) !is.null(x),
                      file_list)
  
  lapply(file_list, xml2::read_html)
  
}

parse_filing_html <- function(filing) {
   tbls <- rvest::html_table(filing)
   
   # grab tables with number,number
   holdings_tbls <- 
     Filter(function(x) str_count(paste0(x,collapse = ""), "[0-9],[0-9]") > 5,
          tbls)
   
   holdings_tbls
}


# list of all filings in a given year 
list_filings <- function(year) {
  list.files(paste0("data/processed/", year), 
             full.names = T, 
             recursive = T, 
             pattern = "crawler.csv")
}


