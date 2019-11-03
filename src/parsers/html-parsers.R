# html table parsing functions



# returns an xml2 object containing all of the html
# in the relevant filing
# there can be multiple filings per text document,
# and this list will contain all of them
get_filing_html <- function(filing) {
  
  # split the file based on beginning html body tags
  # we don't really need the html headers
  filing_html_list <- filing %>% 
    str_split(pattern = "<body", simplify = T) %>% 
    as.vector
  
  
  # subset those that don't close the body (sometimes a header or footer isn't html)
  filing_html_tables <- subset(filing_html_list, str_detect(filing_html_list,
                                                            "</body>"))
  
  # identify the locations of the ending character
  ends <- str_locate(filing_html_tables, "/body>")
  
  # loop through the html components and grab them
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
  
  # filter out nulls from the list
  file_list <- Filter(function(x) !is.null(x),
                      file_list)
  
  # read via an xml parser for extracting tables
  lapply(file_list, xml2::read_html)
  
}

# parse tables that use horizontal line breaks to demarcate
# rows rather than normal html table formats
find_hr_breaks <- function(tr_nodes) {
  if(length(html_nodes(tr_nodes, "hr")) > 0) {
    splits <- html_nodes(tr_nodes, "hr") 
    xml_text(splits) <- "SPLIT-ROWS-HERE"
  } else {
    return(tr_nodes)
  }
}

# convert html table to character for cleanup
# finance firms REALLY don't know how to use
# html tables properly
parse_single_html_table <- function(html_table) {
  html_nodes(html_table, "tr") %>% 
    html_text() %>% 
    paste0(collapse = "\n")
}

# loop through the tables and apply the parser
parse_all_html_tables <- function(html_filing) {
  
  # grab all tables in the html
  tbls <- xml2::xml_find_all(html_filing, 
                             ".//table")
  
  # filter out tables without numbers
  lapply(tbls, parse_single_html_table) %>% 
    Filter(function(x) 
      str_count(x, 
                "[0-9],[0-9]") > 0 & !is.null(x),
      .)
}

# parse an html filing
parse_filing_html <- function(html_filing) {
  
  html_filings <- get_filing_html(html_filing)
  
  # parse the tables, filter out the null ones and ones with no numbers
  lapply(html_filings, parse_all_html_tables) %>% 
    Filter(function(x) 
        length(x) > 0,
      .)
}
