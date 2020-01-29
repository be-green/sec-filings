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
  filing_html_tables <- subset(filing_html_list, 
                               str_detect(filing_html_list,
                                          "</body>"))
  
  if(length(filing_html_tables) == 0){
    tryCatch({
      tmp <- list(xml2::read_html(filing))
      return(tmp)
      }, 
      error = function(e) {
      "Can't locate <body> tag in html filing"
    })
  }
  
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
  lapply(file_list, xml2::read_html, options = "HUGE")
  
}

get_html_filing_tables <- function(filing) {
  
  # get header data from filing
  header <- get_sec_header(filing)
  
  # funds <- header$fund_data
  # fundids <- funds$`series-id` %>% unique
  
  # pulls out the relevant html
  filing_html <- get_filing_html(filing)
  
  # pulls out the tables in the html, converts to text
  # for cleanup
  filing_html_tables <- lapply(filing_html, 
                               parse_filing_html,
                               html_header = header) %>% 
    unlist_until_table %>% # removes lists of lists
    Filter(x = ., function(x) length(x) > 0) # get rid of empty tables
  
  filing_html_tables
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

unicode_to_tabs <- function(x) {
  str_replace_all(x, "\\u[0-9]{4}","\t")
}

spaces_to_tabs <- function(x) {
  str_replace_all(x, "[ \n]{4,}", "\t")
}

breaks_to_tabs <- function(x) {
  str_replace_all(x, "\n", "\t")
}

remove_redundant_spaces <- function(x) {
  str_replace_all(x, "[ ]+", " ")
}

remove_redundant_tabs <- function(x) {
  str_replace_all(x, "[\t]+", "\t")
}

remove_firstchar_linebreaks <- function(x) {
  str_replace_all(x, "^\n","")
}

# custom xml_text function for table cells

html_text <- function(x, trim = TRUE) {
  xml_text(x, trim = trim)
}

xml_text <- function(x, trim = TRUE) {
  UseMethod("xml_text")
}

xml_text.xml_nodeset <- function(x, trim = TRUE) {
  vapply(x, xml_text, trim = trim, FUN.VALUE = character(1))
}

xml_text.xml_node <- function (x, trim = TRUE) 
{
  res <- xml2:::node_text(x$node)
  if (isTRUE(trim)) {
    res <- str_replace_all(res, "[ \n\r\t]+", " ")
  }
  res
}

# reduced version of html_table
# to allow custom post-processing of the strings
get_table_data <- function (x, trim = TRUE) {
  stopifnot(html_name(x) == "table")
  rows <- html_nodes(x, "tr")
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")

  values <- lapply(cells, html_text, trim = trim) %>% 
    lapply(str_trim) %>% 
    lapply(replace_empty) %>% 
    lapply(remove_dollar_signs) %>%  # often in their own cell, messes up parser
    lapply(function(x) subset(x, !is.na(x))) %>% 
    lapply(function(x) paste0(x, collapse = "\t")) %>% 
    unlist(recursive = F)
  
  values
}

string_to_na <- function(vec) {
  vec[which(vec == "NA")] <- NA
  vec
}


# convert html table to character for cleanup
# finance firms REALLY don't know how to use
# html tables properly
parse_single_html_table <- function(single_table, fund_header = NULL) {
  
  # for future use
  if(!is.null(fund_header)) {
    
  }
  
  invisible(xml_replace(xml_find_all(single_table, ".//br"), "p>\n</p"))
  
  get_table_data(single_table) %>% 
    remove_firstchar_linebreaks() %>% 
    fix_u0096 %>% # lots of filings use this instead of regular dashes...
    strange_characters_to_spaces() %>% 
    spaces_to_tabs() %>% # sometimes there are 4 spaces and \n breaks instead of tabs
    breaks_to_tabs() %>% # lots of extra \n characters, even within a single table line
    remove_redundant_tabs() %>% 
    remove_redundant_spaces() %>% 
    remove_dollar_signs %>% # might generalize this to currency later
    fix_extra_spaces_in_parens %>% 
    fix_percent_placement %>% 
    fix_extra_spaces_in_parens %>% 
    strange_characters_to_spaces %>%
    read_char_table %>% 
    replace_empty %>% 
    unique %>% 
    .[,lapply(.SD, string_to_na), .SDcols = colnames(.)] %>% 
    remove_na_cols() %>% 
    .[complete.cases(.)]
}

# loop through the tables and apply the parser
parse_all_html_tables <- function(html_filing, html_header) {

  # grab all tables in the html
  tbls <- xml2::xml_find_all(html_filing, 
                             ".//table")
  
  # filter out tables without numbers
  lapply(tbls, 
         parse_single_html_table,
         fund_header = html_header$fund_data) %>% 
    Filter(function(x) 
      suppressWarnings({
        
            any(
              sapply(
                x,
                function(x)
                  str_count(x, 
                "[0-9],[0-9]")
                )> 0) & !is.null(x)
        }),
      .)
}

# parse an html filing
parse_filing_html <- function(html_filing, html_header) {
  
  parse_all_html_tables(html_filing, 
                        html_header = html_header) %>% 
    Filter(function(x) 
        length(x) > 0,
      .)
}
