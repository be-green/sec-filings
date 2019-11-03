# parsers for sec filing headers
# headers contain important information like
# CIK number of parent company
# share class and ticker of a fund


get_sec_header <- function(filing) {
  do.call(paste0("get_", guess_header_format(filing), "_series_data"),
          args = list(filing = filing))
}


# xbrl is not xml because it has opening tags with no closing
# tags. This fixes that issue (through brute force).
finish_xml <- function(xml_string, start_tag) {
  start_tag <- str_replace_all(start_tag,"[<>]","")
  extracts <- 
    str_extract_all(xml_string, pattern = paste0(start_tag, ".[^<]+")) %>% 
    .[[1]] %>% 
    unique
  
  for(i in 1:length(extracts)) {
    xml_string <- str_replace_all(xml_string, extracts[i], 
                                  paste0(extracts[i],"</",start_tag,">"))
    
  }
  
  xml_string
  
}

guess_header_format <- function(filing) {
  # grab header
  sec_header <- str_extract_all(filing, "<sec-header>.*</sec-header>")[[1]]
  
  if(str_detect(sec_header, "<series>")) {
    "xml"
  } else {
    "text"
  }
}

# get the sec header an automatically parse it as xml
# rather than the weird xbrl thing
get_xml_sec_header <- function(filing) {
  # grab header
  sec_header <- str_extract_all(filing, "<sec-header>.*</sec-header>")[[1]]
  
  # identify all the tags, and find the ones that aren't
  # properly closed
  xml_tags <- unique(str_extract_all(sec_header,"<[^/>]+>")[[1]])
  closed_versions <- str_replace_all(xml_tags, "<", "</")
  unclosed_tags <- subset(xml_tags, !str_detect(sec_header, closed_versions))
  
  # fix that issue in the xml for all ags
  for(i in 1:length(unclosed_tags)) {
    sec_header <- finish_xml(sec_header, unclosed_tags[i])
  }
  
  # remove rogue & that can break xml
  sec_header <- str_replace_all(sec_header, "&(?!#?[a-z0-9]+;)","&amp;")
  xml2::read_xml(sec_header)
}

get_text_sec_header <- function(filing) {
  # we first get rid of faux-xml tags
  # then we grab everything after company data
  # then we replace duplicate tabs
  # then we split everything into key-value pairs
  # based on the notion key:\tvalue\t
  
  sec_header <- str_extract_all(filing, "<sec-header>.*</sec-header>")[[1]] %>% 
    str_replace_all(., "<[^>]+>","") %>% 
    str_extract(.,"company data:.*") %>% 
    str_replace_all("[\t]+","\t") %>% 
    str_extract_all(., "[a-z0-9 ]+:\t[a-z0-9 ]+\t") %>% 
    .[[1]] %>% 
    str_split(":", simplify = T)
  
  sec_header[,1] <- str_replace_all(sec_header[,1],
                                    " ",
                                    "-")
  
  sec_header[,2] <- str_replace_all(sec_header[,2],
                                    "\t",
                                    " ") %>% 
    str_trim

  header_data <- data.table(t(sec_header[,2]))
  setnames(header_data, sec_header[,1])
  
  header_data[]
}

# older filings use a text-based format for headers
get_text_series_data <- function(filing) {
  list(parent_data = get_text_sec_header(filing))
}

get_xml_series_data <- function(filing) {
  sec_header <- get_xml_sec_header(filing)
  
  parse_series <- function(x) {
    x <- as_list(x)
    not_shareclass <- subset(names(x), names(x) != "class-contract")
    series_table <- x[not_shareclass] %>% unlist %>% t %>% as.data.table
    sc_table <- setdiff(x, x[not_shareclass]) %>% 
      lapply(function(x) unlist(x) %>% t %>% as.data.table) %>% 
      rbindlist(fill = T, use.names = T)
    data.table(series_table, sc_table)
  }
  
  series_data <- xml_find_all(sec_header, ".//series") %>% 
    lapply(parse_series) %>% 
    rbindlist(fill = T)
  
  parent_data <- get_text_sec_header(filing)
  
  list(parent_data = parent_data,
       fund_data = series_data)
}

# dispatch the relevant filing parser
parse_filing <- function(filing) {
  
  filing <- do.call(paste0("parse_filing_", guess_text_html(filing)), list(filing = filing))
  
  filing
}