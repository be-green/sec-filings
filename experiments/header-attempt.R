# gets the raw filing text
filing <- get_filing(test_cases[5])

# get header data from filing
header <- get_sec_header(filing)

if(!is.null(header$fund_data)) {
  funds <- header$fund_data
  fund_list <- split(funds, by = "series_id")
  fund_names <- unique(funds$series_name)
}

filing_html <- get_filing_html(filing)


# pulls out the relevant html
filing_html <- get_filing_html(filing)

# pulls out the tables in the html, converts to text
# for cleanup
filing_html_tables <- lapply(filing_html, parse_filing_html) %>% 
  Filter(x = ., function(x) length(x) > 0)

# dispatch relevant parser
parsed_tables <- lapply(filing_html_tables, combine_all_tables_from_filing)

