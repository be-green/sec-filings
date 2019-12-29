
string_to_na <- function(vec) {
  vec[which(vec == "NA")] <- NA
  vec
}

# parse single text table
# we kept the linebreaks from read in the lines
# unlike the html table parser
# so we use that to break up each line
parse_single_text_table <- function(single_table, fund_header = NULL) {
  
  tbl_list <- list()
  
  for(i in 1:length(single_table)) {
    tbl_list[[i]] <- 
      single_table[i] %>% 
        str_split("\r\n") %>%
        .[[1]] %>% 
        remove_dollar_signs %>% # might generalize this to currency later
        remove_firstchar_linebreaks() %>% 
        fix_u0096 %>% # lots of filings use this instead of regular dashes...
        strange_characters_to_spaces() %>% 
        spaces_to_tabs() %>% # sometimes there are 4 spaces and \n breaks instead of tabs
        breaks_to_tabs() %>% # lots of extra \n characters, even within a single table line
        remove_redundant_tabs() %>% 
        remove_redundant_spaces() %>% 
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
  
  if(length(tbl_list) == 1) {
    tbl_list[[1]]
  } else {
    tbl_list
  }
}

# loop through the tables and apply the parser
parse_all_text_tables <- function(filing, header) {
  
  # grab all tables in the html
  tbls <- str_extract_all(filing, "<table>(?s:.*?)</table>")
  
  # filter out tables without numbers
  lapply(tbls, 
         parse_single_text_table,
         fund_header = header$fund_data)
}

# get all tables from text filing
get_text_filing_tables <- function(filing, header) {
  
  # parse the tables, filter out the null ones and ones with no numbers
  lapply(filing, 
         parse_all_text_tables,
         header = header) %>% 
    unlist_until_table() %>% 
    Filter(function(x) 
      length(x) > 0,
      .)
}
