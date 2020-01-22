# text helper functions
# specific functions for fixing common finance stuff

# fixes when there are large spaces or tabs between the number and the
# dollar sign beforehand (fairly common)
fix_dollar_placement <- function(x) {
  str_replace_all(x, "(?<=\\$)(.*?)(?=[0-9])", "")
}

remove_dollar_signs <- function(x) {
  UseMethod("remove_dollar_signs")
}

remove_dollar_signs.default <- function(x){
  x
}

remove_dollar_signs.character <- function(x) {
  str_replace_all(x, fixed("$"), "")
}

remove_dollar_signs.data.table <- function(x) {
  x[,lapply(.SD, remove_dollar_signs), .SDcols = colnames(x)]
}

# checks to see if x could be read in as a table
# this process converts separators to tabs
check_if_table <- function(x) {
  if(length(x) <= 1) {
    if(!is.na(x) & str_detect(x, "\t")) {
      T
    } else {
      F
    }
  } else {
    T
  }
}

# fixes when there are large spaces or tabs between the number and the
# percent sign afterwards (fairly common)
fix_percent_placement <- function(x) {
  str_replace_all(x, "(?<=[0-9])(.[ \t]+?)(?=%)", "")
}

# if there are duplicate spaces inside parens
# they defintely aren't meant to be in different columns until the 
# parents close
# this fixes that
fix_extra_spaces_in_parens <- function(x) {
  str_replace_all(x, "[ \n\t]+(?=[^()]*\\))", " ")
}

fix_vertical_separators <- function(x) {
  if(str_count(x, "\n[\n\t ]+\n") > 10) {
    str_replace_all(x, "[\n\t ]+\n", "\n")
  }
}

# lots of filings use this unicode control character instead of dashes...
fix_u0096 <- function(x) {
  # endash
  x <- str_replace_all(x, "\u0096", "-")
  
  # emdash
  x <- str_replace_all(x, "[-]+", "-")
  
  x
}

# attempt to remove characters outside of a typical range of
# financial reporting characters (e.g. odd unicode, euro marks)
# other stuff that can really break the parser
strange_characters_to_spaces <- function(x) {
  str_replace_all(x, "[\\.]{2,}","\t") %>% 
    str_replace_all("[^A-Za-z0-9,\\.%\\$\t\n\"\'&\\(\\) \\/-]"," ")
}

# regex to replace between 1 and 3 spaces or dashes with spaces
# then 2 or more spaces with tabs
# then duplicate tabs with a single tab
fix_separators <- function(x) {
  str_replace_all(x, "(?<=[A-Za-z&\\.0-9])([ -]{1,3}?)(?=[A-Za-z])"," ") %>% 
    str_replace_all("[ ]{2,}","\t") %>% 
    str_replace_all("[\t ]{2,}", "\t") %>% 
    str_replace_all("[\n\t  ]{2,}", "\n")
}

# read the text as a table
# using tabs as the delimiter
read_char_table <- function(x){
  if(check_if_table(x)) {

    x %>% 
      str_split(pattern = "\t", simplify = T) %>% 
      as.data.table 
    
  } else {
    data.table()
  }
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

