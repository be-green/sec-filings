
# prob break | space
# prior = freq spaces
# posterior 
bayes_update <- function(pointwise_prior, pointwise_obs) {
  pointwise_prior
}

string_to_na <- function(vec) {
  vec[which(vec == "NA")] <- NA
  vec
}

guess_separator <- function(char_table) {
  if(str_detect(char_table, "[.]{4,}")) {
    
  }
}


identify_breakpoints <- function(x) {
  
  all_breakpoints <- list()
  
  for(i in 1:length(x)) {
    
    locations <- str_locate_all(x[i], " ")[[1]]
    start <- locations[,"start"]
    end <- locations[,"end"]
    
    if(nrow(locations) > 0) {
      
      temp <- matrix(ncol = 2, nrow = length(start) + 1)
      temp[,2] <- c(start, nchar(x[i]))
      temp[, 1] <- c(0,end)
      
      all_breakpoints[[i]] <- temp
      
    } else {
      all_breakpoints[[i]] <- NA
    }
  }
  
  c(all_breakpoints, max(sapply(x, nchar)))
  
}

get_modal_breakpoints <- function(all_breakpoints,
                                  freq = 0.1) {
  
  num_obs <- length(all_breakpoints)
  
  frequencies <- 
    unlist(all_breakpoints) %>% 
    subset(., . > 1) %>% 
    table %>% 
    divide_by(num_obs) %>% 
    subset(., . > freq) # arbitrary threshold for bre
  
  c(1, sort(as.integer(names(frequencies))))
  
}

# (5/nchar(x[10]))/(length(which(strsplit(x[10],"")[[1]] == " "))/nchar(x[10]))
# 
# 
space_freq <- sapply(fwf_pos, as.vector)
char_length <- lapply(x, nchar)

space_freq[[1]] <- NULL

tmp <- list()
for(i in 1:length(space_freq)) {
  tmp[[i]] <- ifelse(1:char_length[[i]] %in% space_freq[[i]],
         1, 0)
}

tmp <- tmp %>% 
  lapply(function(x) data.table(t(x))) %>% 
  rbindlist(fill = T)

# P(Break | Space) = P(Space | Break)*P(Break)/P(Space)

make_positions <- function(breakpoints) {
  
  start <- breakpoints[1:(length(breakpoints) - 1)]
  end <- breakpoints[2:length(breakpoints)]
  readr::fwf_positions(start, end, col_names = NULL)
}

num_spaces <- sum(stringr::str_count(x, " "))
num_characters <- sum(stringr::str_length(x))
percent_spaces <- num_spaces/num_characters

fwf_pos

read_as_fwf <- function(x) {
  
  x <- fix_u0096(x)
  fwf_pos <- x %>% 
    str_split("\r\n") %>%
    .[[1]] %>% 
    identify_breakpoints %>% 
    get_modal_breakpoints(freq = 0.2) %>% 
    make_positions
  

  suppressWarnings({
    
    readr::read_fwf(x, fwf_pos, ) %>% 
      as.data.table
    
  })  
}



# parse single text table
# we kept the linebreaks from read in the lines
# unlike the html table parser
# so we use that to break up each line
parse_single_text_table <- function(single_table, fund_header = NULL) {
  
  tbl_list <- list()
  
  for(i in 1:length(single_table)) {
    tbl_list[[i]] <- 
      single_table[i]  %>%
        read_as_fwf %>% 
        remove_dollar_signs %>%
        unique %>% 
        .[,lapply(.SD, string_to_na), .SDcols = colnames(.)] %>% 
        remove_na_cols()
    
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
