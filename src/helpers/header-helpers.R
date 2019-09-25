# help with titled and striped tables

# vector logical conditions

## check if it's a repeating vector (happens with html tables sometimes)
is_header_footer <- function(x) {
  (is_repeated(x) | most_na(x))
}

## check if it's not
is_main_data <- function(x) {
  !is_header_footer(x)
}

## check if it's mostly NA
most_na <- function(vec) {
  sum(is.na(vec))/length(vec) > 0.5
}

## check if it's a vector that repeats across rows
is_repeated <- function(vec) {
  vec <- subset(vec, !is.na(vec))
  if(length(vec) == 0) {
    T
  } else {
    dups <- duplicated(vec)
    if(any(dups)) {
      numdups <- sum(dups) + 1
    } else {
      numdups <- 0
    }
    numdups > 0
  }
}

## check if any rows match a condition
has_row <- function(df, condition) {
  any(apply(df, MARGIN = 1, condition))
}

## check which rows match a condition
which_row <- function(df, condition) {
  as.numeric(which(apply(df, MARGIN = 1, condition)))
}

# Now onto tables

get_header <- function(df) {
  
  header_end <- min(which_row(df, is_main_data))
  footer_start <- max(which_row(df, is_main_data))
  header_footer <- which_row(df, is_header_footer)
  
  header <- df[subset(header_footer, header_footer < header_end),]
  footer <- df[subset(header_footer, header_footer > header_end),]
  
  list(header, footer)
}

remove_header <- function(df) {
  start <- min(which_row(df,is_main_data))
  
  df <- clean_table(df[start:nrow(df),])
  
  df
}

get_main_data <- function(df) {
 
  df[which_row(df, is_main_data),]
}

fix_striped <- function(df) {
  data_rows <- which_row(df, is_main_data)
  above_rows <- setdiff(data_rows - 1, data_rows)
  above_rows <- above_rows[which(!is.na(above_rows))]
  
  for(i in 1:ncol(df)) {
    col <- colnames(df)[i]
    
    right_rows <- setdiff(above_rows, which(is.na(df[[i]])))
    
    this_col <- df[right_rows + 1,i, with = F]
    above_col <- df[right_rows,i, with = F]

    if(nrow(this_col) > 0 & nrow(above_col) > 0){
      # df[data_rows,i] <- paste0(above_col, this_col)
      if(check_format(df[data_rows,i, with = F][[1]]) == 
         check_format(above_col[[1]]) & 
         check_format(df[data_rows,i, with = F][[1]]) == "character"){
        
        for(j in 1:length(right_rows)){
          if(!is.na(above_col[j]) & !is.na(this_col[j])){
            df[right_rows[j] + 1,(col) := paste0(above_col[j], " ; ",
                                             this_col[j])] 
          } else if (!is.na(above_col[j]) & is.na(this_col[j])) {
            df[right_rows[j] + 1,(col) := above_col[j]]
          }

          
        }
      }
      
    }
  }
  df[]
  
}

make_names <- function(df) {
  setnames(df, as.character(df[1,]))
  df[!1,]
}

is_titled <- function(df) {
  if(has_row(df, is_repeated)) {
    df
  } else if(has_row(df, most_na)) {
    
  } else {
    
  }
}

