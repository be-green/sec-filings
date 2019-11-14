# text helper functions
# specific functions for fixing common finance stuff

# fixes when there are large spaces or tabs between the number and the
# dollar sign beforehand (fairly common)
fix_dollar_placement <- function(x) {
  str_replace_all(x, "(?<=\\$)(.*?)(?=[0-9])", "")
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
  fread(text = x, 
        header = F, 
        fill = T,
        verbose = F,
        sep = "\t")
}
