# gets the R data type to save files with

get_data_type <- function(path) {
  
  path_hdr <- gsub(x = path, pattern = ".dat", replacement = ".hdr")
  
  data_type_envi = read_lines(path_hdr)[9:10] %>%
    str_subset("data type") %>%
    str_split(" = ") %>%
    .[[1]] %>%
    .[2]
  
  data.codes <- tibble(envi.code = c("1","2","4","12"),
                           r.code = c("INT1U", "INT2S", "FLT4S", "INT2U"))
  
  data_code_r <- data.codes %>%
    filter(envi.code == data_type_envi) %>%
    pull(r.code)
  
  return(data_code_r)
}

