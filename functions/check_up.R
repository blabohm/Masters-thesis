file_list <- bbList$cityTag
out_dir <- out_path 

#get files that have to be downloaded
check.up <- function(file_list,
                     out_dir,
                     df_out = F) {
  
  # check if there are files in out dir
  if (
    list.files(out_dir) %>% 
    purrr::is_empty(.)
  ) return(1:length(file_list))
  
  n <-
    file_list %>% 
    #    namify() %>% 
    as_tibble() %>% 
    mutate(key = "x",
           priority = row_number())
  
  m <-
    out_dir %>% 
    list.files() %>% 
    namify0() %>% 
    as_tibble() %>% 
    mutate(key = "x")
  
  df <-
    merge(n, m, by = "value", 
          all.x = TRUE) %>%
    transmute(interval = value,
              file_list = key.x,
              joined_files = key.y,
              priority = priority) %>% 
    arrange(priority) %>% 
    select(-priority)
  
  
  if (
    df_out == T
  ) return(
    df
  ) else if (
    anyNA(df$joined_files)
  ) return(
    which(df$joined_files %>% is.na())
  ) else message(
    "All fine! :)"
  )
  
}
