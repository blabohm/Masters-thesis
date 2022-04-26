getFreeSlots <- function(APIlink)
{
  try({
    suppressWarnings({
    nSlots <- APIlink %>%
      sub('interpreter', 'status', .) %>%
      httr::GET() %>%
      httr::content() %>%
      strsplit("\n") %>%
      unlist() %>%
      grep("available", ., value = TRUE) %>%
      substr(1,1) %>%
      as.numeric() %>%
      .[1]})
  }, silent = TRUE)
  if (exists("nSlots")) {if (!is.na(nSlots)) return(nSlots) else return(0)
    } else return(0)
}

