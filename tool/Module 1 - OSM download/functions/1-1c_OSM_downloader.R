################################################################################
APIselect <- function(
  api_list = dplyr::tibble(interpreter =
                             c('http://overpass-api.de/api/interpreter',
                               'https://lz4.overpass-api.de/api/interpreter',
                               'https://z.overpass-api.de/api/interpreter')))
{
  require(dplyr, quietly = TRUE)
  #N <- nrow(api_list)
  api_list <- api_list %>%
    mutate(slots = sapply(interpreter, getFreeSlots)) %>%
    arrange(desc(slots))

  while (sum(api_list$slots, na.rm = T) < 1) {
    message("Waiting for API slot.")
    Sys.sleep(30)
    api_list <- api_list %>%
      mutate(slots = sapply(interpreter, getFreeSlots)) %>%
      arrange(desc(slots))
  }
  return(api_list$interpreter[1])
}

