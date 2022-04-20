# get status of overpass API link
APIstatus <- 
  function(APIlink) {
    require(dplyr, quietly = T)
    status <-
      APIlink %>% 
      sub('interpreter', 'status', .) %>% 
      httr::GET() %>% 
      httr::http_status() %>%
      .$category
    
    return(status)
  }

# select a working API
getAPI <-
  function(url_list, waitTime) {
    # set n to 1 to check first position in API list
    n <- 1
    # retrieve first API link
    Link <- api_list[n]
    # check status
    status_check <- APIstatus(Link)
    
    # keep checking status of all API links until one is not busy anymore
    while (status_check != "Success") {
      
      # check next API link
      n <- n + 1
      # reset n to one if last element was reached and wait
      if (n >= length(url_list) + 1) {
        n <- 1
        status_check <- "Failure"
        
        Sys.sleep(waitTime)
        
      } else {
        
        Link <- api_list[n]
        
        status_check <- APIstatus(Link)
      }
    }
    # return working API link
    return(Link)
    
  }
