x <- 1
bb <- bbListNew[to_do,]
bboxList <- tibble()
i <- 1
tSize <- 0.05
rm(x, bb, bboxList, i, tSize)

splitBBdf <- function(bb, bboxList, tSize){
  
  for (x in 1:nrow(bb)) {
    
    bbTemp <- bb[x, ]
    
    cityTagString <- bbTemp$cityTag
    
    while (bbSize(bbTemp[1,]) > tSize) {
      
      for (i in 1:nrow(bbTemp)) {
        bbTemp <-
          bbTemp[i,] %>% 
          splitBB() %>% 
          dplyr::bind_rows(bbTemp, .)
      }
      
      bbTemp <- 
        bbTemp %>% 
        filter(is.na(cityTag)) %>% 
        mutate(cityTag = paste0(cityTagString, "_", row_number()))
    }
    bboxList <-
      bbTemp %>% 
      bind_rows(bboxList, .) 
  }
  return(bboxList)
}

test <- tibble()
test <- splitBBdf(bbListNew[to_do,], test, .05)
