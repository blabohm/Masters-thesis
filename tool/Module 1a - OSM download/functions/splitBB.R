
bb2df <- 
  function(BBox){
    data.frame(
      "xmin" = BBox[1, 1],   
      "xmax" = BBox[1, 2],
      "ymin" = BBox[2, 1],
      "ymax" = BBox[2, 2])
  }

df2bb <- 
  function(BBox_df){
    matrix(BBox_df %>% unlist(), 
           nrow = 2, ncol = 2, byrow = T,
           dimnames = list(c('x', 'y'), c('min', 'max'))
    )}

splitBB <- 
  function(BBox){
    
    if (dim(BBox)[1] == 1) {
      BBox <-
        BBox %>% 
        select(xmin:ymax) %>% 
        df2bb
    }
    # get length of bbox edges
    a <- BBox[1, 2] - BBox[1, 1]
    b <- BBox[2, 2] - BBox[2, 1]
    
    # create min, max and mid points for x and y
    xmin <- BBox[1, 1]   
    xmax <- BBox[1, 2]
    ymin <- BBox[2, 1]
    ymax <- BBox[2, 2]
    xmid <- BBox[1, 1] + (a / 2)   
    ymid <- BBox[2, 1] + (b / 2)   
    
    
    # if too big: split bbox into 4 equal parts
    bbDf <-
      data.frame('cityTag' = paste0(cityTag, 
                                    c('_a', '_b', '_c', '_d')),
                 'xmin' = c(xmin, xmid, xmin, xmid),    # xmin
                 'xmax' = c(xmid, xmax, xmid, xmax),    # xmax
                 'ymin' = c(ymid, ymid, ymin, ymin),    # ymin
                 'ymax' = c(ymax, ymax, ymid, ymid)) %>%# ymax
      return()
  }
