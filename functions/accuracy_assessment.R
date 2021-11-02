#############################################################################
# MSc Earth Observation Exercise Session 07
# Benjamin Labohm
#############################################################################

# create function to get accuracy assesment for map and reference data
acc.assess <- function(..., class_data, ref_data, 
                       class_names = c("ja", "nein"), out = NULL){

  # Load packages, use install.packages('packagename') to install if needed
  require(dplyr)

  in_df <- data.frame(class_data, ref_data)
  
  # create confusion matrix
  conf_matrix <- table(in_df$class_data,
                       in_df$ref_data) 
  
  if (dim(conf_matrix)[2] == 1) conf_matrix[, 2] <- 0
  
  # proportion belonging to class
  prob_conf <- mapply(function(i, n){
    data.frame(i  = conf_matrix[,n] / sum(conf_matrix[,n]))
    }, class_names, 1:length(class_names)) %>% bind_rows %>% as.data.frame
  
  names(prob_conf) <- class_names
  row.names(prob_conf) <- class_names

  # accuracy statistics
  acc_stats <- data.frame(
                          Producers_Accuracy  = lapply(1:(conf_matrix %>% diag %>% length),
                                                       function(i)
                                                       {conf_matrix[i,i] / sum(conf_matrix[,i]) * 100}
                          ) %>% unlist,
                          
                          Users_Accuracy      = lapply(1:(conf_matrix %>% diag %>% length),
                                                       function(i)
                                                       {conf_matrix[i,i] / sum(conf_matrix[i,]) * 100}
                          ) %>% unlist,
                          
                          Omission_Error      = lapply(1:(conf_matrix %>% diag %>% length),
                                                       function(i)
                                                       {(1 - conf_matrix[i,i] / sum(conf_matrix[,i])) * 100}
                          ) %>% unlist,
                          
                          Commission_Error    = lapply(1:(conf_matrix %>% diag %>% length),
                                                       function(i)
                                                       {(1 - conf_matrix[i,i] / sum(conf_matrix[i,])) * 100}
                          ) %>% unlist
  ) 
  row.names(acc_stats) <- class_names
  
  # overall accuracy
  overall_accuracy <- 
    conf_matrix %>% 
    diag %>%
    sum / sum(conf_matrix)
  
  # gather results
  results <- list(Confusion_matrix = conf_matrix, 
                  Class_probabilities = prob_conf, 
                  Accuracy_statistics = acc_stats, 
                  Overall_accuracy = overall_accuracy)
  
  if (is.null(out)) {
    return(results)
  } else if (out == "oa") {
      return(overall_accuracy)
  } else if (out == "prod") {
      return(acc_stats$Producers_Accuracy[1])
  } else if (out == "com") {
      return(acc_stats$Commission_Error[1])
    }
}

