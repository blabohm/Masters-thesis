#############################################################################
# MSc Earth Observation Exercise Session 07
# Benjamin Labohm
#############################################################################

# create function to get accuracy assesment for map and reference data
acc.assess <- function(aa_data, class_names = c("yes", "no")){

  # Load packages, use install.packages('packagename') to install if needed
  require(dplyr)

  # create confusion matrix
  conf_matrix <- table(aa_data$reference,
                       aa_data$classification) 
  
  # proportion belonging to class
  prob_conf <- mapply(function(i, n){
    data.frame(i  = conf_matrix[,n] / sum(conf_matrix[n,]))
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
  return(results)
}

