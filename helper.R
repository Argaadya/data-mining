
evaluate_model <- function(data){
  
  data.frame(Accuracy = accuracy_vec(data$truth, data$prediction),
             Recall = sens_vec(data$truth, data$prediction),
             Precision = precision_vec(data$truth, data$prediction),
             check.names = F
  )
}


profit_class <- function(percent){
  
  select_perc <- nrow(data_test) * percent
  
  profit_rf <- df_prediction_rf %>% 
    arrange(-Yes) %>% 
    rownames_to_column("id") %>% 
    mutate(
      id = as.numeric(id),
      prediction = ifelse(id <= round(select_perc), "Yes", "No"),
      benefit = case_when(prediction == "Yes" & truth == "Yes" ~ b_pos, # true positive
                          prediction == "No" & truth == "No" ~ b_neg, # true negative
                          prediction == "Yes" & truth == "No" ~ c_pos, # false positive
                          prediction == "No" & truth == "Yes" ~ c_neg, # false negative
      )
    ) %>% 
    pull(benefit) %>% 
    sum()
  
  profit_tree <- df_prediction_tree %>% 
    arrange(-Yes) %>% 
    rownames_to_column("id") %>% 
    mutate(
      id = as.numeric(id),
      prediction = ifelse(id <= round(select_perc), "Yes", "No"),
      benefit = case_when(prediction == "Yes" & truth == "Yes" ~ b_pos, # true positive
                          prediction == "No" & truth == "No" ~ b_neg, # true negative
                          prediction == "Yes" & truth == "No" ~ c_pos, # false positive
                          prediction == "No" & truth == "Yes" ~ c_neg, # false negative
      )
    ) %>% 
    pull(benefit) %>% 
    sum()
  
  data.frame("Random Forest" = profit_rf,
             "Decision Tree" = profit_tree, 
             check.names = F)
}