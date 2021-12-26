# custom metrics function
require(tidyverse)
require(yardstick)

p_top_k_day = function(df_day, k, target, prediction) {
  "
  compute top-k precision for a given day
  warning: df_day assumed to span over only one unique day
            if multiple days are present call p_top_k
            df_day must be a data.frame object
  input:
    - df_day -> it must containt the true prediction (TX_FRAUD)
             -> it must contain the probability of fraud (P_HAT)
    - K -> number of transaction to consider
  output: |A_fraud| / |A| where |A| = k
  pseudocode:
    sort df_day by fraud probability
    extract top k transaction
    count fraud / k
  "
  top_k_day = df_day %>%
    slice_max(., n=k, order_by=.data[[prediction]], with_ties=FALSE)
  sum(top_k_day[, target]) / k
}

p_top_k = function(df, k, target, prediction, date) {
  "
  compute mean top_k_precision over multiple days
  input:
    - df -> it must containt the true prediction (TX_FRAUD)
             -> it must contain the probability of fraud (P_HAT)
    - K -> number of transaction to consider
  output:
    mean(p_top_k_day) grouping over day
  pseudocode:
    gropu by day
    compute p_top_k_day over different groups
    take the mean
  "
  res = df %>%
    mutate(day = as.Date(.data[[date]])) %>%
    group_by(day) %>%
    group_map(~ p_top_k_day(.x, k, target, prediction)) %>%
    unlist()
  
  mean(res)
}

card_p_top_k_day = function(df_day, k, target, prediction, customer_id) {
  "
  compute top-k precision for a given day considering unique card,
  instead of considering transaction we consider card as compromised or not
  in a given day
  
  warning: df_day assumed to span over only one unique day
            if multiple days are present call p_top_k
  input:
    - df_day -> it must containt the true prediction (TX_FRAUD)
             -> it must contain the probability of fraud (P_HAT)
    - K -> number of transaction to consider
  output: |A_fraud| / |A| where |A| = k
  pseudocode:
    group by costumer
    extarct transaction wiuth highest prob given costumer
    sort df_day by fraud probability
    extract top k transaction
    count fraud / k
  "
  res = df_day %>%
    group_by(.data[[customer_id]]) %>%
    slice_max(., n=1, order_by=.data[[prediction]], with_ties=FALSE) %>%
    ungroup() %>%
    slice_max(., n=k, order_by=.data[[prediction]], with_ties=FALSE) 
  #print(res)
  sum(res[, target]) / k
}


card_p_top_k = function(df, k, target, prediction, customer_id, date) {
  "
  compute tmean card_p_top-k precision  over multiple days
  input:
    - df -> it must containt the true prediction (TX_FRAUD)
             -> it must contain the probability of fraud (P_HAT)
    - K -> number of transaction to consider
  output:-
    mean(p_top_k_day) grouping over day
  pseudocode:
    gropu by day
    compute card_p_top_k over different groups
    take the mean
  "
  res = df %>%
    mutate(day = as.Date(.data[[date]])) %>%
    group_by(day) %>%
    group_map(~ card_p_top_k_day(.x, k, target, prediction, customer_id)) %>%
    unlist()
  
  mean(res)
}

mean_fraud = function(df, target, date) {
  df %>% mutate(day = as.Date(.data[[date]])) %>%
    group_by(day) %>%
    summarise(frauds = sum(.data[[target]])) %>%
    summarise(mean = mean(frauds)) %>% pull()
}

performance = function(df, k, target, target_factor, prediction, date, customer_id){
  "
  p_top_k and card need target value as numeric (1being fraud)
  ap and roc auc need factor target
  "
  data.frame(
    p_top_k = p_top_k(df, k, target, prediction, date),
    mean_frauds = mean_fraud(df, target, date),
    card_p_top_k = card_p_top_k(df, k, target, prediction, customer_id, date),
    average_precision = average_precision(df, !!target_factor, !!prediction, event_level="second")[[1, ".estimate"]],
    roc_auc = roc_auc(df, !!target_factor, !!prediction, event_level="second")[[1, ".estimate"]]
  )
}
