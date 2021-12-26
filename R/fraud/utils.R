require(tidyverse)

prequential_cv = function(df, 
                          starting_date,
                          n_fold,
                          train_delta, delay_delta, test_delta
) {
  
  # compute time span needed
  time_span = test_delta * (n_fold - 1) + train_delta + delay_delta + test_delta
  
  print("time span in days")
  print(time_span)
  
  starting_date = as.Date(starting_date)
  folds = list()
  for (f in 1:n_fold) {
    idx_train = df[
      as.Date(df$TX_DATETIME) >= starting_date & as.Date(df$TX_DATETIME) < starting_date + train_delta,
      "TRANSACTION_ID"
    ]
    val_date = starting_date + train_delta + delay_delta
    val_idx = df[
      as.Date(df$TX_DATETIME) >= val_date & as.Date(df$TX_DATETIME) < val_date + test_delta,
      "TRANSACTION_ID"
    ]
    starting_date = starting_date + test_delta
    folds[[f]]= list(idx_train, val_idx)
  }
  return(list(folds, time_span))
}
