######### Writing some tests ##### 
testthat::equals(hyperparameter, labels = c("name", "type", "range") )
hyperparameter(
  learning_rate = c(0.001, 0.01, 0.1),
  batch_size = c(1, Inf),
  optimizer = c("adam", "sgd", "rmsprop"), z = factor(letters)
)