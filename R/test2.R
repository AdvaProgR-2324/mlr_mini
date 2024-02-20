n <- 50
folds <- 5
repeats <- 1
result <- list()

set_training <- function() {
  indices <- sample(seq_len(n))
  result <- split(indices, cut(indices, breaks=folds, labels=FALSE))
  setNames(result, rep("training", folds))
  
}

result <- list()
for (rep_idx in seq(repeats)) {
  rep()
}



rep(set_training(), 2)

indices <- sample(seq_len(n))
result <- split(indices, cut(indices, breaks=folds, labels=FALSE))
setNames(result, rep("training", folds))
