######### Write a function for the dataset and test that on the car data #########

Dataset <- function(data, target, type)
{
  #split dataset into target and features 
  subset(data, select = target)
}