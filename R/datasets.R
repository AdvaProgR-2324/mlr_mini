######### Write a function for the dataset and test that on the car data #########

Dataset <- function(data, target, type)
{
  Dataset$new(data, target, type)
  #Dataset is an object 
  # it has as arguments the data, target and type of task ( regression vs classification ) + name
  
  #split dataset into target and features 
  subset(data, select = target)
}
Dataset()