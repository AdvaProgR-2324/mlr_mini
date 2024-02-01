######### Write a function for the dataset and test that on the car data #########

Dataset <- function(dat, target, type)
{
  
  #Subset dataset so it can be splitted into features and target . It is useful to implement the [-function for this dataset for subsetting.
  dat <-  subset(dat, select=sapply(dat, is.numeric))
  print(dat)
  target <- strsplit(as.character(dat), " ")
  print(target)
  
  #Dataset is an object 
  # it has as arguments the data, target and 
  #type of task ( regression vs classification ) + name
  
 # switch(mode(type),  regression = "regr", classification = "class")
  
}
ca <- Dataset(cars, "dist")
print(ca)
?mtcars

