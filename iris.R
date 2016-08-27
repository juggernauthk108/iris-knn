require(class)
#import the data of iris
iris = read.csv("iris.data",header=F)
n = c("sepal.length","sepal.width","petal.length","petal.width","species")
names(iris) = n

#shuffle the data
set.seed(9850)
gp <- runif(nrow(iris))
iris <- iris[order(gp),]

#normalize function
normalize <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize the coloumns except the species column
iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))

#take the training set
iris_train <- iris_n[1:129,]

#take the test set
iris_test <- iris_n[130:150,]

#take the target of the traning data set
iris_train_target <- iris[1:129 , 5]

#take the target of the test data set
iris_test_target <- iris[130:150 , 5]

#Working with kNN
valueOfk = floor(sqrt(nrow(iris))) #Ideal value of k is sqrt of no of obv

mykNN <- knn(train = iris_train , test = iris_test , cl= iris_train_target , k = valueOfk)

confTable = table(mykNN , iris_test_target , dnn = list("predicted","actual"))
confTable
