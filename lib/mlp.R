#Multi-layer Perceptron
#Introduction on the basics of MLP:
#http://scikit-learn.org/stable/modules/neural_networks_supervised.html
#2 things about MLP:
#MLP requires tuning a number of hyperparameters such as the number of hidden neurons, layers, and iterations.
#MLP is sensitive to feature scaling.

########################################################################################
## (1)Scaling your data ##
#Multi-layer Perceptron is sensitive to feature scaling, 
#so it is highly recommended to scale your data. !remember to scale your test data to get meaningful result!
#For example, scale each attribute on the input vector X to [0, 1] or [-1, +1], 
#or standardize it to have mean 0 and variance 1. 
Train = read.csv("../data/Train.csv",as.is = T,header = T)
Test = read.csv("../data/test_final.csv",as.is = T,header = T)
########################################################################################
## (2)loading library ##

# R.version
# #if your R version is not 3.3.3(2017-03-06),update it first by running:
# install.packages("installr")
# installr::install.R()
# install.packages("RSNNS")

library(RSNNS)
########################################################################################
## (3)Train Multilayer Perceptron Classifier ##
stm = proc.time()
## scaling
Fea = Train[,-(ncol(Train)-1)]
maxs <- apply(Fea, 2, max) 
mins <- apply(Fea, 2, min)
scaled_Fea <- as.data.frame(scale(Fea, center = mins, scale = maxs - mins))
y = Train[,(ncol(Train)-1)]

MLP = mlp(scaled_Fea, y, 
          size = c(27), maxit = 30,
          initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
          learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
          updateFunc = "Topological_Order", updateFuncParams = c(0),
          hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
etm = proc.time()-stm
########################################################################################
## (4)Accuracy and time##
predictions_train <- predict(MLP,scaled_Fea)
predictions_train = ifelse(predictions_train>0.5,1,0)
TrainAccu = (1-mean(predictions_train != y))*100
################################
Fea_test = Test[,-(ncol(Test)-1)]
maxs <- apply(Fea, 2, max) 
mins <- apply(Fea, 2, min)
scaled_Fea <- as.data.frame(scale(Fea_test, center = mins, scale = maxs - mins))
y_test = Test[,(ncol(Test)-1)]
predictions <- predict(MLP,Fea_test)
predictions = ifelse(predictions>0.5,1,0)
TestAccu = (1-mean(predictions!=y_test))*100
cat(paste("It takes",etm[3],"sec to train.\n",
          "Accuracy on the Train set: ",TrainAccu,"%\n",
          "Accuracy on the Test set: ",TestAccu,"%\n"))