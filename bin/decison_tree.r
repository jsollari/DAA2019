#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     02.07.2017
#modificado: 06.12.2019

library("rpart")

trainset <- read.csv("../data/golf.csv",header=TRUE)
testset <- read.csv("../data/golf-testset.csv",header=TRUE)

#Perform Decision Trees fit
mod <- rpart(Play ~.,                  #model design
             trainset,                 #training set
             method="class",           #Categorical Tree
             parms=list(split="gini"), #Metric "Gini Impurity"
             control=rpart.control(
               minsplit=4,             #minimal size for split   
               minbucket=2,            #minimal leaf size
               cp=0.1,                 #minimal gain
               maxdepth=20))           #maximal depth

#Plot model
plot(mod,uniform=TRUE,branch=0.5,margin=0.05,main="Classification Tree")
text(mod,use.n=TRUE,all=TRUE,cex=0.8,fancy=TRUE,pretty=0)

#Test model
res <- predict(mod,                    #model
               testset,                #testing set
               type="class")           #returns classification/probabilities

#Confusion Matrix
conf_mat <- table(testset[,"Play"],res)
conf_mat

#Error rate
err_rt <- 100*(sum(conf_mat) - sum(diag(conf_mat)))/sum(conf_mat)
err_rt
