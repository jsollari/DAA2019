#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     02.07.2017
#modificado: 09.12.2019

library("class")

trainset <- read.csv("../data/golf.csv",header=TRUE)
testset <- read.csv("../data/golf-testset.csv",header=TRUE)

#Test model
res <- knn(trainset[,c("Temperature","Humidity")], #attributes of training set
           testset[,c("Temperature","Humidity")],  #testing set
           trainset[,"Play"],                      #classes of trainset
           k=3,                                    #k-nearest neighbour
           prob=TRUE)                              #proportion of winning class

#Confusion Matrix
conf_mat <- table(testset[,"Play"],res)
conf_mat

#Error rate
err_rt <- 100*(sum(conf_mat) - sum(diag(conf_mat)))/sum(conf_mat)
err_rt
