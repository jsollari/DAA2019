#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     02.07.2017
#modificado: 09.12.2019

library("e1071")

#Create testing set by hold out
set.seed(12345)
n <- nrow(iris)
n_test <- floor(n/3)
sampindex <- sample(1:n,size=n_test,replace=FALSE)
testset <- iris[sampindex,]
trainset <- iris[-sampindex,]

#Perform Naive Bayes fit
mod <- naiveBayes(Species ~ .,          #model design
                  trainset,             #training set    
                  laplace=1)            #Laplace correction

#Test model
res <- predict(mod,                     #model
               testset)                 #testing set

#Confusion Matrix
conf_mat <- table(testset[,"Species"],res)
conf_mat

#Error rate
err_rt <- 100*(sum(conf_mat) - sum(diag(conf_mat)))/sum(conf_mat)
err_rt
