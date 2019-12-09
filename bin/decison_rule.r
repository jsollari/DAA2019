#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     06.12.2019
#modificado: 09.12.2019

library("OneR")

trainset <- read.csv("../data/golf.csv",header=TRUE)
testset <- read.csv("../data/golf-testset.csv",header=TRUE)

#Discretize numerical variables
trainset <- bin(trainset,              #training set
                nbins=2)               #binning by intervals of equal length
testset <- bin(testset,                #training set
                nbins=2)               #binning by intervals of equal length

#Perform Decision Rule fit
mod <- OneR(Play ~.,                   #model design
            trainset)                  #training set

#Summarize model
summary(mod)

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
