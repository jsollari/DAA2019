#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.2.3
#criado:     08.02.2019
#modificado: 09.12.2019

#1. FUNCTIONS
source("misc_v2.3.r")

#2. ANALYSE DATA
set.seed(12345)

#load best model
f1 = "../results/lmktattract_2/modsel.RDS"
load(file=f1)
decay = 1e-4   #parameter for weight decay
maxit = 1000   #maximum number of iterations
abstol = 1e-6  #absolute fit criterion
reltol = 1e-12 #relative fit criterion

#fit best model (nn size = 1)
size = 1       #number of units in the hidden layer
f1 = "../results/lmktattract_3/fit_nnet1"
f2 = paste(f1,"_1.log",sep="")
f3 = paste(f1,"_2.log",sep="")
if(!file.exists(f2) | !file.exists(f3)){
    fit_nnet_v2(mod1$formula,mod1$data,nn_size=size,nn_decay=decay,nn_maxit=maxit,
	  nn_abstol=abstol,nn_reltol=reltol,f1=f1,main="LMkt Mobility")
}

#fit best model (nn size = 3)
size = 3       #number of units in the hidden layer
f1 = "../results/lmktattract_3/fit_nnet3"
f2 = paste(f1,"_1.log",sep="")
f3 = paste(f1,"_2.log",sep="")
if(!file.exists(f2) | !file.exists(f3)){
    fit_nnet_v2(mod1$formula,mod1$data,nn_size=size,nn_decay=decay,nn_maxit=maxit,
	  nn_abstol=abstol,nn_reltol=reltol,f1=f1,main="LMkt Mobility")
}

#fit best model (nn size = 10)
size = 10       #number of units in the hidden layer
f1 = "../results/lmktattract_3/fit_nnet10"
f2 = paste(f1,"_1.log",sep="")
f3 = paste(f1,"_2.log",sep="")
if(!file.exists(f2) | !file.exists(f3)){
    fit_nnet_v2(mod1$formula,mod1$data,nn_size=size,nn_decay=decay,nn_maxit=maxit,
	  nn_abstol=abstol,nn_reltol=reltol,f1=f1,main="LMkt Mobility")
}
