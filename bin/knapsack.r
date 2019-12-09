#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.2.3
#criado:     02.07.2017
#modificado: 11.02.2019

library("GA")

#Define data 
n <- c("pocketknife","beans","potatoes","unions","sleepingbag","rope","compass")                # Items 
p <- c(10,20,15,2,30,10,30) # Profits 
w <- c(1,5,10,1,7,5,1)      # Weights
W <- 20                     # Knapsack ’s capacity 

#Define fitness function 
knapsack <- function(x){ 
    if(sum(x*w) > W)
	    return(0)
    else
		return(sum(x*p))
}

# Run SGA
SGA <- ga(type="binary", 
  fitness=knapsack,         #fitness function
  nBits=length(n),          #chromosome length
  popSize=100,              #population size
  pcrossover=0.8,           #crossover rate
  pmutation=0.1,            #mutation rate
  elitism=5,                #number of best individuals sure to be selected
  maxiter=100,              #number of generations
  names=n,                  #name of "genes"
  seed=12345)               #random seed

res <- SGA@solution
print(res)                  #Best solution
sum(res)                    #Total number of selected items
sum(res*p)                  #Total profit of selected items
sum(res*w)                  #Total weight of selected items

#Plot model
plot(SGA@summary[,"mean"],type="l",ylab="mean fitness",xlab="iteration",
  col="blue",lty=1,lwd=2)
barplot(SGA@fitness,ylab="fitness",xlab="",col="blue")
