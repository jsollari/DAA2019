#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     07.07.2017
#modificado: 06.12.2019

fnam <- "../images/svm_10.tiff"
tiff(file=fnam,units="in",width=3.5,height=3,res=300,compression="lzw")
par(mgp=c(2,0.75,0),mar=c(3.1,3.1,0.2,0.2))
x <- seq(-1,1,len=100)
y <- (3/4)*(1-x^2)
plot(x,y,type="l",col=2,lwd=2,xlim=c(-1.5,1.5),xlab="distance",ylab="weights")
lines(c(-1,-2),c(0,0),col=2,lwd=2)
lines(c(1,2),c(0,0),col=2,lwd=2)
dev.off()