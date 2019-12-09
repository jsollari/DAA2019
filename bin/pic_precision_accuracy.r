#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     10.07.2017
#modificado: 06.12.2019

set.seed(12345)
n <- 20
x <- rep(0,n)
y <- rep(0,n)
x1 <- x + rnorm(n,sd=0.2)
y1 <- y + rnorm(n,sd=0.2)
x2 <- x + rnorm(n,sd=0.5)
y2 <- y + rnorm(n,sd=0.5)
x3 <- x + rnorm(n,sd=0.2) - 1
y3 <- y + rnorm(n,sd=0.2)

fnam <- "../images/concept_1.tiff"
tiff(file=fnam,units="in",width=9,height=3,res=300,compression="lzw")
par(mfrow=c(1,3),mgp=c(2,0.75,0),mar=c(3.1,3.1,0.2,0.2))
plot(0,0,xlim=c(-2,2),ylim=c(-2,2),pch=16,cex=1.5,col=1,xlab="x",ylab="y")
lines(c(-2,0),c(0,0),lwd=1.5,lty=2,col=1)
lines(c(0,0),c(-2,0),lwd=1.5,lty=2,col=1)
points(x1,y1,col=2,cex=1.5)
text(-2,2,"High accuracy | High precision",pos=4,cex=1.5,col=2)
plot(0,0,xlim=c(-2,2),ylim=c(-2,2),pch=16,cex=1.5,col=1,xlab="x",ylab="y")
lines(c(-2,0),c(0,0),lwd=1.5,lty=2,col=1)
lines(c(0,0),c(-2,0),lwd=1.5,lty=2,col=1)
points(x2,y2,col=2,cex=1.5)
text(-2,2,"High accuracy | Low precision",pos=4,cex=1.5,col=2)
plot(0,0,xlim=c(-2,2),ylim=c(-2,2),pch=16,cex=1.5,col=1,xlab="x",ylab="y")
lines(c(-2,0),c(0,0),lwd=1.5,lty=2,col=1)
lines(c(0,0),c(-2,0),lwd=1.5,lty=2,col=1)
points(x3,y3,col=2,cex=1.5)
text(-2,2,"Low accuracy | High precision",pos=4,cex=1.5,col=2)
dev.off()
