#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.2.3
#criado:     10.07.2017
#modificado: 06.12.2019

set.seed(12345)
n <- 15
x <- runif(n)
k0 <- 1.5
k1 <- 2
y <- k0 + k1*x + rnorm(n,sd=0.1)
mod1 <- lm(y~1)
mod2 <- lm(y~x)
mod3 <- lm(y~poly(x,5)) 

fnam <- "../images/concept_2.tiff"
tiff(file=fnam,units="in",width=9,height=3,res=300,compression="lzw")
par(mfrow=c(1,3),mgp=c(2,0.75,0),mar=c(3.1,3.1,0.2,0.2))
plot(x,y,xlim=c(0,1))
abline(mod1,lwd=2,col=2)
text(0.1,3.2,"y ~ 1",pos=4,cex=1.5,col=2)
plot(x,y,xlim=c(0,1))
abline(mod2,lwd=2,col=2)
text(0.1,3.2,"y ~ x + 1",pos=4,cex=1.5,col=2)
plot(x,y,xlim=c(0,1))
xpred <- seq(0,1,len=100)
ypred <- predict(mod3,data.frame(x=xpred))
lines(xpred,ypred,lwd=2,col=2)
text(0.1,3.2,"y ~ poly(x,5) + 1",pos=4,cex=1.5,col=2)
dev.off()
