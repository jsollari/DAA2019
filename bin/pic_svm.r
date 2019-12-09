#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     02.07.2017
#modificado: 06.12.2019

library("e1071")

##1. Linear SVM Classifier
#[adapted from https://www.datacamp.com/community/tutorials/support-vector-machines-r]
set.seed(12345)
n1 <- 10
y1 <- rnorm(n1,mean=0,sd=1)
x1 <- rnorm(n1,mean=0,sd=1)
n2 <- 10
y2 <- rnorm(n2,mean=0,sd=1) + 1
x2 <- rnorm(n2,mean=0,sd=1) + 1

dat1 <- data.frame(stringsAsFactors=FALSE,
  y=c(y1,y2),
  x=c(x1,x2),
  grp=c(rep("blue",n1),rep("red",n2)))

fnam <- "../images/svm_1.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,col=grp,pch=19))
dev.off()

modfit <- svm(grp ~ .,
              dat1,
              scale=FALSE,             #scaling stored for testing
              type="C-classification", #SVM algorithm
              kernel="linear",         #kernel type
              cost=1.0,                #C-constant (cost)
              tolerance=0.001)         #convergence epsilon

#beta_0 + beta_1*x + beta_2*y = 0
#y = -(beta_1/beta_2)*x - (beta_0/beta_2)
beta_0 = -modfit$rho
beta_1 = t(modfit$coefs)%*%dat1$x[modfit$index]
beta_2 = t(modfit$coefs)%*%dat1$y[modfit$index]

grid1 <- expand.grid(
  x = seq(min(dat1$x),max(dat1$x),length=100),
  y = seq(min(dat1$y),max(dat1$y),length=100))
grid1$grp <- as.character(predict(modfit,grid1))

fnam <- "../images/svm_2.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(grid1,plot(x,y,col=grp,pch=19,cex=0.2))
with(dat1,points(x,y,col=grp,pch=19))
abline(a=-beta_0/beta_2,b=-beta_1/beta_2)
dev.off()

##2. Non-linear SVM Classifier
set.seed(12345)
n1 <- 30
rad1 <- sqrt(runif(n1,1,2))
the1 <- runif(n1,0,2*pi)
x1 <- rad1*cos(the1)
y1 <- rad1*sin(the1)
n2 <- 30
rad2 <- sqrt(runif(n2,0,1))
the2 <- runif(n2,0,2*pi)
x2 <- rad2*cos(the2)
y2 <- rad2*sin(the2)

dat1 <- data.frame(stringsAsFactors=FALSE,
  x=c(x1,x2),
  y=c(y1,y2),
  grp=c(rep("blue",n1),rep("red",n2)))

fnam <- "../images/svm_3.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3.0,3.0,1.5,1.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,col=grp,pch=19))
dev.off()

dat1$z <- dat1$x^2 + dat1$y^2

fnam <- "../images/svm_4.tiff"
tiff(file=fnam,units="in",width=8,height=4,res=300,compression="lzw")
par(mfcol=c(1,2),mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,z,col=grp,pch=19))
with(dat1,plot(y,z,col=grp,pch=19))
dev.off()

fnam <- "../images/svm_5.tiff"
tiff(file=fnam,units="in",width=8,height=4,res=300,compression="lzw")
par(mfcol=c(1,2),mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,z,col=grp,pch=19))
abline(h=1)
with(dat1,plot(y,z,col=grp,pch=19))
abline(h=1)
dev.off()

fnam <- "../images/svm_6.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3.0,3.0,1.5,1.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,col=grp,pch=19))
radius <- 1
theta <- seq(0,2*pi,length=200)
lines(x=radius*cos(theta),y=radius*sin(theta))
dev.off()

modfit <- svm(grp ~ x + y,
              dat1,
              scale=FALSE,             #scaling stored for testing
              type="C-classification", #SVM algorithm
              kernel="radial",         #kernel type
              gamma=0.5,               #kernel gamma
              cost=1.0,                #C-constant (cost)
              tolerance=0.001,         #convergence epsilon
              epsilon=0.1)             #loss-function epsilon

grid1 <- expand.grid(
  x = seq(min(dat1$x),max(dat1$x),length=100),
  y = seq(min(dat1$y),max(dat1$y),length=100))
grid1$grp <- as.character(predict(modfit,grid1))

fnam <- "../images/svm_7.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3.0,3.0,1.5,1.5),mgp=c(1.7,0.7,0),cex=0.8)
with(grid1,plot(x,y,col=grp,pch=19,cex=0.2))
with(dat1,points(x,y,col=grp,pch=19))
radius <- 1
theta <- seq(0,2*pi,length=200)
lines(x=radius*cos(theta),y=radius*sin(theta))
dev.off()

##3. Linear SVM Regression

## 2. Model Fitting
set.seed(12345)

real_a1 <- 1.5
real_a2 <- 4
x <- runif(n=30,min=0,max=10)
y <- real_a1*x + real_a2 + rnorm(n=30,mean=0,sd=1)
dat1 <- data.frame(x,y)

fnam <- "../images/svm_7.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,pch=19))
dev.off()

modfit <- svm(y ~ x,
              dat1,
              scale=FALSE,           #scaling stored for testing
              type="eps-regression", #SVM algorithm
              kernel="linear",       #kernel type
              tolerance=0.001,       #convergence epsilon
              epsilon=0.5)           #loss-function epsilon

#y = beta_1*x + beta_0
beta_0 = -modfit$rho
beta_1 = t(modfit$coefs)%*%dat1$x[modfit$index]

fnam <- "../images/svm_8.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,pch=19))
abline(a=beta_0,b=beta_1,col="blue")
abline(a=beta_0 - 0.5,b=beta_1,lty=2,col="blue")
abline(a=beta_0 + 0.5,b=beta_1,lty=2,col="blue")
dev.off()

lmfit <- lm(y ~ x,data=dat1)
a1 <- lmfit$coef[1]
a2 <- lmfit$coef[2]

fnam <- "../images/svm_9.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
par(mar=c(3,2.5,1.5,0.5),mgp=c(1.7,0.7,0),cex=0.8)
with(dat1,plot(x,y,pch=19))
abline(a=a1,b=a2,lwd=2,col="red")
abline(a=beta_0,b=beta_1,col="blue")
abline(a=beta_0 - 0.5,b=beta_1,lty=2,col="blue")
abline(a=beta_0 + 0.5,b=beta_1,lty=2,col="blue")
dev.off()
