#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     10.07.2017
#modificado: 06.12.2019

#based on https://en.wikipedia.org/wiki/File:Local_maximum.png
f <- function(x,y){
    exp(1)^(-(x^2+y^2))+2*exp(1)^(-((x-1.5)^2+(y-1.5)^2))
}
x <- seq(-2,4,len=50)
y <- seq(-2,4,len=50)
z <- outer(x,y,f)

fnam <- "../images/concept_3.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
par(mar=c(0.2,0.2,0.2,0.2))
persp(x,y,z,phi=20,theta=30,d=5,expand=0.5,box=FALSE)
dev.off()
