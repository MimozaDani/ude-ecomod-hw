## Assignment 1 ##
## Author: Mimoza Dani
## Date: 01.12.2022

## Mice model ##
d<-.7
b<-3
m<-4
Nt<-42

mice<- function(Nt, b, d, m){
  Nt1<-(1+b)*(1-d)*Nt+m
  return(Nt1)
}

mice(Nt, b, d, m)
N<- Nt
N

for(i in 1:100){
  Nt1<-mice(Nt, b, d, m)
  N<-c(N, Nt1)
  Nt<-Nt1
}

N
plot(N)
plot(N,xlab="time",ylab="N_mice",pch=19,col="black")




