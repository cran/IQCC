dataindI <-
function(m,mu,Sigma,p){
u<-matrix(nrow=m,ncol=p)
n=1;
library(MASS)
for(i in 1:m){
N<-mvrnorm(n,mu,Sigma)
u[i,]<-N
}
print(u)
}

