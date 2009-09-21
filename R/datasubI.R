datasubI <-
function(m,n,mu,Sigma,p){
u<-array(dim=c(n,p,m))
#library(MASS)
for(i in 1:m){N<-mvrnorm(n,mu,Sigma)
u[,,i]<-N 
}
print(u)
}

