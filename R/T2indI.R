T2indI <-
function(estat,m){
t2<-vector()
for (i in 1:m)
{T2<-(t(estat[[3]][i,])%*%solve(estat[[2]])%*%(estat[[3]][i,]))
t2<-c(t2,T2)}
return(t2)
}

