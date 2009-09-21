dataindII <-
function(estat,p,delta=0)
{n=1;
N<-mvrnorm(n, c(estat[[1]])+delta, matrix( c( estat[[2]]),2,2))}

