alfa <-
function(n){
d2<-function(n){
d<-integrate(function(w){1-ptukey(w,n,Inf)},0,Inf)
d
}
d3<-function(n){
int<-integrate(function(w){w*(1-ptukey(w,n,Inf))},0,Inf)
e<-sqrt(2*int[[1]]-(d2(n)[[1]])^2)
e
}
D1<-function(n){
d1<-max(0, d2(n)[[1]]-3*d3(n))
d1
}
D2<-function(n){
D2<-d2(n)[[1]]+3*d3(n)
D2
}
risco<-function(n){
risco<- 1-(ptukey(D2(n), n, Inf)- ptukey(D1(n), n, Inf))
risco
}
return(risco(n))
}

