const.table <-
function(n){
u<-matrix(nrow=n,ncol=3)
colnames(u)<-c("d2","d3","c4")
for(i in 2:n){
d2<-function(n){d<-integrate(function(w){1-ptukey(w,n,Inf)},0,Inf);d}
d3<-function(n){int<-integrate(function(w){w*(1-ptukey(w,n,Inf))},0,Inf)
e<-sqrt(2*int[[1]]-(d2(n)[[1]])^2);e}
c4<-function(n){c<-(sqrt(2/(n-1)))*(gamma(n/2)/gamma((n-1)/2));c}
u[i,]<-c(d2(i)[[1]],d3(i),c4(i))
}
y<-u[2:n,]
rownames(y)<-c(2:n)
return(y)}

