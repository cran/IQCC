d3 <-
function(n){
int<-integrate(function(w){w*(1-ptukey(w,n,Inf))},0,Inf)
e<-sqrt(2*int[[1]]-(d2(n)[[1]])^2)
return(e)
}

