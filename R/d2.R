d2 <-
function(n){
d<-integrate(function(w){1-ptukey(w,n,Inf)},0,Inf)
return(d)
}

