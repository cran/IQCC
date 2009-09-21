Raprox <-
function(x,n){
qcc(x, type="R",xlab="")
alfa<-function(n){
d2<-function(n){d<-integrate(function(w){1-ptukey(w,n,Inf)},0,Inf);d}
d3<-function(n){int<-integrate(function(w){w*(1-ptukey(w,n,Inf))},0,Inf)
e<-sqrt(2*int[[1]]-(d2(n)[[1]])^2);e}
D1<-function(n){d1<-max(0, d2(n)[[1]]-3*d3(n));d1}
D2<-function(n){D2<-d2(n)[[1]]+3*d3(n);D2}
risco<-function(n){risco<- 1-(ptukey(D2(n), n, Inf)- ptukey(D1(n), n, Inf))
risco}
risco(n)}
resu<-alfa(n)
result<-signif(resu,3)
mtext(paste("Warning: Prob. of false alarm alfa=",result,"is inflated ( >>0.0027 ) 
since the normal approx. for R is not appropriated; in order to
have alfa=0.0027 it must be used the exact distribution for R."),side=1,font=2)
}

