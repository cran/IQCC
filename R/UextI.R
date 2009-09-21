UextI <-
function(x,sizes,par,length){
lambda<-mean(par)
z<-matrix(nrow=length,ncol=1); w<-matrix(nrow=length,ncol=1)
for(i in 1:length){
UCL<-lambda+(3*sqrt(lambda/sizes[i]))+(4/(3*sizes[i]))-(1/((3*sizes[i])*sqrt(lambda*sizes[i])))
z[i,]<-UCL
LCL<-lambda-(3*sqrt(lambda/sizes[i]))+(4/(3*sizes[i]))-(1/((3*sizes[i])*sqrt(lambda*sizes[i])))
w[i,]<-LCL}
qcc(x,type="u",sizes,limits=c(w,z),center=lambda,title="U exact phase I")
return(lambda)
}

