UextII <-
function(x,sizes,lambda,length){
q<-matrix(nrow=length,ncol=1); v<-matrix(nrow=length,ncol=1)
for(i in 1:length){
UCL<-lambda+(3*sqrt(lambda/sizes[i]))+(4/(3*sizes[i]))-(1/((3*sizes[i])*sqrt(lambda*sizes[i])))
q[i,]<-UCL
LCL<-lambda-(3*sqrt(lambda/sizes[i]))+(4/(3*sizes[i]))-(1/((3*sizes[i])*sqrt(lambda*sizes[i])))
v[i,]<-LCL}
qcc(x,type="u",sizes,limits=c(v,q),center=lambda,title="U exact phase II")
}

