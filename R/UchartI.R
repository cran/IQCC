UchartI <-
function(x,sizes,par,length){
lambda<-mean(par)
h<-matrix(nrow=length,ncol=1); j<-matrix(nrow=length,ncol=1)
for(i in 1:length){ UCL<-lambda+(3*sqrt(lambda/sizes[i])); h[i,]<-UCL
LCL<-lambda-(3*sqrt(lambda/sizes[i])); j[i,]<-LCL}
qcc(x,type="u",sizes,limits=c(j,h),center=lambda,title="U chart phase I")
return(lambda)
}

