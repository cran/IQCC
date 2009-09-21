UchartII <-
function(x,sizes,lambda,length){
k<-matrix(nrow=length,ncol=1); l<-matrix(nrow=length,ncol=1)
for(i in 1:length){ UCL<-lambda+(3*sqrt(lambda/sizes[i])); k[i,]<-UCL
LCL<-lambda-(3*sqrt(lambda/sizes[i])); l[i,]<-LCL}
qcc(x,type="u",sizes,limits=c(k,l),center=lambda,title="U chart phase II")
}

