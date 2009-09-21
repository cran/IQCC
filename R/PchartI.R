PchartI <-
function(x,sizes,par,length){
phat<-mean(par)
s<-matrix(nrow=length,ncol=1); d<-matrix(nrow=length,ncol=1)
for(i in 1:length)
{UCL<-phat+(3*sqrt((phat*(1-phat))/sizes[i]))
s[i,]<-UCL
LCL<-phat-(3*sqrt((phat*(1-phat))/sizes[i]))
d[i,]<-LCL}
qcc(x,type="p",sizes,limits=c(d,s),center=phat,title="P chart phase I")
return(phat)
}

