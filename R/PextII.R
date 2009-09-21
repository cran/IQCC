PextII <-
function(x,sizes,phat,length){
b<-matrix(nrow=length,ncol=1); c<-matrix(nrow=length,ncol=1)
for(i in 1:length)
{UCL<-phat+(3*sqrt((phat*(1-phat))/sizes[i]))+(4*(1-2*phat)/(3*sizes[i]))
b[i,]<-UCL
LCL<-phat-(3*sqrt((phat*(1-phat))/sizes[i]))+(4*(1-2*phat)/(3*sizes[i]))
c[i,]<-LCL}
qcc(x,type="p",sizes,limitsc=c(c,b),center=phat,title="P exact phase II")
}

