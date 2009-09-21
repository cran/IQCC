PextI <-
function(x,sizes,par,length){
phat<-mean(par)
z<-matrix(nrow=length,ncol=1); w<-matrix(nrow=length,ncol=1)
for(i in 1:length)
{UCL<-phat+(3*sqrt((phat*(1-phat))/sizes[i]))+(4*(1-2*phat)/(3*sizes[i]))
z[i,]<-UCL
LCL<-phat-(3*sqrt((phat*(1-phat))/sizes[i]))+(4*(1-2*phat)/(3*sizes[i]))
w[i,]<-LCL}
qcc(x,type="p",sizes,limitsc=c(w,z),center=phat,title="P exact phase I")
return(phat)
}

