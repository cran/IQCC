PchartII <-
function(x,sizes,phat,length){
g<-matrix(nrow=length,ncol=1); j<-matrix(nrow=length,ncol=1)
for(i in 1:length)
{UCL<-phat+(3*sqrt((phat*(1-phat))/sizes[i]))
g[i,]<-UCL
LCL<-phat-(3*sqrt((phat*(1-phat))/sizes[i]))
j[i,]<-LCL}
qcc(x,type="p",sizes,limits=c(j,g),center=phat,title="P chart phase II")
}

