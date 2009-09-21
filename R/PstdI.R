PstdI <-
function(par,sizes,length){
phat<-mean(par)
f<-matrix(nrow=length,ncol=1)
for(i in 1:length){
z<-(par[i]-phat)/sqrt((phat*(1-phat))/sizes[i])
f[i,]<-z}
std<-f*sizes
qcc(std,type="p",sizes,center=0,limits=c(-3,3),title="P standardized phase I")
return(phat)
}

