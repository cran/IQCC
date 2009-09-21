UstdII <-
function(par,sizes,lambda,length){
f<-matrix(nrow=length,ncol=1)
for(i in 1:length){y<-(par[i]-lambda)/sqrt(lambda/sizes[i]); f[i,]<-y}
std<-f*sizes
qcc(std,type="u",sizes,center=0,limits=c(-3,3),title="U standardized phase II")
}

