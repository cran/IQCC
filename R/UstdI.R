UstdI <-
function(par,sizes,length){
lambda<-mean(par)
e<-matrix(nrow=length,ncol=1)
for(i in 1:length){z<-(par[i]-lambda)/sqrt(lambda/sizes[i]); e[i,]<-z}
std<-e*sizes
qcc(std,type="u",sizes,center=0,limits=c(-3,3),title="U standardized phase I")
return(lambda)
}

