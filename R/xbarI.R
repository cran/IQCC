xbarI <-
function(x, sizes){
a<-rowMeans(x)
x2bar<-mean(a)
sigma<-sd.xbar(x)
qcc(x,type="xbar",sizes)
return(x2bar,sigma)
}

