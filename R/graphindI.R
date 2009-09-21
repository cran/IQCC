graphindI <-
function(T2,m,p){
n=1;
amostra<-c(1:m)
Q=(2*(m-1)^2)/(3*m-4)
UCL<-(((m-1)^2)/m)*qbeta(1-0.0027,p/2,(Q-p-1)/2) 
plot(amostra,T2,main="Hotelling T2 ind I",ylim=c(0,UCL+1),ylab="T2")
lines(amostra,T2)
mtext("UCL", side = 4, outer = F, at = UCL , padj = 0, col='red', font = 2)
abline(h=UCL, lty=2, col='red')
}

