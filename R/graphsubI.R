graphsubI <-
function(estat,T2,m,n,p){
amostra<-c(1:m)
UCL<-((p*(m-1)*(n-1))/(m*n-m-p+1))*qf(1-0.0027,p,m*n-m-p+1) 
plot(amostra,T2,main="Hotelling T2 sub I",ylim=c(0,UCL+1),ylab=" T2")
lines(amostra,T2)
mtext("UCL", side = 4, outer = F, at = UCL , padj = 0, col='red', font = 2)
abline(h=UCL, lty=2, col='red')
}

