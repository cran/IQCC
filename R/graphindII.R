graphindII <-
function(T2II,m,j,t,p){
amostra<-c(1:t)
UCL<-((p*(m+1)*(m-1))/((m^2)-m*p))*qf(1-0.0027,p,m-p) 
plot(amostra[j],T2II[1],main="Hotelling T2 ind II",ylim=c(0,UCL+1),xlim=c(0,t),ylab="T2",xlab="amostra")
mtext("UCL", side = 4, outer = F, at = UCL , padj = 0, col='red', font = 2)
abline(h=UCL, lty=2, col='red')
}

