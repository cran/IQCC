updatesub <-
function(datum2,estat,T2II,n,t,j){
amostra<-c(1:t)
b<-T2subII(datum2,estat,n)
points(amostra[j],b[1])
c<-c(amostra[j],amostra[j-1])
d<-c(b[1],T2II[1])
lines(c,d)
T2II<<-b}

