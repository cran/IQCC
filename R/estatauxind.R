estatauxind <-
function(datum,m,p){
n=1
g<-array(dim=c(n,p,m-1))
#q<-matrix(nrow=m,ncol=p)
media<-colMeans(datum)         # média das colunas dos dados
m1<-matrix(media,m,p,byrow=T)# matriz com as 20 médias
w<-datum-m1# array com a diferença de X - Xbarra
for(i in 1:m-1)
{v<-matrix(datum[i+1,]-datum[i,],nrow=1,ncol=p)
g[,,i]<-v
}
p1<-matrix(unlist(split(as.matrix(g), rep(1:p, each = 1))),nrow=m-1,ncol=p)
S<-(t(p1)%*%(p1))/(2*(m-1))
list(media,S,w)
}

