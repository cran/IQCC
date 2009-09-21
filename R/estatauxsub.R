estatauxsub <-
function(datum,m,n,p){
datum<-get("datum")
q<-array(dim=c(p,p,m))
w<-array(dim=c(n,p,m))
M2<-array(dim=c(n,p,m))
media<-colMeans(datum)         # média de cada face do array, deve ser uma matriz com 20 linhas e 2 colunas
m1<-matrix(media,m,p,byrow=T)# matriz com as 20 médias
mm<-colMeans(m1)# média das médias
for(i in 1:m)
{ M1<-matrix(m1[i,],n,p,byrow=T) # repetindo cada média da matriz m1 para construir o array de médias e subtrair dos dados
#print(M1)
M2[,,i]<-M1# guardando as repetições em um array
}
w<-datum-M2# array com a diferença de X - Xbarra
for(i in 1:m){
S<-(t(w[,,i])%*%w[,,i])/(n-1)# matriz S
q[,,i]<-S
}
mS<-rowMeans(q,dims=2) # média da matriz de var-cov
return(list(mm,mS,m1))
}

