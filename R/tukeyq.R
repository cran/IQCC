tukeyq <-
function(alfa,n){u<-matrix(nrow=n,ncol=4)
colnames(u)<-c("alfa/2","alfa","1-alfa","1-alfa/2")
for(i in 2:n){
a<-function(i){qtukey(alfa/2,i,Inf)}
b<-function(i){qtukey(alfa,i,Inf)}
g<-function(i){qtukey(1-alfa,i,Inf)}
d<-function(i){qtukey(1-(alfa/2),i,Inf)}
u[i,]<-c(a(i),b(i),g(i),d(i))}
y<-u[2:n,]; rownames(y)<-c(2:n); y}

