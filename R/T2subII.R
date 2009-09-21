T2subII <-
function(datum2,estat,n){
media<-colMeans(datum2)
T2<-n*(t(media-estat[[1]])%*%solve(estat[[2]])%*%(media-estat[[1]]))
return(T2)
}

