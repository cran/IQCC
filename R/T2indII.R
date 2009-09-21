T2indII <-
function(datum2,estat){n=1
T2<-(t(datum2-estat[[1]])%*%solve(estat[[2]])%*%(datum2-estat[[1]]))}

