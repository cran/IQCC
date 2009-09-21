Rexact <-
function(x,y,m){
qcc(x, type="R", limits=c(qtukey(0.00135,m,Inf)*sd.R(y), qtukey(0.99865,m,Inf)*sd.R(y)))
}

