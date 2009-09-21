Sexact <-
function(x,m){
qcc(x, type="S",limits=c((sqrt(qchisq(0.00135,m-1)/(m-1)))*sd.S(x),(sqrt(qchisq(0.99865,m-1)/(m-1)))*sd.S(x)))
}

