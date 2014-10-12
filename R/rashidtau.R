rashidtau <-
function (resd, p, delta = 0.8,hcorrect=(length(resid) > p+1) )
{
    n = length(resd)
#
#  for slider
#
    r = n/p
#    if(r < 10){
#       if(r <= 5){
#          delta = .95
#       } else {
#            delta = .95 - .03*(r-5)
#         }
#    } 
#
#   for huber's correction
#
    ind = rep(0,n)
    sr = abs((resd-median(resd))/mad(resd))
    ind[sr <= 2] = 1
    hc = sum(ind)/n
    khub = 1 + ((p*(1-hc))/(n*hc))
    rd = sort(matrix(abs(outer(resd, resd, "-")), ncol = 1))
    tn = quantile(rd, delta)/sqrt(n)
    gtn = 0
    for (i in 1:n^2) {
        if (rd[i] <= tn) {
            gtn = gtn + 1
        }
        else {
            break
        }
    }
    gtn = gtn/n^2
    rashidtau = 1/(sqrt(3)/tn) * gtn 
    if( hcorrect )  rashidtau = rashidtau * sqrt((n - p - 1)/n)
    rashidtau = rashidtau*khub
    rashidtau
}
