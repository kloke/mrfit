print.mrfit <-
function (x, ...) 
{
    coef <- x$betahat
    cat("\nCoefficients:\n")
    print(coef, ...)
}
