print.summary.mrfit <-
function (x, digits = max(5, .Options$digits - 2), ...) 
{
    cat("\nCoefficients:\n")
    est <- x$coef
    print(format(est, digits = digits), quote = F)
}
