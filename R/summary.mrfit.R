summary.mrfit <-
function (object,...) 
{
	fit<-object; rm(object)
    est <- fit$betahat
    ses <- fit$sebetahat
    tstat <- est/ses
    coef <- cbind(est, ses, tstat)
    colnames(coef) <- c("Estimate", "Std. Error", "t-ratio")
    ans <- list(coefficents = coef)
    class(ans) <- "summary.mrfit"
    ans
}
