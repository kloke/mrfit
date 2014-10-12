\name{gly4gen}
\alias{gly4gen}
\docType{data}
\title{ Simulated triglyceride data}
\description{
Simulated dataset similar to an actual trial.
Lipid levels for the patients were measured at specified times.
The response variable of interest change in triglyceride level between baseline and week 4.
Five treatment groups were considered.  The study was conducted at two centers.
}
\usage{data(gly4gen)}
\format{
  A data frame with 97 observations on the following 4 variables.
  \describe{
    \item{\code{obs}}{subject id}
    \item{\code{group}}{treatment group}
    \item{\code{center}}{medical center}
    \item{\code{diffgly4}}{change from baseline in triglyceride at week 4 visit}
  }
}
\details{
Data were simulated to resemble an actual clinical trial dataset.
}
\source{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trail \emph{Statistics in Biopharmaceutical Research}.
}
\examples{
data(gly4gen)
X<-with(gly4gen,model.matrix(~as.factor(group)-1))
X<-X[,2:5]
y<-gly4gen$diffgly4
block<-gly4gen$center
mrfit(X,y,block,rfit(y~X)$coef[2:5])
}
\keyword{datasets}
