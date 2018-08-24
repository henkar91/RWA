\name{rwa-package}
\alias{rwa-package}
\alias{rwa}
\docType{package}
\title{
\packageTitle{Relative Weight Analysis}
}
\description{
\packageDescription{A function that computes several Relative Weight Analysis for total and subset of the data}
}
\details{

}
\author{
\packageAuthor{rwa}

Maintainer: \packageMaintainer{Henrik Karlsson}
}
\references{
http://core.ecu.edu/psyc/wuenschk/MV/multReg/Relative_Weights_Analysis.pdf
}

\keyword{ package }
\seealso{

}
\examples{
data("exdata")
rwa(formula = formula, data = df, split_var = "split", weights = "weight", tb_limit = 4)
}
