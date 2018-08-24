\name{rwa-package}
\alias{rwa-package}
\alias{rwa}
\docType{package}
\title{
\packageTitle{Relative Weight Analysis}
}
\description{
\packageDescription{rwa}
}
\details{

}
\author{
\packageAuthor{rwa}

Maintainer: \packageMaintainer{rwa}
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
