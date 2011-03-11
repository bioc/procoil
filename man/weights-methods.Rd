\name{weights-methods}
\docType{methods}
\alias{weights}
\alias{weights-methods}
\alias{weights,CCModel-method}
\alias{weights.CCModel}
\title{Retrieve pattern weights from a CCModel object}
\description{
  Function for retrieving the pattern weights from a
  \code{\linkS4class{CCModel}} object}
\usage{
\S4method{weights}{CCModel}(object)
}
\arguments{
  \item{object}{The model to be considered; can either be one of the 
                models included in the package (\code{\link{PrOCoilModel}}
                and \code{\link{PrOCoilModelBA}})
                or any other model loaded or created by the user.
                For a detailed explanation of the two default models,
                see \code{\linkS4class{CCModel}}.}
}
\details{
  The function \code{weights} provides an accessor function for
  retrieving the pattern weights from a \code{\linkS4class{CCModel}}
  object without the need to directly access the \code{weights} slot.
}
\value{
  \code{weights} returns a list of pattern weights.
}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\seealso{\code{\link{procoil}}, \code{\linkS4class{CCModel}},
\code{\linkS4class{CCProfile}}, \code{\link{readCCModel}}}
\examples{
## show weight of pattern "N..La"
weights(PrOCoilModel)[["N..La"]]

## show the 10 patterns that are most indicative for trimers
## (as the weights are sorted in descending order in PrOCoilModel)
weights(PrOCoilModel)[1:10]

## show the 10 patterns that are most indicative for dimers
## (as the weights are sorted in descending order in PrOCoilModel)
nW <- length(weights(PrOCoilModel))
weights(PrOCoilModel)[nW:(nW - 9)]

## show weight of pattern "N..La"
weights(PrOCoilModelBA)[["N..La"]]

## show the 10 patterns that are most indicative for trimers
## (as the weights are sorted in descending order in PrOCoilModelBA)
weights(PrOCoilModelBA)[1:10]

## show the 10 patterns that are most indicative for dimers
## (as the weights are sorted in descending order in PrOCoilModelBA)
nW <- length(weights(PrOCoilModelBA))
weights(PrOCoilModelBA)[nW:(nW - 9)]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
