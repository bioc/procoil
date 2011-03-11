\name{show-methods}
\docType{methods}
\alias{show}
\alias{show-methods}
\alias{show,CCModel-method}
\alias{show,CCProfile-method}
\alias{show.CCModel}
\alias{show.CCProfile}
\title{Functions for CCModel class}
\description{
  Display information about \code{\linkS4class{CCModel}} and
  \code{\linkS4class{CCProfile}} objects
}
\usage{
\S4method{show}{CCModel}(object)
\S4method{show}{CCProfile}(object)
}
\arguments{
  \item{object}{An object of class \code{\linkS4class{CCModel}} or
                \code{\linkS4class{CCProfile}}}
}
\details{
  If called for an object of class \code{\linkS4class{CCModel}},
  \code{show} displays a summary of data in the
  \code{\linkS4class{CCModel}} object.
  Since the number of pattern weights is potentially large, these
  values are not displayed by \code{show}. In order to see the
  pattern weights (for what reason ever), directly access the
  \code{weights} slot (see example below).

  If called for an object of class \code{\linkS4class{CCProfile}},
  \code{show} displays a summary of the prediction
  result, including the sequence, its heptad register, the
  discriminant function value, and the final prediction (trimer or
  dimer).
  The prediction profile itself is not displayed. To access the profile,
  directly access the \code{profile} slot (see also
  \code{\linkS4class{CCProfile}}).
}
\value{
  Both variants of \code{show} returns an invisible \code{NULL}
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\seealso{\code{\link{procoil}}, \code{\linkS4class{CCModel}},
\code{\linkS4class{CCProfile}}}
\examples{
## display summary of default model
show(PrOCoilModel)

## show all pattern weights
PrOCoilModel@weights

## predict oligomerization of GCN4 wildtype
GCN4wt<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## show result
show(GCN4wt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
