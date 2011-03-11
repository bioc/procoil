\name{profile-methods}
\docType{methods}
\alias{profile}
\alias{profile-methods}
\alias{profile,CCProfile-method}
\alias{profile.CCProfile}
\title{Retrieve prediction profile from a CCProfile object}
\description{
  Function for retrieving the prediction profile from a
  \code{\linkS4class{CCProfile}} object}
\usage{
\S4method{profile}{CCProfile}(fitted)
}
\arguments{
  \item{fitted}{the coiled coil prediction to be considered}
}
\details{
  The function \code{profile} provides an accessor function for
  retrieving the prediction profile from a \code{\linkS4class{CCProfile}}
  object without the need to directly access the \code{profile} slot.
}
\value{
  \code{profile} returns a vector with the prediction profile.
}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\seealso{\code{\link{procoil}},
\code{\linkS4class{CCProfile}}, \code{\link{predict-methods}}}
\examples{
## predict oligomerization of GCN4 wildtype
GCN4wt<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## show raw prediction profile
profile(GCN4wt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
