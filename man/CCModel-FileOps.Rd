\name{CCModel-FileOps}
\alias{CCModel-FileOps}
\alias{readCCModel}
\alias{writeCCModel}
\title{Reading and writing of coiled coil prediction model from/to files}
\description{
  Functions for reading a coiled coil prediction models
  from a file into a \code{\linkS4class{CCModel}} object
  and writing a \code{\linkS4class{CCModel}} object to a file.
}
\usage{
   readCCModel(file)
   writeCCModel(object, file)
}
\arguments{
  \item{file}{the name of the file from which \code{readCCModel}
    should read the model / the name of the file to which
    \code{writeCCModel} should write the model}
  \item{object}{the \code{\linkS4class{CCModel}} object that
    \code{writeCCModel} should write to a file}
}
\details{
  The \pkg{procoil} package comes with two ready-made models
  for oligomerization prediction, \code{\link{PrOCoilModel}}
  and \code{\link{PrOCoilModelBA}}. In case the user wants to
  define custom models or wishes to use previous versions of
  the prediction models, the functions \code{readCCModel} and
  \code{writeCCModel} can be used to read/write models
  from/to plain text files that can be viewed and also modified.
  
  \code{writeCCModel} writes models in the following format:
  \preformatted{%
    _b,-1.07262284445085
    _m,5
    _scaling,1
    L...Vd...a,1.63626232200227
    R....Eg....e,1.5382098040217
    R.Ec.e,1.29025032360792
    E..Ve..a,1.22837780239385
    ...
  }
  Correspondingly, \code{readCModel} expects the file to conform
  to the above format. See \code{\linkS4class{CCModel}} for an
  overview of model parameters and an explanation of patterns and
  weights.
}
\value{
  Upon successful completion, \code{readCCModel} returns a
  \code{\linkS4class{CCModel}} object. \code{writeCCModel}
  returns an invisible \code{NULL}.
}
\note{
  The PrOCoil model is available on
  on \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModel_v2.CCModel}.
  in exactly the format the function \code{readCCModel} requires.
  Analogously for the alternative model optimized for balanced
  accuracy (see \code{\linkS4class{CCModel}}):
  \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModelBA_v2.CCModel}.
  The original models described in \cite{[Mahrenholz et al., 2011]}
  are available on
  \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModel_v1.CCModel}
  and
  \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModelBA_v1.CCModel},
  respectively. So, by loading one of these files, the original models
  can still be used.
}
\author{Ulrich Bodenhofer}
\references{\url{https://github.com/UBod/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics 10(5):M110.004994.
  DOI: \doi{10.1074/mcp.M110.004994}.
}
\seealso{\code{\link{procoil}}, \code{\link{CCModel-class}}}
\examples{
## load small example model file for testing purposes
## NOTE: this is an incomplete model that will probably not provide
##       meaningful predictions
file <- system.file("examples", "testModel.CCModel", package="procoil")
testModel <- readCCModel(file)

testModel

\dontrun{
## read original model from file
URL <- "http://www.bioinf.jku.at/software/procoil/PrOCoilModel_v1.CCModel"
PrOCoilModelV1 <- readCCModel(URL)

## display summary of example model
PrOCoilModelV1

## display 10 heightes pattern weights
weights(PrOCoilModelV1)[1:10]
}
}
\keyword{manip}
\keyword{data}
