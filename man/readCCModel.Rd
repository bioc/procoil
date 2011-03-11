\name{readCCModel}
\alias{readCCModel}
\title{Reading a coiled coil predicton model from a file}
\description{
  Function to read a coiled coil prediction model from a file into a
  \code{\linkS4class{CCModel}} object.
}
\usage{
readCCModel(file)
}
\arguments{
  \item{file}{the name of the file from which the model should be read}
}
\details{
  This function is more or less for testing purposes and future
  safety. Once the package \pkg{procoil} is loaded,
  the default model \code{\link{PrOCoilModel}} is pre-loaded. The
  function \code{readCCModel} can be used to load alternative
  models that may be published in the future (or created manually
  by intensionally changing parameters). The file must be a
  comma-separated table with two columns. The left column must
  contain strings and the right column must contain numerical
  values. Entries in the first column that start with \sQuote{\_}
  refer to model parameters that are directly written into the
  corresponding slots of the resulting model. All other entries
  are interpreted as patterns (with the corresponding weights in
  the second columns). See \code{\linkS4class{CCModel}} for an
  overview of model parameters.
}
\value{
  Upon successful completion, the function returns a
  \code{\linkS4class{CCModel}} object. In case of an error, the
  function does not return any value.
}
\note{
  The PrOCoil model is available on
  on \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModel.patternmodel}.
  in exactly the format the function \code{readCCModel} requires.
  Analogously for the alternative model optimized for balanced
  accuracy (see \code{\linkS4class{CCModel}}):
  \url{http://www.bioinf.jku.at/software/procoil/PrOCoilModelBA.patternmodel}
}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\seealso{\code{\link{procoil}}, \code{\link{CCModel-class}}}
\examples{
\dontrun{## read example model file
testmodel<-readCCModel("http://www.bioinf.jku.at/software/procoil/PrOCoilModel.patternmodel")

## display summary of example model
testmodel

## display 10 heightes pattern weights
weights(testmodel)[1:10]}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{data}
