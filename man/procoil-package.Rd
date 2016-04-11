\name{procoil-package}
\alias{procoil-package}
\alias{procoil}
\docType{package}
\title{Prediction of Oligomerization of Coiled Coil Proteins}
\description{
  The package allows for predicting whether a coiled coil sequence
  (amino acid sequence plus heptad register) is more likely to form
  a dimer or more likely to form a trimer. Additionally to the
  prediction itself, a prediction profile is computed which allows
  for determining the strengths to which the individual residues
  are indicative for either class. Prediction profiles can also
  be visualized as curves or heatmaps.
}
\details{
The package defines two S4 classes, \code{\linkS4class{CCModel}} and
\code{\linkS4class{CCProfile}}. The former's purpose is to represent
a coiled coil prediction model. The default model
\code{\link{PrOCoilModel}} is pre-loaded when the package is loaded.
An alternative model \code{\link{PrOCoilModelBA}} is also available.
Other models can be loaded with the function
\code{\link{readCCModel}}. The
\code{\link[=predict,CCModel-method]{predict}} function is
used to predict the oligomerization of one or more coiled coil
sequences (which consist of a amino acid sequences and heptad
registers aligned to them). The result is stored in a
\code{\linkS4class{CCProfile}} object.
The resulting prediction profile can be visualized with
\code{\link[=plot,CCProfile,missing-method]{plot}}.
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics 10(5):M110.004994.
  DOI: 10.1074/mcp.M110.004994
}
\keyword{package}
\examples{
## display summary of default model
PrOCoilModel

## predict oligomerization of GCN4 wildtype
GCN4wt <- predict(PrOCoilModel,
                  "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                  "abcdefgabcdefgabcdefgabcdefga")

## display result
GCN4wt

## plot profile
plot(GCN4wt)

## predict oligomerization of unknown sequence (Marcoil example)
MarcoilEx <- predict(PrOCoilModel,
    "MGECDQLLVFMITSRVLVLSTLIIMDSRQVYLENLRQFAENLRQNIENVHSFLENLRADLENLRQKFPGKWYSAMPGRHG",
    "-------------------------------abcdefgabcdefgabcdefgabcdefgabcdefg--------------")

## display result
MarcoilEx

## plot profile
plot(MarcoilEx)
}
