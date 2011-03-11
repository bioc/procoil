\name{predict-methods}
\docType{methods}
\alias{predict}
\alias{predict-methods}
\alias{predict,CCModel-method}
\alias{predict.CCModel}
\title{Predict oligomerization of coiled coil segment}
\description{
  Function for predicting the oligomerization of a coiled coil segment
}
\usage{
\S4method{predict}{CCModel}(object, seq, reg)
}
\arguments{
  \item{object}{The model to be considered; can either be one of the 
                models included in the package (\code{\link{PrOCoilModel}}
                and \code{\link{PrOCoilModelBA}})
                or any other model loaded or created by the user.
                For a detailed explanation of the two default models,
                see\code{\linkS4class{CCModel}}.}
  \item{seq}{An amino acid sequence; valid
             characters are all uppercase letters except \sQuote{B},
             \sQuote{J}, \sQuote{O}, \sQuote{U}, \sQuote{X}, and
             \sQuote{Z}; invalid characters are tolerated, but ignored
             by prediction. This argument can be of any type that can
             be cast to a character string, including Biostrings objects.}
  \item{reg}{A character string denoting the heptad register;
             valid characters are the lowercase letters \sQuote{a} to
             \sQuote{g}. All characters must be in proper order, e.g.
             \sQuote{a} can only be followed by \sQuote{b},
             \sQuote{b} can only be followed by \sQuote{c}, \dots,
             \sQuote{g} can only be followed by \sQuote{a}. The
             register can start with any of the seven letters.
             It must always have the same length as the \code{seq}
             argument. If this argument is missing, \code{predict}
             looks whether the object passed as argument \code{seq}
             has an attribute \code{reg}. If this attribute is missing
             and if \code{seq} is of class \code{BString} or
             \code{AAString}, \code{predict} looks whether there is a
             \code{reg} component in the \code{metadata} slot of \code{seq}.}
}
\details{
  The function \code{predict} is the most important one in the
  \pkg{procoil} package. It is used to apply a coiled coil
  prediction model to a new coiled coil sequence. It uses the
  discriminant function described in \code{\linkS4class{CCModel}}.
  By default the final classification is computed on the basis of
  the discriminant function value. If \eqn{f(x)>=0}, \eqn{x} is
  predicted as trimer, otherwise as dimer.
}
\value{
  If the heptad register (supplied by argument \code{reg} or
  by one of the way described above) does not contain any dashes \sQuote{-},
  \code{predict} returns a \code{\linkS4class{CCProfile}} object
  containing the classification result and the prediction profile.
  If the heptad register contains dashes \sQuote{-}, \code{predict}
  returns a list of \code{\linkS4class{CCProfile}} objects, each
  corresponding to the prediction profile of one of the
  coiled coil segments contained in the sequence. The names of the
  components are of the format \dQuote{s_e}, where \sQuote{s} denotes the
  start position and \sQuote{e} denotes the end position of the
  respective coiled coil sequence in the original sequence.
  \code{show} returns an invisible \code{NULL}
}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\seealso{\code{\link{procoil}}, \code{\linkS4class{CCModel}},
\code{\linkS4class{CCProfile}}}
\examples{
## predict oligomerization of GCN4 wildtype
GCN4wt<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## show and plot results
GCN4wt
plot(GCN4wt)

## predict oligomerization of unknown sequence (Marcoil example)
MarcoilEx<-predict(PrOCoilModel,
                 "MGECDQLLVFMITSRVLVLSTLIIMDSRQVYLENLRQFAENLRQNIENVHSFLENLRADLENLRQKFPGKWYSAMPGRHG",
                 "-------------------------------abcdefgabcdefgabcdefgabcdefgabcdefg--------------")

## show and plot results
MarcoilEx
plot(MarcoilEx[[1]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
