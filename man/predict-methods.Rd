\name{predict-methods}
\docType{methods}
\alias{predict}
\alias{predict-methods}
\alias{predict,CCModel-method}
\alias{predict.CCModel}
\title{Predict oligomerization of one or more coiled coil segments}
\description{
  Function for predicting the oligomerization of one or multiple coiled
  coil segments
}
\usage{
\S4method{predict}{CCModel}(object, seq, reg)
}
\arguments{
  \item{object}{The model to be considered; can either be one of the 
    models included in the package (\code{\link{PrOCoilModel}}
    and \code{\link{PrOCoilModelBA}}) or any other model loaded or
    created by the user. For a detailed explanation of the two
    default models, see \code{\linkS4class{CCModel}}.}
  \item{seq}{One or several amino acid sequences; valid
    characters are all uppercase letters except \sQuote{B},
    \sQuote{J}, \sQuote{O}, \sQuote{U}, \sQuote{X}, and
    \sQuote{Z}; invalid characters are tolerated, but ignored
    by the prediction. This argument can be a character vector,
    an \code{\linkS4class{AAString}} object, an
    \code{\linkS4class{AAStringSet}} object, or an
    \code{\linkS4class{AAVector}} object}
  \item{reg}{a character vector containing the heptad register(s);
    valid characters are the lowercase letters
    \sQuote{a}-\sQuote{g} and dashes \sQuote{-}. Can also be omitted,
    see details below.}
}
\details{
  The function \code{predict} is the most important one in the
  \pkg{procoil} package. It is used to apply a coiled coil
  prediction model to coiled coil sequences/segments. It uses the
  discriminant function described in \code{\linkS4class{CCModel}}.
  By default the final classification is computed on the basis of
  the discriminant function value \eqn{f(x)}. If \eqn{f(x)>=0},
  the sequence \eqn{x} is predicted as trimer, otherwise as dimer.

  If the \code{reg} argument is missing, \code{predict}
  looks whether the object passed as argument \code{seq}
  includes heptad register information, either as an attribute
  \code{reg} (if \code{seq} is a character vector), as
  metadata field \code{reg} (if \code{seq} is an
  \code{\linkS4class{AAString}} or \code{\linkS4class{AAStringSet}}
  object), or via annotation metadata (if \code{seq} is
  an \code{\linkS4class{AAStringSet}} or \code{\linkS4class{AAVector}}
  object; see \code{\link{annotationMetadata}}).
  In any case, the \code{reg} argument has priority over all other
  ways of specifying the heptad annotation. In other words,
  if \code{reg} is specified and \code{seq} contains heptad
  annotations in one of the ways described above, the
  \code{reg} argument has priority and the heptad annotation in
  \code{seq} is ignored.
  
  The \code{reg} argument must have exactly as many elements
  as \code{seq} has sequences, and the registers must be
  aligned to the sequences, i.e. the first register must be
  exactly as long as the first sequence, and so on.

  If heptad registers contain dashes, the \code{predict}
  function extracts all contiguous coiled coil segments and computes
  predictions for all of them. The returned
  \code{\linkS4class{CCProfile}} object then contains
  profiles/predictions of all coiled coil segments that were
  extracted from \code{seq} (see example below).
}
\value{  returns a \code{\linkS4class{CCProfile}} object}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics 10(5):M110.004994.
  DOI: 10.1074/mcp.M110.004994
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\seealso{\code{\link{procoil}}, \code{\linkS4class{CCModel}},
\code{\linkS4class{CCProfile}}}
\examples{
## predict oligomerization of GCN4 wildtype
GCN4wt <- predict(PrOCoilModel,
                  "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                  "abcdefgabcdefgabcdefgabcdefga")

## show result
GCN4wt

## example with four GCN4 mutations
GCN4mSeq <- c("GCN4wt"        ="MKQLEDKVEELLSKNYHLENEVARLKKLV",
              "GCN4_N16Y_L19T"="MKQLEDKVEELLSKYYHTENEVARLKKLV",
              "GCN4_E22R_K27E"="MKQLEDKVEELLSKNYHLENRVARLEKLV",
              "GCN4_V23K_K27E"="MKQLEDKVEELLSKNYHLENEKARLEKLV")

## to illustrate the alternative interface, we convert this
## character vector to an 'AAStringSet' object and add
## heptad registers as annotation metadata
GCN4mAA <- AAStringSet(GCN4mSeq)
annotationMetadata(GCN4mAA, annCharset="abcdefg") <-
    rep("abcdefgabcdefgabcdefgabcdefga", 4)

## predict oligomerization (note: no 'reg' argument!)
GCN4mut <- predict(PrOCoilModel, GCN4mAA)

## display summary of result
GCN4mut

## predict oligomerization of unknown sequence (Marcoil example)
MarcoilEx <- predict(PrOCoilModel,
   "MGECDQLLVFMITSRVLVLSTLIIMDSRQVYLENLRQFAENLRQNIENVHSFLENLRADLENLRQKFPGKWYSAMPGRHG",
   "-------------------------------abcdefgabcdefgabcdefgabcdefgabcdefg--------------")

## show results
MarcoilEx
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
