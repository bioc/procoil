\name{CCModel-class}
\docType{class}
\alias{CCModel-class}
\alias{CCModel}
\alias{PrOCoilModel}
\alias{PrOCoilModelBA}
\alias{weights,CCModel-method}
\alias{show,CCModel-method}
\alias{show.CCModel}
\alias{weights.CCModel}

\title{Class "CCModel"}
\description{S4 class representing a coiled coil prediction model}
\section{Objects from the Class}{
  In principle, objects of this class can be created by calls of
  the form \code{new("CCModel")}, although it is probably never necessary
  to create such an object from scratch - and not advised either.
  The default model is stored in
  the object \code{PrOCoilModel}. An alternative model,
  \code{PrOCoilModelBA}, that is optimized for balanced accuracy
  is available too (see below). Custom models can be loaded
  from files using the function \code{\link{readCCModel}}.
}
\section{Discriminant function of model}{
  Given a new coiled coil sequence \eqn{x} and a model,
  the discriminant function of the model is given as 

  \deqn{f(x)=b+\sum_{p\in P} N(p,x)\cdot w(p),}{%
    f(x)=b+sum over all p in P of (N(p,x) w(p)),}

  where \eqn{b} is a constant offset,  \eqn{N(p,x)} denotes the number of
  occurrences of pattern \eqn{p} in sequence \eqn{x}, and
  \eqn{w(p)} is the weight assigned to pattern \eqn{p}. \eqn{P}
  is the set of all patterns contained in the model.
  In the models used in the \pkg{procoil} package, the 
  weights are computed from a support vector machine. Models can
  include kernel normalization or not. The formula above refers to
  the variant without kernel normalization. If kernel normalization
  is employed, the weights are computed in a different way and the
  discriminant function changes to

  \deqn{f(x)=b+\frac{\sum_{p\in P} N(p,x)\cdot w(p)}{R(x)},}{%
    f(x)=b+(sum over all p in P of (N(p,x) w(p)))/R(x),}

  where \eqn{R(x)} is a normalization value depending on the
  sample \eqn{x}. It is defined as follows:

  \deqn{R(x)=\sqrt{\sum_{p\in P} N(p,x)^2}}{%
    R(x)=sqrt(sum over all p in P of N(p,x)^2)}

  The \pkg{procoil} package does not consider arbitrary
  patterns, but only very specific ones: pairs of amino acids at
  fixed register positions with no more than a maximum number
  \eqn{m} of residues in between. Internally, these patterns are
  represented as strings with an amino acid letter on the first
  position, then a certain number of wildcards (between 0 and
  \eqn{m} as noted above), then the second amino acid letter, and
  an aligned sequence with the same number of wildcards and letters
  \sQuote{a}-\sQuote{g} denoting the heptad
  register position on the first and last amino acid, e.g.
  \dQuote{N..La..d}. This pattern matches a coiled coil sequence if
  the sequence has an \sQuote{N} (Asparagine) at an \sQuote{a}
  position and a \sQuote{L} (Leucine) at the next \sQuote{d}
  position. For instance, the GCN4 wildtype has one occurrence of
  this pattern:
  \preformatted{%
    MKQLEDKVEELLSKNYHLENEVARLKKLV
    abcdefgabcdefgabcdefgabcdefga
                  N..L
                  a  d
  }
}
\section{Slots}{
  \describe{
    \item{\code{b}:}{Object of class \code{numeric} the value
      \eqn{b} as described above}
    \item{\code{m}:}{Object of class \code{integer} the value
      \eqn{m} as described above}
    \item{\code{scaling}:}{Object of class \code{logical}
      indicating whether the model should employ
      kernel normalization}
    \item{\code{weights}:}{Object of class
      \code{matrix} storing all pattern weights;
      the matrix in this slot is actually consisting of only
      one row that contains the weights. The patterns are
      stored in column names of the matrix and encoded in 
      the format described above}
  }
}
\section{Methods}{
  \describe{
    \item{predict}{\code{signature(object = "CCModel")}: see
      \code{\link[=predict,CCModel-method]{predict}}}
    \item{show}{\code{signature(object = "CCModel")}: displays the most
      important information stored in the \code{CCModel} object
      \code{object}, such as, kernel parameters and a summary of weights.}
    \item{weights}{\code{signature(object="CCModel")}: returns the
      weights stored in \code{object} as a named numeric vector.}
   }
 }
\section{Default model \code{PrOCoilModel}}{
  The \pkg{procoil} package provides a default
  coiled coil prediction model, \code{PrOCoilModel}.
  The model was created with the \pkg{kebabs} package
  \cite{[Palme et al., 2015]} using the coiled coil kernel
  with \eqn{m=5}, \eqn{C=2}, and kernel normalization on the
  BLAST-augmented data set.
  It is optimized for standard (unbalanced) accuracy, i.e. it tries to
  minimize the probability of misclassifications. Since dimers are more
  frequent in the data set, it slightly favors dimers for unknown
  sequences.

  Note that this is not the original model as described in
  \cite{[Mahrenholz et al., 2011]}. The models have been re-trained
  for version 2.0.0 of the package using a newer snapshot of PDB and
  newer methods.
  The original models are still available for download and can still
  be used if the user wishes to. For detailed instructions, see the
  package vignette.
}
\section{Alternative model \code{PrOCoilModelBA}}{
  As mentioned above, the default model \code{PrOCoilModel}
  slightly favors dimers. This may be undesirable for some
  applications. For such cases, an alternative model
  \code{PrOCoilModelBA} is available that is optimized
  for balanced accuracy, i.e. it tries not to favor the larger
  class - dimers -, but may therefore prefer trimers in borderline cases.
  The overall misclassification probability is slightly higher for
  this model than for the default model \code{PrOCoilModel}.

  The model \code{PrOCoilModelBA} was created with PSVM
  \cite{[Hochreiter and Obermayer, 2006]} using
  the coiled coil kernel with \eqn{m=8}, \eqn{C=8},
  \eqn{\varepsilon=0.8}{e=0.8}, class balancing, and kernel
  normalization on the PDB data set (i.e. without BLAST augmentation).

  The same applies as for \code{PrOCoilModel}: this model has been
  re-trained for package version 2.0.0. For detailed instructions how to
  use the original models, see the package vignette.
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics 10(5):M110.004994.
  DOI: 10.1074/mcp.M110.004994

  Palme, J., Hochreiter, S., and Bodenhofer, U. (2015) KeBABS:
  an R package for kernel-based analysis of biological sequences.
  Bioinformatics 31(15):2574-2576. DOI: 10.1093/bioinformatics/btv176

  Hochreiter, S., and Obermayer, K. (2006) Support vector machines for
  dyadic data. Neural Computation 18:1472-1510.
  DOI: 10.1162/neco.2006.18.6.1472
}
\seealso{\code{\link{predict-methods}}}
\examples{
showClass("CCModel")

## show summary of default model (optimized for accuracy)
PrOCoilModel

## show weight of pattern "N..La..d"
weights(PrOCoilModel)["N..La..d"]

## show the 10 patterns that are most indicative for trimers
## (as the weights are sorted in descending order in PrOCoilModel)
weights(PrOCoilModel)[1:10]

## predict oligomerization of GCN4 wildtype
GCN4wt <- predict(PrOCoilModel,
                  "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                  "abcdefgabcdefgabcdefgabcdefga")

## show summary of alternative model (optimized for balanced accuracy)
PrOCoilModelBA

## show weight of pattern "N..La..d"
weights(PrOCoilModelBA)["N..La..d"]

## show the 10 patterns that are most indicative for trimers
## (as the weights are sorted in descending order in PrOCoilModelBA)
weights(PrOCoilModelBA)[1:10]
}
\keyword{classes}
