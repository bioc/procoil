\name{CCModel-class}
\docType{class}
\alias{CCModel-class}
\alias{CCModel}
\alias{PrOCoilModel}
\alias{PrOCoilModelBA}

\title{Class "CCModel"}
\description{S4 class representing a coiled coil prediction model}
\section{Objects from the Class}{
In principle, objects of this class can be created by calls of
the form \code{new("CCModel")}, although it is probably never necessary
to create such an object from scratch. The default model is stored in
the object \code{PrOCoilModel}. An alternative model,
\code{PrOCoilModelBA}, that is optimized for balanced accuracy
is available too (see below). For future safety, other models can be loaded
from files using the function \code{\link{readCCModel}}.
}
\section{Discriminant function of model}{
  \describe{
     Given a new coiled coil sequence \eqn{x} and a model,
     the discriminant function of the model is given as 

     \deqn{f(x)=b+\sum_{p\in P} N(p,x) w(p),}{%
           f(x)=b+sum over all p in P of (N(p,x) w(p)),}

     where \eqn{b} is a constant,  \eqn{N(p,x)} denotes the number of
     occurrences of pattern \eqn{p} in sequence \eqn{x}, and
     \eqn{w(p)} is the weight assigned to pattern \eqn{p}. \eqn{P}
     is the set of all patterns contained in the model.
     In the models used in the \pkg{procoil} package, the 
     weights are computed from a support vector machine. Models can
     include kernel normalization or not. The formula above refers to
     the variant without kernel normalization. If kernel normalization
     is employed, the weights are computed in a different way and the
     discriminant function changes to

     \deqn{f(x)=b+\frac{\sum_{p\in P} N(p,x) w(p)}{R(x)},}{%
           f(x)=b+(sum over all p in P of (N(p,x) w(p)))/R(x),}

     where \eqn{R(x)} is a normalization value depending on the
     sample \eqn{x}. It is defined as follows:

     \deqn{R(x)=\sum_{p\in P} N(p,x)^2}{%
           R(x)=b+sum over all p in P of N(p,x)^2}

     The \pkg{procoil} package does not consider arbitrary
     patterns, but only very specific ones: pairs of amino acids at
     fixed register positions with no more than a maximum number
     \eqn{m} of residues in between. Internally, these patterns are
     represented as strings with an amino acid letter on the first
     position, then a certain number of wildcards (between 0 and
     \eqn{m} as noted above), then the second amino acid letter, and
     finally a letter \sQuote{a}-\sQuote{g} denoting the heptad
     register position of the first amino acid, e.g.
     \dQuote{N..La}. This pattern matches a coiled coil sequence if
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
}
\section{Slots}{
     \describe{
    \item{\code{b}:}{Object of class \code{"numeric"} the value
                     \eqn{b} as described above}
    \item{\code{m}:}{Object of class \code{"integer"} the value
                     \eqn{m} as described above}
    \item{\code{scaling}:}{Object of class \code{"logical"}
                           indicating whether the model should employ
                           kernel normalization}
    \item{\code{weights}:}{Object of class
               \code{"list"} storing all pattern weights;
               the patterns are stored in the format
               described above}
  }
}
\section{Methods}{
  \describe{
    \item{predict}{\code{signature(object = "CCModel")}: see
       \code{\link[=predict,CCModel-method]{predict}}}
    \item{weights}{\code{signature(object = "CCModel")}: see
       \code{\link[=weights,CCModel-method]{weights}}}
    \item{show}{\code{signature(object = "CCModel")}: see
       \code{\link{show-methods}}}}
}
\section{Default model PrOCoilModel}{
  The \pkg{procoil} package provides a default
  coiled coil prediction model,  \code{PrOCoilModel}.

  The model was created with libSVM \cite{[Chang and Lin, 2001]}
  using the coiled coil kernel
  with \eqn{m=7}, \eqn{C=8}, and kernel normalization on the
  BLAST-augmented data set.
  It is optimized for standard (unbalanced) accuracy, i.e. it tries to
  minimize the probability of misclassifications. Since dimers are more
  frequent in the data set, it slightly favors dimers for unknown
  sequences.
}
\section{Alternative model PrOCoilModelBA}{
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
  the coiled coil kernel with \eqn{m=8}, \eqn{C=2},
  \eqn{\varepsilon=1.3}{e=1.3}, class balancing, and kernel
  normalization on the BLAST-augmented data set.
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994

  Chang, C.-C., and Lin, C.-J. (2001) LIBSVM: a library for
  support vector machines. Software available at
  \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}

  Hochreiter, S., and Obermayer, K. (2006) Support vector machines for
  dyadic data. Neural Computation 18:1472-1510.
  DOI: 10.1162/neco.2006.18.6.1472
}
\seealso{\code{\link{predict-methods}}, \code{\link{show-methods}},
  \code{\link{weights-methods}}}
\examples{
showClass("CCModel")

## show summary of default model (optimized for accuracy)
PrOCoilModel

## show weight of pattern "N..La"
weights(PrOCoilModel)[["N..La"]]

## show the 10 patterns that are most indicative for trimers
## (as the weights are sorted in descending order in PrOCoilModel)
weights(PrOCoilModel)[1:10]

## show the 10 patterns that are most indicative for dimers
## (as the weights are sorted in descending order in PrOCoilModel)
nW <- length(weights(PrOCoilModel))
weights(PrOCoilModel)[nW:(nW - 9)]

## predict oligomerization of GCN4 wildtype
GCN4wt<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## show summary of alternative model (optimized for balanced accuracy)
PrOCoilModelBA

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
\keyword{classes}
