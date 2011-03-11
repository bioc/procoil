\name{CCProfile-class}
\docType{class}
\alias{CCProfile-class}
\alias{CCProfile}

\title{Class "CCProfile"}
\description{S4 class for representing coiled coil prediction results}
\section{Objects from the Class}{
In principle, objects of this class can be created by calls of
the form \code{new("CCProfile")}, although there is no need in doing
so. Most importantly, the
\code{\link[=predict,CCModel-method]{predict}} function of
\code{\link{procoil}} stores its results in objects of this type.
}
\section{Slots}{
  \describe{
    \item{\code{seq}:}{Object of class \code{"character"} containing
                       the amino acid sequence for which the
                       prediction has been made}
    \item{\code{reg}:}{Object of class \code{"character"}  containing
                       the heptad register corresponding to the
                       amino acid sequence for which the
                       prediction has been made}
    \item{\code{profile}:}{Array of numerical values representing
                           the prediction profile for the sequence
                           under consideration. This array has the
                           same length as the sequence.}
    \item{\code{b}:}{Object of class \code{"numeric"}; value
                     \eqn{b} used in the discriminant function
                     (see \code{\linkS4class{CCModel}} for details}
    \item{\code{disc}:}{Object of class \code{"numeric"} containing
                        the discriminant function value
                        (see \code{\linkS4class{CCModel}} for details}
    \item{\code{pred}:}{Object of class \code{"character"} containing
                        the final classification. Upon a call to
                        \code{\link[=predict,CCModel-method]{predict}},
                        it is either \dQuote{trimer} or
                    \dQuote{dimer}.}
  }
}
\section{Methods}{
  \describe{
   \item{plot}{\code{signature(x = "CCProfile", y = "missing")}:  see
       \code{\link[=plot,CCProfile,missing-method]{plot}}}
   \item{plot}{\code{signature(x = "CCProfile", y = "CCProfile")}:  see
       \code{\link[=plot,CCProfile,CCProfile-method]{plot}}}
   \item{profile}{\code{signature(fitted = "CCProfile")}:  see
       \code{\link[=profile,CCProfile-method]{profile}}}
   \item{show}{\code{signature(object = "CCProfile")}:
        see \code{\link[=show,CCProfile-method]{show}}}
     }
}

\section{Prediction profiles}{
As described in \code{\linkS4class{CCModel}}, the discriminant function
of the coiled coil classifier is essentially a weighted sum of
numbers of occurrences of certain patterns in the sequence under
consideration, i.e. every pattern occurring in the sequence contributes
a certain weight to the discriminant function. Since every such
occurrence is uniquely linked to two specific residues in the
sequence, every amino acid in the sequence contributes a unique weight
to the discriminant function value which is nothing else but half the
sum of weights of matching patterns in which this amino acid is
involved. If we denote the contribution of each position \eqn{i} with
\eqn{s_i}{si}, it follows immediately that

\deqn{f(x)=b+\sum\limits_{i=1}{L} s_i,}{%
      f(x)=b+sum over all si for i=1,\dots L,}

where \eqn{L} is the length of the sequence \eqn{x}.
}
\author{Ulrich Bodenhofer \email{bodenhofer@bioinf.jku.at}}
\references{\url{http://www.bioinf.jku.at/software/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics.
  DOI: 10.1074/mcp.M110.004994}
\seealso{\code{\linkS4class{CCModel}},
  \code{\link[=plot,CCProfile,missing-method]{plot}},
  \code{\link[=plot,CCProfile,CCProfile-method]{plot}},
  \code{\link[=profile,CCProfile-method]{profile}},
  \code{\link[=show,CCProfile-method]{show}},
}
\examples{
showClass("CCProfile")

## predict oligomerization of GCN4 wildtype
GCN4wt<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## display summary of result
GCN4wt

## show raw prediction profile
profile(GCN4wt)

## plot profile
plot(GCN4wt)
}
\keyword{classes}
