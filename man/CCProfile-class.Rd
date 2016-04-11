\name{CCProfile-class}
\docType{class}
\alias{CCProfile-class}
\alias{CCProfile}
\alias{[,CCProfile,index,ANY,ANY-method}
\alias{show,CCProfile-method}
\alias{heatmap,CCProfile,missing-method}
\alias{baselines,CCProfile-method}
\alias{profiles,CCProfile-method}
\alias{profile,CCProfile-method}
\alias{sequences,CCProfile-method}
\alias{fitted,CCProfile-method}
\alias{show.CCProfile}
\alias{heatmap.CCProfile}
\alias{baselines.CCProfile}
\alias{profiles.CCProfile}
\alias{profile.CCProfile}
\alias{sequences.CCProfile}
\alias{fitted.CCProfile}

\title{Class "CCProfile"}
\description{S4 class for representing coiled coil prediction results}
\section{Objects from the Class}{
In principle, objects of this class can be created by calls of
the form \code{new("CCProfile")}, although it is not advised to do so.
Most importantly, the
\code{\link[=predict,CCModel-method]{predict}} function of
returns its results in objects of this type.
}
\section{Slots}{
This class extends the class \code{\linkS4class{PredictionProfile}} from
the \pkg{kebabs} package directly and therefore inherits all its slots
and methods. The following slots are defined for \code{CCProfile} objects
additionally:
\describe{
  \item{\code{disc}:}{Object of class \code{"numeric"} containing
    the discriminant function value(s)
    (see \code{\linkS4class{CCModel}} for details)}
  \item{\code{pred}:}{Object of class \code{"factor"} containing
    the final classification(s). Upon a call to
    \code{\link[=predict,CCModel-method]{predict}},
    it is either \dQuote{trimer} or
    \dQuote{dimer}.}
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
  \eqn{s_i(x)}{si(x)}, it follows immediately that
  
  \deqn{f(x)=b+\sum\limits_{i=1}^{L} s_i(x),}{%
    f(x)=b+sum over all si(x) for i=1,\dots L,}

  where \eqn{L} is the length of the sequence \eqn{x}. The values
  \eqn{s_i(x)}{si(x)} can then be understood as the contributions that
  the i-th residue makes to the overall classification of the sequence
  \eqn{x}, which we call \emph{prediction profile}. These profiles can
  either be visualized as they are without taking the offset \eqn{b}
  into account or by distributing \eqn{b} equally over all residues.
  These are the so-called \emph{baselines} that are included in
  \code{CCProfile} objects. They are computed as \eqn{-b / L}.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x="CCProfile", y="missing")}: see
      \code{\link[=plot,CCProfile,missing-method]{plot}}}
    \item{heatmap}{\code{signature(x="CCProfile", y="missing")}: if the
      \code{CCProfile} object \code{x} contains the profiles of at least
      three sequences, the profiles are visualized as a heatmap.
      This method is inherited from the \pkg{kebabs} package; for
      details, see
      \code{\link[=heatmap,PredictionProfile,missing-method]{heatmap}}.}
    \item{show}{\code{signature(object="CCProfile")}:
      displays the most important information stored in the
      \code{CCProfile} object \code{object}, such as, the sequences,
      kernel parameters, baselines, profiles, and classification results.}
  }
}
\section{Accessor-like methods}{
  The \code{CCProfile} class inherits all accessors from the
  \code{\linkS4class{PredictionProfile}} class, such as,
  \code{\link[=sequences,PredictionProfile-method]{sequences}},
  \code{\link[=baselines,PredictionProfile-method]{baselines}},
  \code{\link[=profiles,PredictionProfile-method]{profiles}}, and
  the indexing operator \code{x[i]}. 
  Additionally, the \pkg{procoil} package defines the following two methods:
  \describe{
    \item{profile}{\code{signature(fitted="CCProfile")}: for
      compatibility with previous versions, a method \code{profile}
      is available, too. It extracts the profile(s) in the same way as
      \code{\link[=profiles,PredictionProfile-method]{profiles}}}
    \item{fitted}{\code{signature(object="CCProfile")}: extracts
      the final classifications. This function returns a factor with
      levels \dQuote{dimer} and \dQuote{trimer}. If
      \code{decision.values=TRUE} is specified, a numeric vector is
      attached to the result as an attribute \code{"decision.values"}
      which also contains the discriminant function values.}
  }
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
}
\seealso{\code{\linkS4class{CCModel}},
  \code{\link[=plot,CCProfile,missing-method]{plot}},
  \code{\link[=heatmap,PredictionProfile,missing-method]{plot}},
  \code{\link{PredictionProfileAccessors}},
}
\examples{
showClass("CCProfile")

## predict oligomerization of GCN4 wildtype
GCN4wt <- predict(PrOCoilModel,
                  "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                  "abcdefgabcdefgabcdefgabcdefga")

## display summary of result
GCN4wt

## show raw prediction profile
profile(GCN4wt)

## plot profile
plot(GCN4wt)

## define four GCN4 mutations
GCN4mSeq <- c("GCN4wt"        ="MKQLEDKVEELLSKNYHLENEVARLKKLV",
              "GCN4_N16Y_L19T"="MKQLEDKVEELLSKYYHTENEVARLKKLV",
              "GCN4_E22R_K27E"="MKQLEDKVEELLSKNYHLENRVARLEKLV",
              "GCN4_V23K_K27E"="MKQLEDKVEELLSKNYHLENEKARLEKLV")
GCN4mReg <- rep("abcdefgabcdefgabcdefgabcdefga", 4)

## predict oligomerization
GCN4mut <- predict(PrOCoilModel, GCN4mSeq, GCN4mReg)

## display summary of result
GCN4mut

## display predictions
fitted(GCN4mut)

## overlay plot of two profiles 
plot(GCN4mut[c(1, 2)])

## show heatmap
heatmap(GCN4mut)
}
\keyword{classes}
