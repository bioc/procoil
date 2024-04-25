\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,CCProfile,missing-method}
\alias{plot.CCProfile}
\title{Plotting prediction profiles}
\description{
  Functions for plotting prediction profiles
}
\usage{
\S4method{plot}{CCProfile,missing}(x, col=c("red", "blue"),
     standardize=TRUE, shades=NULL, legend="default",
     legendPos="topright", xlab="", ylab="weight",
     lwd.profile=1, lwd.axis=1, las=1,
     heptads=TRUE, annotate=TRUE, ...)
}
\arguments{
  \item{x}{Object of class \code{\linkS4class{CCProfile}} to be
    plotted with \code{plot}}
  \item{col}{Character string containing the name(s) of the color(s)
    in which the profile(s) should be plotted.}
  \item{standardize}{If \code{FALSE}, the profile
    values \eqn{s_i}{si} are displayed as they are with the value
    \eqn{y=-b/L} superimposed as a light gray line. If \code{TRUE}
    (default), the profile(s) is/are shifted by the baseline values
    \eqn{-b/L} and the light
    gray line is displayed at \eqn{y=0}.}
  \item{shades}{Vector of at least two color specifications (default:
    NULL). If not NULL, the background area above and below the base
    line  \eqn{y=-b/L} are shaded in colors \code{shades[1]} and
    \code{shades[2]}, respectively.}
  \item{legend}{A character string containing the legend/description of
    the profile. If \code{"default"}, the names of the
    sequences/profiles are used. If no names are available, the profiles
    are simply enumerated (as long as two profiles should be plot
    together; if only a single unnamed profile is to be plotted, no
    legend is shown). If \code{legend} is an empty string, no legend
    is displayed at all.}
  \item{legendPos}{position specification for legend (if \code{legend}
    is specified). Can either be a vector with
    coordinates or a single keyword like
    \dQuote{topright} (see
    \code{\link[graphics:legend]{legend}}).}
  \item{xlab}{label of horizontal axis, empty by default.}
  \item{ylab}{label of vertical axis, defaults to \dQuote{weight}.}
  \item{lwd.profile}{profile line width as described for
    parameter \code{lwd} in \code{\link[graphics:par]{par}}}
  \item{lwd.axis}{axis line width as described for
    parameter \code{lwd} in \code{\link[graphics:par]{par}}}
  \item{las}{see \code{\link[graphics:par]{par}}}
  \item{heptads}{if \code{TRUE} (default), the heptad structure
    is indicated by vertical light gray lines separating the different
    heptads. Heptad irregularities are indicated with red lines.}
  \item{annotate}{if \code{TRUE} (default), the heptad annotation
    information is shown in the center of the plot.}
  \item{...}{all other arguments are passed to the
    \code{\link[graphics:plot]{plot}} method from the
    \pkg{kebabs} package}
}
\details{
  The \code{plot} function displays a prediction profile as a step
  function over the sequence with the steps connected by vertical lines.
  The sequence and the heptad register are visualized below and above
  the profile, respectively. The baseline value \eqn{-b/L} and the light
  gray line has the  following meaning: It is obvious that we can rewrite

  \deqn{f(x)=b+\sum\limits_{i=1}^{L} s_i(x)}{%
      f(x)=b+sum over all si(x) for i=1,\dots L}

  as

  \deqn{f(x)=\sum\limits_{i=1}^{L} (s_i(x) - (-\frac{b}{L}))}{%
      f(x)=sum over all (si(x) - (-b/L)) for i=1,\dots L,}

  so the discriminant function value \eqn{f(x)} can be understood
  as the sum of values \eqn{s_i(x) - (-\frac{b}{L})}{(si(x) - (-b/L))}, i.e.
  the area between the constant value  \eqn{-b/L} and the prediction
  profile. If the area above the light gray line is greater than
  the area below the light gray line, the sequence is predicted as
  trimer, otherwise as dimer.

  If \code{plot} is called for a \code{\linkS4class{CCProfile}} object
  that contains profiles of two sequences, the two profiles are plotted
  together to facilitate a comparison of profiles (e.g. wild type
  sequences versus mutants). Although the \code{plot} function tolerates
  profiles/sequences with different lengths and/or unaligned heptad
  registers, it is obvious that the superimposition of profiles of
  two unaligned, unrelated sequences makes little sense.

  The \code{plot} functions gives an error if is called for a
  \code{\linkS4class{CCProfile}} object that contains profiles of
  three or more sequences.

  The given function is only a wrapper around the
  \code{\link[=plot,PredictionProfile-method]{plot}} function provided
  by the \pkg{kebabs} package. The only difference is that heptad
  seperators (argument \code{heptads}) and the heptad annotation
  (argument \code{annotate}) are displayed by default.
  Moreover, presently,  no legend is displayed by default if a
  single profile is plotted for an unnamed sequence.
}
\value{
   This function does not return any value.
}
\author{Ulrich Bodenhofer}
\references{\url{https://github.com/UBod/procoil/}

  Mahrenholz, C.C., Abfalter, I.G., Bodenhofer, U., Volkmer, R., and
  Hochreiter, S. (2011) Complex networks govern coiled coil
  oligomerization - predicting and profiling by means of a machine
  learning approach. Mol. Cell. Proteomics 10(5):M110.004994.
  DOI: \doi{10.1074/mcp.M110.004994}.

  Palme, J., Hochreiter, S., and Bodenhofer, U. (2015) KeBABS:
  an R package for kernel-based analysis of biological sequences.
  Bioinformatics 31(15):2574-2576. DOI: \doi{10.1093/bioinformatics/btv176}.
}
\seealso{\code{\link{procoil}}, \code{\linkS4class{CCModel}},
\code{\linkS4class{CCProfile}}}
\examples{
## predict oligomerization of GCN4 wildtype
GCN4wt <- predict(PrOCoilModel,
                  "MKQLEDKVEELLSKNYHLENEVARLKKLV",
                  "abcdefgabcdefgabcdefgabcdefga")

## plot profile
plot(GCN4wt)

## define two GCN4 mutations
GCN4mSeq <- c("GCN4wt"        ="MKQLEDKVEELLSKNYHLENEVARLKKLV",
              "GCN4_N16I_L19N"="MKQLEDKVEELLSKIYHNENEVARLKKLV")
GCN4mReg <- rep("abcdefgabcdefgabcdefgabcdefga", 2)

## predict oligomerization
GCN4mut <- predict(PrOCoilModel, GCN4mSeq, GCN4mReg)

## overlay plot of the two profiles 
plot(GCN4mut)
}
\keyword{classif}
\keyword{models}
\keyword{methods}
