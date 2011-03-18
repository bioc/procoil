\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,CCProfile,missing-method}
\alias{plot,CCProfile,CCProfile-method}
\alias{plot.CCProfile}
\title{Plotting prediction profiles}
\description{
  Functions for plotting prediction profiles
}
\usage{
\S4method{plot}{CCProfile,missing}(x, col="red", rng=0,
             standardize=FALSE, shades=NULL,
             legend="", legend.pos="topright", ...)
\S4method{plot}{CCProfile,CCProfile}(x, y, col=c("red", "blue"), rng=0,
             standardize=FALSE, shades=NULL, legend=NULL,
             legend.pos="topright", ...)
}
\arguments{
  \item{x}{Object of class \code{\linkS4class{CCProfile}} to be
           plotted with \code{plot}}
  \item{y}{Object of class \code{\linkS4class{CCProfile}} to be
           plotted with \code{plot} (in case \code{plot} is called
           with two arguments to compare two profiles)}
  \item{col}{Character string containing the name of the color
             in which the profile should be plotted (in case
             \code{plot} is called with one
             \code{\linkS4class{CCProfile}} argument). A vector of
	     character strings containing the names of the two colors
             in which the profiles should be plotted (in case
             \code{plot} is called with two
             \code{\linkS4class{CCProfile}} arguments).}
  \item{rng}{Argument that allows the user to preset the range of
             the profile plot. If 0 (default) or negative, the
             range is determined automatically from
             the values in the profile. Otherwise, the range
             is set to \eqn{[-rng,rng]}.}
  \item{standardize}{logical. If \code{FALSE} (default), the profile
                     values \eqn{s_i}{si} are displayed as they are
                     with the value \eqn{y=-b/L} superimposed as a
                     light gray line. If \code{TRUE}, the whole
                     profile is shifted by \eqn{-b/L} and the light
                     gray line is displayed at \eqn{y=0}.}
  \item{shades}{Vector of at least two color specifications (default:
    NULL). If not NULL, the background area above and below the base
    line  \eqn{y=-b/L} are shaded in colors \code{shades[1]} and
    \code{shades[2]}, respectively.}
  \item{legend}{A character string containing the legend/description of
              the profile (in case \code{plot} is called with one
              \code{\linkS4class{CCProfile}} argument). A vector of
	      character strings containing the legends/descriptions of
              the profiles (in case \code{plot} is called with two
              \code{\linkS4class{CCProfile}} arguments). If empty,
	      no legend is displayed.}
  \item{legend.pos}{Position specification for legend (if \code{legend}
	            is specified). Can either be a vector with
		    coordinates or a single keyword like
		    \dQuote{topright} (see
		    \code{\link[graphics:legend]{legend}}).}
  \item{...}{all other arguments are passed to the standard
             \code{\link[graphics:plot]{plot}} command that is called
             internally to display the graphics window}
}
\details{
  The \code{plot} function displays the profile as a step function over
  the sequence with the steps connected by vertical lines. The vertical
  plot range can be determined by the \code{rng} argument. The sequence
  and the heptad register are visualized below and above the profile,
  respectively. The value \eqn{-b/L} and the light gray line has the 
  following meaning: It is obvious that we can rewrite

  \deqn{f(x)=b+\sum\limits_{i=1}{L} s_i}{%
      f(x)=b+sum over all si for i=1,\dots L}

  as

  \deqn{f(x)=\sum\limits_{i=1}{L} (s_i - (-\frac{b}{L}))}{%
      f(x)=sum over all (si - (-b/L)) for i=1,\dots L,}

  so the discriminant function value \eqn{f(x)} can be understood
  as the sum of values \eqn{s_i - (-\frac{b}{L})}{(si - (-b/L))}, i.e.
  the area between the constant value  \eqn{-b/L} and the prediction
  profile. If the area above the light gray line is greater than
  the area below the light gray line, the sequence is predicted as
  trimer, otherwise as dimer. Note that this correspondence only
  holds for predictions based on the raw discriminant value. If
  probability estimates are used for the final classification, this
  may not be fully true.

  If \code{plot} is called with two \code{\linkS4class{CCProfile}}
  arguments, the two profiles are plotted together to facilitate
  a comparison of profiles (e.g. wild type sequences versus
  mutants).
}
\value{
   Both variants of \code{plot} do not return any value.
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

## plot profile
plot(GCN4wt)

## predict oligomerization of a GCN4 mutant
GCN4m3<-predict(PrOCoilModel,
                "MKQLEDKVEELLSKIYHNENEVARLKKLV",
                "abcdefgabcdefgabcdefgabcdefga")

## plot two profiles
plot(GCN4wt, GCN4m3, legend=c("GCN4 wild type", "GCN4 mutant"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{models}
\keyword{methods}
