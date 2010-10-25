\name{stninfo}
\alias{stninfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Station info
}
\description{
Retreives station info (longitude, latitude, elevation) from CIMIS.
}
\usage{
stninfo(station)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station}{station ID}
}
\value{
a numeric vector of length 3: c(longitude, latitude, elevation)}
\author{
Greg Hirson <ghirson@ucdavis.edu>
}
\examples{
stninfo(6)
}
