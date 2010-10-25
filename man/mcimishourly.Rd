\name{mcimishourly}
\Rdversion{1.1}
\alias{mcimishourly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform cimishourly over multiple stations
}
\description{
Performs cimishourly over multiple stations.
}
\usage{
mcimishourly(stationlist, label = "none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stationlist}{
A list of stations.
}
  \item{label}{
    One of the following: "none" for no station labels - quite useless;
    "station" to label with the station number; and "name" to label with
    the name of the station.
}
}
\value{
A dataframe.
}

\author{
Greg Hirson <ghirson@ucdavis.edu>
}
\seealso{
\code{\link{cimishourly}}
}
\examples{
\dontrun{w <- mcimishourly(c('006', '077'), label='name')
head(w)
}}