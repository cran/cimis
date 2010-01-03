\name{mcimisannual}
\Rdversion{1.1}
\alias{mcimisannual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve annual weather data from multiple stations over multiple years.
}
\description{
  This function will get weather data from multiple stations over multiple
  years and return in it a tidy data frame.
}
\usage{
mcimisannual(stationlist, yearlist, label = "none", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stationlist}{
A list of stations.
}
  \item{yearlist}{
A list of years
}
  \item{label}{
    One of "none", "station", and "name" which will add a label of nothing
    useless, the station number, and the station name, respectively.
}
  \item{\dots}{
    Additional parameters to \code{\link{cimisannual}}, most likely, period
    and units.
}
}
\details{
Keepfile = FALSE is highly not recommended, especially
    for hourly measurements, as a new connection is opened for each
    station-year combination.
}
\value{
  A dataframe
}
\author{
Greg Hirson <ghirson@ucdavis.edu>
}
\seealso{
\code{\link{cimisannual}}
}
\examples{
w <- mcimisannual(station = c("006", "077"), year = 2000:2002,
         label = "name")

head(w)
tail(w)
}