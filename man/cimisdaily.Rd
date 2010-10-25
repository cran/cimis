\name{cimisdaily}
\Rdversion{1.1}
\alias{cimisdaily}

\title{Retrieve daily data from CIMIS}
\description{Retrieves annual data from CIMIS servers.}
\usage{
cimisdaily(station, units = "imp")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station}{station ID, in quotes. See \code{\link{getStnList}}}
  \item{units}{either "imp" for imperial units or "SI" for SI
    units. Defaults to "imp"}
}

\value{
A 7 x 18 data frame. Returns daily weather data for the last 7 days.  
}
\references{
\url{http://www.cimis.water.ca.gov/cimis/welcome.jsp}
}
\author{
Greg Hirson <\email{ghirson@ucdavis.edu}>
}
\note{
Thank you to Dylan Beaudette at California Soil Resource Lab for
inspiration and
parsing. \url{http://casoilresource.lawr.ucdavis.edu/drupal/node/38},
\url{http://casoilresource.lawr.ucdavis.edu/drupal/node/676}
}

\seealso{\code{\link{cimisannual}}, \code{\link{cimismonthly}}, \code{\link{cimishourly}}}

\examples{
#Average soil temperature for last 7 days in Davis, CA
weather = cimisdaily("006")
plot(soil_avg ~ julian_day, data = weather, pch = 19,
   xlab = "Julian Day", ylab = "Avg. Soil Temp, F",
   main = "Recent soil temperature in Davis, CA")

}