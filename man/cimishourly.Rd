\name{cimishourly}
\Rdversion{1.1}
\alias{cimishourly}

\title{Retrieve hourly data from CIMIS}
\description{Retrieves hourly data from CIMIS servers.}
\usage{
cimishourly(station, units = "imp")
}

\arguments{
  \item{station}{station ID, in quotes. See \code{\link{stnlist}}}
  \item{units}{either "imp" for imperial units or "SI" for SI
    units. Defaults to "imp"}
}


\value{
A 192 x 16 data frame. Returns hourly weather data for the last 7 days.  
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

\seealso{\code{\link{cimisdaily}}, \code{\link{cimismonthly}}, \code{\link{cimisannual}}}

\examples{
##Recent hourly solar radiation in Davis, CA
weather = cimishourly("006")
plot(solar_rad ~ datetime, data = weather, type = "l",
   xlab = "Time", ylab = "Solar Radiation",
   main = "Recent solar radiation in Davis, CA")
}