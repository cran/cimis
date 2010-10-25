\name{cimisannual}
\Rdversion{1.1}
\alias{cimisannual}

\title{Retrieve annual data drom CIMIS}
\description{Retrieves annual data from CIMIS servers. Currently only
  implemented for imperial units.}
\usage{
cimisannual(station, year, period = "daily", units = "imp", keepfile = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station}{station ID. See \code{\link{getStnList}}}
  \item{year}{year, in quotes}
  \item{period}{either "daily" or "hourly". Defaults to "daily".}
  \item{units}{either "imp" for imperial or "SI" for SI. For this
    function in particular, only imperial is implemented.}
  \item{keepfile}{Should the zip file be saved to current working
    directory? This is useful for hourly data in particular, where file
    size can be upwards of 20MB. Defaults to TRUE}
}

\value{A data frame of weather data. CIMIS codes are stripped, only
  numeric data is retained. Dimensions depend on period. For daily data,
  a 365-366 x 18 data frame. For hourly data, a 8760 or 8784 x 16 data frame.
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

\seealso{\code{\link{cimisdaily}}, \code{\link{cimismonthly}}, \code{\link{cimishourly}}}

\examples{
#Maximum daily temperature for Davis, CA, 2005
weather = cimisannual(station = "006", year = "2005", keepfile = FALSE)
plot(weather$max_air, type = "l", xlab = "Day of Year",
   ylab = "Max. Air Temp, F", main = "Davis, CA, 2005")
}
