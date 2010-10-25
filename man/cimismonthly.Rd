\name{cimismonthly}
\Rdversion{1.1}
\alias{cimismonthly}

\title{Retrieve monthly data from CIMIS}
\description{Retrieves monthly data from CIMIS servers.}
\usage{
cimismonthly(station, month, period = "daily", units = "imp", keepfile = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station}{station ID. See \code{\link{getStnList}}}
  \item{month}{month, in quotes. All lower case, of the form jan, feb,
    mar, ...}
  \item{period}{either "daily" or "hourly". Defaults to "daily".}
  \item{units}{either "imp" for imperial or "SI" for SI.}
  \item{keepfile}{Should the zip file be saved to current working
    directory? This is useful for annual hourly data in particular,
    where file size can be upwards of 20MB. Defaults to TRUE}
}

\value{A data frame of weather data. CIMIS codes are stripped, only
  numeric data is retained. Dimensions depend on period. For daily data,
  a 28-31 x 18 data frame. For hourly data, a (28-31)*24 x 16 data frame.
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

\seealso{\code{\link{cimisdaily}}, \code{\link{cimisannual}}, \code{\link{cimishourly}}}

\examples{
##Feburary dew point in Davis, CA
weather = cimismonthly(station = "006", month = "feb", period = "daily",
   keepfile = FALSE)
plot(dew_pt~datetime, data= weather, pch = 19, type = "o",
   xlab = "Date", ylab = "Dew Point, F",
   main = "February dew point, Davis, CA")
}