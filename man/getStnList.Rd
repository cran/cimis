\name{getStnList}
\alias{getStnList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download up-to-date CIMIS Station list
}
\description{
Retrieves CIMIS Sation list from CIMIS
}
\usage{
getStnList(active = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{active}{logical. if TRUE, only returns active station. if FALSE,
  returns active and inactive stations.}
}
\value{
a dataframe with station number, name, county, and status.  
}

\author{
Greg Hirson <ghirson@ucdavis.edu>
}

\examples{
getStnList()
}
