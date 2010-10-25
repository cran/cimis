getStnList <- function(active=TRUE){
  x = "http://www.cimis.water.ca.gov/cimis/frontStationListInfo.do"

  y = readHTMLTable(x)[[8]][-c(1:4),]
  rownames(y) = seq(nrow(y))
  names(y) = c("Station", "Name", "County", "Status")

  if(active){
    return(y[y$Status == "Active",])
  }
  y
}
