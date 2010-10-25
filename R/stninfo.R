stninfo <-
  #retrieves station info - lat, long, elev, from CIMIS
  function(station){

    base = "http://wwwcimis.water.ca.gov/cimis/frontStationDetailData.do"
    d = getForm(base, stationId = station)
    dd = htmlParse(d, asText = TRUE)
    res = readHTMLTable(dd)[[11]]
    lat = as.numeric(gsub(".* / (.*)", "\\1", as.character(res[2,2])))
    long = as.numeric(gsub(".* / (.*)", "\\1", as.character(res[3,2])))
    elev = as.numeric(as.character(res[1,2]))
    ret = c(long = long, lat = lat, elev = elev)
    attr(ret, "station") = station
    return(ret)
  }
