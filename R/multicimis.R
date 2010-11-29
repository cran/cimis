mcimishourly <- function(stationlist, label = "none") {

  if(label %in% c("none", "station", "name") == 0) {
    stop("Invalid 'add' parameter. Leave as default of see ?mcimishourly")
  }
  if(is.numeric(stationlist)){
    stationlist <- sprintf("%03i", stationlist)
  }

  
  #remove redundant stations
  stationlist <- unique(stationlist)
  
  if(length(stationlist) == 1) {
    return(cimishourly(stationlist))
  }
  #presort
  stationlist = sort(stationlist)

  w <- lapply(stationlist, cimishourly)
  names(w) <- stationlist

  elen <- sapply(w, nrow)
  

  wout <- do.call("rbind", w)

  if(label == "station") {
    wout$station <- as.factor(c(mapply(rep, names(w), elen)))
    
  } else if(label == "name") {
    sl = getStnList()
    wout$station <- as.factor(c(mapply(rep, sl$Name[sl$Station %in% 
            stationlist], elen)))


  } 
  return(wout)
}
     
mcimisannual <- function(stationlist, yearlist, label = "none", ...) {
  #... additional arguments to cimisannual - units and keepfile

  if(label %in% c("none", "station", "name") == 0) {
    stop("Invalid 'add' parameter. Leave as default of see ?mcimishourly")
  }

  if(is.numeric(stationlist))
    stationlist <- sprintf("%03i", stationlist)

  #presort
  stationlist = sort(stationlist)

  n <- length(stationlist)
  m <- length(yearlist)

  if(n == 1 & m == 1) {
    return(cimisannual(stationlist, yearlist, ...))
  }

  cappend <- function(station, year, label = label, ...){
    w <- cimisannual(station, year, ...)

    if(label == "station") {

      w$station <- station
      return(w)
      
    } else if(label == "name") {

      sl = getStnList()
      w$station <- factor(sl$Name[sl$Station == station])
      return(w)
    }

    return(w)
  }

  comb <- expand.grid(stationlist, yearlist)

  w <- apply(comb, 1, function(x)cappend(x[1], x[2], label = label, ...))
  wout <- do.call("rbind", w)
  return(wout)
  
    
}
    
