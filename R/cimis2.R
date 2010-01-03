#fix NA value lines in cimishourly
cimishourly = function(station, units = "imp")
{
  dir = c(imp = "ftp://ftpcimis.water.ca.gov/pub/hourly/",
          SI = "ftp://ftpcimis.water.ca.gov/pub/hourlyMetric/")
  base = c(imp = "hourly", SI = "hlymet")
  cat("Downloading: ", paste(dir[units], base[units], station, ".csv", sep = ""), "\n")

  weather = read.csv(paste(dir[units], base[units], station, ".csv", sep = ""),
           header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                       '#######', '#####'),
           colClasses = c("numeric", "character", "numeric", "numeric",
             "character", "numeric", "character", "numeric", "character",
             "numeric", "character", "numeric", "character", "numeric",
             "character", "numeric", "character", "numeric", "character",
             "numeric", "character", "numeric", "character",
             "numeric"))[, c(2,3,4,6,8,10,12,14,16,18,20,22,24)]

  colnames(weather) <- c('date', 'time', 'julian_day','eto', 'precip',
                         'solar_rad', 'vapor_press', 'air_temp',
                         'rel_humidity', 'dew_pt', 'wind_speed',
                         'wind_dir', 'soil_temp')
  weather$hour <- as.numeric(sprintf("%02i", (weather$time/100) - 1))
  weather$datetime <- as.POSIXct(strptime(paste(weather$date, weather$hour),
                                          format='%m/%d/%Y %H'))
  weather$year = as.POSIXlt(weather$datetime)$year+1900

  return(weather)
}


cimisdaily = function(station, units = "imp")
{
  dir = c(imp = "ftp://ftpcimis.water.ca.gov/pub/daily/",
          SI = "ftp://ftpcimis.water.ca.gov/pub/dailyMetric/")
  base = c(imp = "daily", SI = "dlymet")
  cat("Downloading: ", paste(dir[units], base[units], station, ".csv", sep = ""), "\n")

  weather = read.csv(paste(dir[units], base[units], station, ".csv", sep = ""),
           header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                       '#######', '#####'),
    colClasses = c("numeric", "character", "numeric", "character",
    "numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character",
      "numeric"))[, c(2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31)]

                  
  colnames(weather) <- c('date', 'julian_day', 'solar_rad', 'soil_avg',
                         'max_air', 'min_air', 'avg_air', 'avg_vap',
                         'avg_wind', 'precip', 'max_rh', 'min_rh',
                         'eto', 'avg_rh', 'dew_pt', 'wind_dir')

  weather$datetime <- as.POSIXct(strptime(paste(weather$date),
                                          format='%m/%d/%Y'))
  weather$year = as.POSIXlt(weather$datetime)$year+1900

  return(weather)
}

cimisannual = function(station, year, period = "daily", units = "imp",
  keepfile=TRUE){

  if(units != "imp") stop("SI Units not yet implemented for annual data. Sorry -GH")

  dir = c(imp = "ftp://ftpcimis.water.ca.gov/pub/annual/",
          SI = "ftp://ftpcimis.water.ca.gov/pub/annual/")

  file = paste(dir[units], period, "Stns", year, ".zip", sep="")
  innerfile = paste(year, period, station, ".csv", sep="")

  shortfile = paste(period, "Stns", year, ".zip", sep="")
  

    #check for local copy
  if(shortfile %in% dir() == FALSE){
    cat("Downloading: ", dir[units], period, "Stns", year, ".zip \n", sep="")
    download.file(file, destfile = shortfile)
  }

  else{
    cat("File already exists at: ", list.files(getwd(),
                                               full=TRUE, pattern=shortfile), "\n")
  
      }

  con = unz(shortfile, innerfile)

  if(period == "daily"){

    weather = read.csv(con,
      header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                  '#######', '#####'),
      colClasses = c("numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character",
        "numeric"))[, c(2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31)]

    
    colnames(weather) <- c('date', 'julian_day', 'solar_rad', 'soil_avg',
                           'max_air', 'min_air', 'avg_air', 'avg_vap',
                           'avg_wind', 'precip', 'max_rh', 'min_rh',
                           'eto', 'avg_rh', 'dew_pt', 'wind_dir')

    weather$datetime <- as.POSIXct(strptime(paste(weather$date),
                                            format='%m/%d/%Y'))
    weather$year = as.POSIXlt(weather$datetime)$year+1900

    return(weather)
  }

  if(period == "hourly"){
    weather = read.csv(con,
      header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                  '#######', '#####'),
      colClasses = c("numeric", "character", "numeric", "numeric",
        "character", "numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character",
        "numeric"))[, c(2,3,4,6,8,10,12,14,16,18,20,22,24)]

    colnames(weather) <- c('date', 'time', 'julian_day','eto', 'precip',
                           'solar_rad', 'vapor_press', 'air_temp',
                           'rel_humidity', 'dew_pt', 'wind_speed',
                           'wind_dir', 'soil_temp')
    weather$hour <- as.numeric(sprintf("%02i", (weather$time/100) - 1))
    weather$datetime <- as.POSIXct(strptime(paste(weather$date, weather$hour),
                                            format='%m/%d/%Y %H'))
    weather$year = as.POSIXlt(weather$datetime)$year+1900

    return(weather)
  }
  
  close(con)

  if(keepfile == FALSE){  
    file.remove(shortfile)
  }
    
}

cimismonthly = function(station, month, period = "daily", units = "imp",
  keepfile = TRUE){

  dir = c(imp = "ftp://ftpcimis.water.ca.gov/pub/monthly/",
          SI = "ftp://ftpcimis.water.ca.gov/pub/monthlyMetric/")

  file = paste(dir[units], period, "Stns", month, ".zip", sep="")
  innerfile = paste(month, period, station, ".csv", sep = "")

  shortfile = paste(period, "Stns", month, ".zip", sep="")

      #check for local copy
  if(shortfile %in% dir() == FALSE){
    cat("Downloading: ", dir[units], period, "Stns", month, ".zip \n", sep="")
    download.file(file, destfile = shortfile)
  }

  else{
    cat("File already exists at: ", list.files(getwd(),
                                               full=TRUE, pattern=shortfile), "\n")
  
      }

  con = unz(shortfile, innerfile)

  if(period == "daily"){

    weather = read.csv(con,
      header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                  '#######', '#####'),
      colClasses = c("numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character",
        "numeric"))[, c(2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31)]

    
    colnames(weather) <- c('date', 'julian_day', 'solar_rad', 'soil_avg',
                           'max_air', 'min_air', 'avg_air', 'avg_vap',
                           'avg_wind', 'precip', 'max_rh', 'min_rh',
                           'eto', 'avg_rh', 'dew_pt', 'wind_dir')

    weather$datetime <- as.POSIXct(strptime(paste(weather$date),
                                            format='%m/%d/%Y'))
    weather$year = as.POSIXlt(weather$datetime)$year+1900

    return(weather)
  }

  if(period == "hourly"){
    weather = read.csv(con,
      header=FALSE, na.strings=c('       --', "--", "*", '  #######',
                  '#######', '#####'),
      colClasses = c("numeric", "character", "numeric", "numeric",
        "character", "numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character", "numeric",
        "character", "numeric", "character", "numeric", "character",
        "numeric", "character", "numeric", "character",
        "numeric"))[, c(2,3,4,6,8,10,12,14,16,18,20,22,24)]

    colnames(weather) <- c('date', 'time', 'julian_day','eto', 'precip',
                           'solar_rad', 'vapor_press', 'air_temp',
                           'rel_humidity', 'dew_pt', 'wind_speed',
                           'wind_dir', 'soil_temp')
    weather$hour <- as.numeric(sprintf("%02i", (weather$time/100) - 1))
    weather$datetime <- as.POSIXct(strptime(paste(weather$date, weather$hour),
                                            format='%m/%d/%Y %H'))
    weather$year = as.POSIXlt(weather$datetime)$year+1900

    return(weather)
  }
  
  close(con)

  if(keepfile == FALSE){  
    file.remove(shortfile)
  }
}
