## This function turns POSIX formatted date-times into radian time
posix2radian <- function(x) {

  if (is.POSIXt(x) == F) {

    stop(simpleError("Time must be in POSIXct or POSIXt format"))

  } else {

    s <- second(x)
    m <- minute(x)
    h <- hour(x)

    rad_time <- ((h + (m + (s / 60)) / 60) / 24) * 2 * pi

    return(rad_time)
  }
}
