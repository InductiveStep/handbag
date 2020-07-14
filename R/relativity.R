

#' Returns a multiple (by default, 1) of the speed of light in m/s
#'
#' @param m Multiple of the speed of light
#' @return Returns m * the speed of light in m/s
#' @examples
#' light_speed() # the speed of light in m/s
#' light_speed(0.5) # half the speed of light in m/s
#' @export
light_speed <- function(m = 1) {
  299792458 * m  # m/s
}


#' Calculates dilated time
#' 
#' Calculates dilated time (perspective of stationary observer)
#' from proper time experienced on object moving at a particular
#' speed
#' 
#' @param proper_time The time on the object. Whatever units
#' are chosen here will also be the units of the results.
#' @param speed Speed of moving object in m/s
#' @return Returns the dilated time in units chosen for proper_time
#' @examples
#' curve(dilated_time(1, light_speed(x)), 0, .99,
#'       xlab = "Speed (as proprtion of speed of light)",
#'       ylab = "Dilated time",
#'       main = "Dilated time when proper time = 1")
#' @export
dilated_time <- function(proper_time, speed) {
  proper_time / sqrt(1 - (speed^2 / light_speed()^2))
}


#' Calculates proper time
#' 
#' Calculates proper time (perspective of moving object)
#' from time as measured on a stationary object
#' 
#' @param dilated_time The time on the stationary object. Whatever units
#' are chosen here will also be the units of the results.
#' @param speed Speed of moving object in m/s
#' @return Returns the proper time in units chosen for dilated_time
#' @export
proper_time <- function(dilated_time, speed) {
  dilated_time * sqrt(1 - (speed^2 / light_speed()^2))
}