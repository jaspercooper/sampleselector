#' Select that are some minimum distance from one another.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
degrees_to_radians <- function(x){
     return(x * pi / 180)
}

# Calculate spherical distance between two points on earth
haversine_distance <- function(long1, lat1, long2, lat2, km = TRUE) {
     R <- 6371 # Earth mean radius in km
     R <- 3959
     delta.long <- deg.to.rad(long2 - long1)
     delta.lat <- deg.to.rad(lat2 - lat1)
     a <- sin(delta.lat/2)^2 + cos(deg.to.rad(lat1)) * cos(deg.to.rad(lat2)) * sin(delta.long/2)^2
     c <- 2 * atan2(sqrt(a),sqrt(1-a))
     d = R * c*1000
     return(c(distance = d)) # Distance in meters
}
