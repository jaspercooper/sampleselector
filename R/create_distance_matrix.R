#' Calculate the distance of each unit to every other unit.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)







dist.mat.gen <- function(latlon.orgs,latlon.dests){


     dist.gen <- function(latlon.org,latlon.dests){

          dists <- adply(.data = latlon.dests,
                         .margins = 1,
                         .fun = function(i){
                              return(haversine(lat1  = latlon.org[1],
                                               long1 = latlon.org[2],
                                               lat2  = i[1],
                                               long2 = i[2]))
                         })[,2]

          return(dists)

     }

     dist.mat <- adply(.data = latlon.orgs,.margins = 1,.fun = function(row.slice){
          dist.gen(latlon.org = row.slice,
                   latlon.dests = latlon.dests
          )
     })[,-1]

     return(dist.mat)

}
