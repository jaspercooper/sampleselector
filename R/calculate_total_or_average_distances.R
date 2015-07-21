#' Calculate the total or average distance between all units for each set in a sample_selection object
#'
#' @param sample_selection_object A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)
#' @param distance_matrix A distance matrix, see function create_distance_matrix()
#' @param total If total = TRUE, the total distance between all units is returned, if total=FALSE, the average distance between all units is returned
#' @export


calculate_total_or_average_distances <- function(sample_selection_object, distance_matrix, total = TRUE) {
  
  if(!class(sample_selection_object)=="sample_selection")
    stop("sample_selection_object must be a sample_selection object, created with a create_set() function.")
  
  set_total_distances <- sapply(sample_selection_object, function(set, distance_matrix) {
    return(sum(distance_matrix[set,set])/2)
  }, distance_matrix)
  
  if (total == FALSE){
  
    set_average_distances <- sapply(1:length(sample_selection_object), function(set_number, set_total_distances) {
      set_total_distances[set_number]/(0.5*length(sample_selection_object[[set_number]])^2)
    }, set_total_distances)

    return(sort(set_average_distances))
  }
  
  return(sort(set_total_distances))
}