#' Read shapefiles and check scores have a matching row
#'
#' @importFrom sf read_sf
#'
#' @author Finlay Campbell
#'
read_shape <- function(file, scores) {

  shape <- read_sf(file)
  placevar <- names(scores)[1]

  if(!placevar %in% names(shape))
    stop(glue("The column '{placevar}' is not found in shapefile"))

  missing <- setdiff(scores[[1]], shape[[placevar]])
  if(length(missing) > 0)
    stop(glue("The following places are missing in shapefile: {paste0(missing, collapse = ", ")}"))

  return(shape)

}
