#' Creating fishnet of polygons
#'
#' The \code{fishnet} function enables the process in which species occurrence
#' data and ecological and environmental variables are modeled together to
#' predict species ranges.
#'
#' @param files A community matrix
#' @rdname fishnet
#' @keywords bioregion
#' @importFrom raster raster rasterToPolygons xyFromCell ncell
#' @importFrom raster values extent
#' @importFrom sp CRS proj4string
#'
#' @export
#' @return
#' \item{psim}{A site × site phylogenetic beta diversity distance matrix}
#'
#' @references
#'
#' \insertRef{Philips2006}{bioregion}
#' @importFrom Rdpack reprompt
#' @examples
#' library(raster)
#' file <- system.file("nigeria/nigeria.rds", package="bioregion")
#' d <- readRDS(file)
#' d1 <- fishnet(d, res = 0.75)
fishnet <- function(shp, res = 0.5){
  s <- raster(extent(shp))
  res(s) <- res
  proj4string(s) <- proj4string(shp)
  m <- rasterToPolygons(s)
  dd <- raster::intersect(shp, m)
  dd
}

