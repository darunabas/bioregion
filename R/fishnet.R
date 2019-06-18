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
#' @importFrom raster values
#' @importFrom sp CRS proj4string<-
#'
#' @export
#' @return
#' \item{psim}{A site × site phylogenetic beta diversity distance matrix}
#'
#' @references
#'
#' \insertRef{Philips2006}{bioregion}
#'
#' @examples
#' library(raster)
#' file <- paste0(system.file(package="bioregion"), "/inst/nigeria/nigeria.shp")
#' d <- shapefile(file)
#' d1 <- fishnet(d, res = 0.75)
fishnet <- function(shp, res=0.5){
  s <- raster(extent(shp))
  res(s) <- res
  proj4string(s)<-proj4string(shp)
  m <- rasterToPolygons(s)
  dd <- intersect(shp, m)
  dd
}

