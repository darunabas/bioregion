SpecRich <- function(x){
  mm1 <- data.frame(table(x$grids))
  names(mm1) <- c("grids", "SR")
  mm1
}



#' Read in sparse community matrices
#'
#' \code{weighted.endemism} is species richness inversely weighted
#' by species ranges.
#'
#' @param files A community matrix
#' @rdname weighted.endemism
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
#' \insertRef{Crispetal2001}{bioregion}
#'
#' \insertRef{LaffanCrisp2003}{bioregion}
#'
#' @examples
#' fdir <- system.file("Aloes", package="bioregion")
#' files <- file.path(fdir, dir(fdir))
#' res <- raster2comm(files)
weighted.endemism <- function(x){
  # browser()
  xx <- SpecRich(x)
  index <- match(x$grids, xx$grids)
  SR <- xx$SR[index]
  ff <- table(x$species)
  x$WE <- SR/ff[x$species]
  xx <- as.data.table(x)
  #browser()
  res <- xx[, sum(WE), by=grids]
  #res <- x %>% group_by(grids) %>% summarise(W_Endemism = sum(WE))
  res
}


