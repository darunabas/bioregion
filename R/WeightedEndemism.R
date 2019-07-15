SpecRich <- function(x){
  mm1 <- data.frame(table(x$grids))
  names(mm1) <- c("grids", "SR")
  mm1
}



#' Measures the distribution of narrow-ranged or endemic species.
#'
#' \code{weighted.endemism} is species richness inversely weighted
#' by species ranges.
#'
#' @param files A community matrix or data frame.
#' @rdname weighted.endemism
#' @keywords bioregion
#' @importFrom raster raster rasterToPolygons xyFromCell ncell
#' @importFrom raster values
#' @importFrom sp CRS proj4string
#' @importFrom data.table as.data.table
#'
#' @return
#' \item{}{A data frame of species traits by site}
#'
#' @references
#' \insertRef{Crispetal2001}{bioregion}
#'
#' \insertRef{LaffanCrisp2003}{bioregion}
#'
#' @examples
#' require(data.table)
#' fdir <- system.file("NGAplants", package="bioregion")
#' files <- file.path(fdir, dir(fdir))
#' dat <- data.frame(raster2comm(files))
#' Endm <- weighted.endemism(dat)
#' @export
weighted.endemism <- function(x){
  tmp <- SpecRich(x)
  index <- match(x$grids, tmp$grids)
  SR <- tmp$SR[index]
  ff <- table(x$species)
  x$WE <- SR/ff[x$species]
  tmp <- as.data.table(x)
  res <- tmp[, sum(WE), by=grids]
  res
}



