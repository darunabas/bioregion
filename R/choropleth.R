#' Binning shapefile polygons based upon a slot value
#'
#' The \code{choropleth} function discretizes the values of a quantity based 
#' on their quantiles.
#' 
#' @param P A site × site phylogenetic beta diversity distance matrix
#' @param \dots Further arguments passed to or from other methods.
#' @rdname choropleth
#'
#' @keywords cluster
#' @export
#' @return
#' \item{choropleth}{A dataframe with evolutionary distinctiveness of each
#' phyloregion}
#' @examples
#' library(raster)
#' file <- paste0(system.file(package="bioregion"), "/inst/SppRich_NG/SR_Nigeria.shp")
#' s <- shapefile(file)
#' k=10
#' COLOUR <- terrain.colors(k)
#' y = choropleth(s, k)
#' plot(y, col=COLOUR[y$SR])
#'
choropleth <- function(x, k=10, ...){
  quants <- quantile(x$SR, seq(0,1, length.out = k+1))
  SR <- x$SR
  l = length(quants) -1
  col_vec <- numeric(length(x$SR))
  col_vec[SR==quants[1]] <- 1
  for(i in seq_len(l)){
    col_vec[SR>quants[i] & SR<= quants[i+1] ] = i
  }
  x$SR <- col_vec
  x
}


