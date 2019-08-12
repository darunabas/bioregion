#' Binning shapefile polygons based on slot values
#'
#' The \code{choropleth} function discretizes the values of a quantity based 
#' on their quantiles.
#' 
#' @param x An object of the class SpatialPolygonsDataFrame.
#' @param values The column in the SpatialPolygonsDataFrame for which to discretize 
#' the values of the quantity.
#' @param k Numeric, the desired number of bins to discretize.
#' @param \dots Further arguments passed to or from other methods.
#' @rdname choropleth
#' @keywords cluster
#' @export
#' @return
#' \item{choropleth} {returns a SpatialPolygonsDataFrame with a column with the 
#' discretized values}
#' @examples
#' require(raster)
#' s <- readRDS(system.file("nigeria/SR_Naija.rds", package= "bioregion"))
#' k=10
#' COLOUR <- colorRampPalette(c("blue", "yellow", "red"))(k)
#' y = choropleth(s, values=s$SR, k)
#'  
#' ## Not run: 
#' plot(y, col=COLOUR[y$values], border = NA)
#'  
#' ## End(Not run)
#'
choropleth <- function(x, values, k=10, ...){
  x$values <- values
  quants <- quantile(values, seq(0,1, length.out = k+1))
  l = length(quants) -1
  col_vec <- numeric(length(values))
  col_vec[values==quants[1]] <- 1
  for(i in seq_len(l)){
    col_vec[values>quants[i] & values<= quants[i+1] ] = i
  }
  x$values <- col_vec
  x
}


