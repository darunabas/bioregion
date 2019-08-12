#' Gets the descendant nodes of a phylogeny at a given time depth.
#'
#' \code{getClusters} returns the tips that descend from a given node or time depth on a dated phylogenetic tree.
#'
#' @param tree is a dated phylogenetic tree with branch lengths stored as a 
#'   phylo object (as in the \code{ape} package).
#' @rdname getClusters
#' @keywords bioregion
#' @importFrom phangorn Descendants
#' @importFrom ape node.depth.edgelength 
#'
#' @return
#' \item{}{A list of descendants}
#'
#' @references
#' \insertRef{Schliep2010}{bioregion}
#'
#' @examples
#' require(ape)
#' geo <- get(data(geospiza))
#' tree <- geo$phy
#' tree <- midpoint(tree)
#' plot(tree)
#' axisPhylo(side = 1)
#' getClusters(tree, .3)
#' @export
getClusters <- function(tree, cut=2, ...){
  nh <- node.depth.edgelength(tree)
  nh <- max(nh) - nh
  ind <- which( (nh[tree$edge[,1]] > cut) & (nh[tree$edge[,2]] < cut) )
  desc <- Descendants(tree)
  res <- desc[tree$edge[ind,2]]
  lapply(res, function(res, tips)tips[res], tree$tip.label)
}

