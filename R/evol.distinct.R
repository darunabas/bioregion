#' A function to calculate evolutionary distinctiveness of bioregions.
#'
#' The \code{evoldistinct.bioregion} function estimates evolutionary
#' distinctiveness of each bioregion by computing the mean value of
#' phylogenetic beta diversity between a focal bioregion and all other
#' bioregions in the study area.
#'
#' @param P A distance matrix
#'
#' @rdname evoldistinct.bioregion
#'
#' @keywords cluster
#' @export
#' @return
#' \item{psim}{A site × site phylogenetic beta diversity distance matrix}
#' @references
#' \insertRef{maurin2014savanna}{bioregion}
#'
#' \insertRef{daru2016novel}{bioregion}
#'
#' \insertRef{daru2017phylogenetic}{bioregion}
#'
#' \insertRef{daru2017understanding}{bioregion}
#'
#' @author Barnabas H. Daru \email{darunabas@@gmail.com} & Klaus Schliep
#' @seealso \code{\link[picante]{evol.distinct}}
#'
#' @examples
#' example(phylo.beta.pair)
#' evol.distinct(P)
#'
#' #2. example




evoldistinct.bioregion <- function(P){

  P1 <- as.dist(P)

  # UPGMA was retrieved as the best algorithm
  P2 <- hclust(P1, method="average")

  ## determine a "good" k using elbow
  P3 <- css.hclust(P1, P2)
  k <- elbow.batch(P3)[[1]]

  g <- cutree(P2, k)

  d <- data.frame(phyloreg=g)
  d$grids <- rownames(d)

  ## pbsim
  ## make a region by region distance matrix based on mean distances between gridcells

  P <- as.matrix(P)
  colnames(P) <- rownames(P)

  region.mat <- matrix(NA, k, k, dimnames = list(1:k, 1:k))

  for(i in 1:k){

    for(j in 1:k){

      region.mat[i, j] <- mean(P[names(g)[g == i], names(g)[g == j]])
    }

  }
  region.dist <- as.dist(region.mat)
  region.mat <- as.matrix(region.dist) #fixes diag

  # TO COMPUTE EVOLUTIONARY DISTINCTIVENESS:

  evol_distinct <- colSums(region.mat)/(nrow(region.mat)-1)

  evol_distinct <- data.frame(ED=evol_distinct)
  evol_distinct$phyloreg <- rownames(evol_distinct)

  evol_distinct
}
