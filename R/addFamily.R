#' A function to calculate evolutionary distinctiveness among phyloregions.
#'
#' The \code{addFamily} function estimates evolutionary distinctiveness of
#' each
#' phyloregion by computing the mean value of phylogenetic beta diversity
#' between a focal phyloregion and all other phyloregions in the study area..
#'
#' @param P A site × site phylogenetic beta diversity distance matrix
#' @param \dots Further arguments passed to or from other methods.
#'
#' @keywords cluster
#' @export
#' @return
#' \item{addFamily}{A dataframe with evolutionary distinctiveness of each
#' phyloregion}
#' @references
#' \insertRef{daru2017understanding}{bioregion}
#'
#' \insertRef{Holt2013}{bioregion}
#'
#' @author Barnabas H. Daru \email{darunabas@@gmail.com} & Klaus Schliep
#' @seealso \code{\link[picante]{evol.distinct}}
#'
#'
#' @examples
#' example(phylo_beta)
#' addFamily(P)
#'

# LOAD FUNCTIONS
# TAXONOMIC RESOLUTION
addFamily <- function(comm, tpl, syn){
  comm[,"species"] <- gsub("_", " ", comm[,"species"])
  sp <- unique(comm[,"species"])
  names(sp) = sp

  index <- match(sp, tpl[,"species"])
  sp_m <- sp[is.na(index)]
  #  index <- index[!is.na(index)]
  #  fam[tpl[index,"species"]] <- tpl[index,"family"]
  #  sp <- names(fam)[is.na(fam)]
  index <- match(sp_m, syn[,"synonym"])
  index <- index[!is.na(index)]
  sp[syn[index, "synonym"]] = syn[index,"species"]
  fam <- rep(NA, length(sp))
  names(fam) = sp
  index <- match(sp, tpl[,"species"])
  index <- index[!is.na(index)]
  fam[tpl[index,"species"]] <- tpl[index,"family"]
  comm[,"species"] <- sp[comm[,"species"]]
  result <- cbind(family=fam[comm[, "species"]], comm)
  rownames(result) <- NULL
  result <- result[!is.na(result[,1]),]
  result
}
