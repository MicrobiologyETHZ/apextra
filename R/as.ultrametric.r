#' Function to extend the terminal branches of a phylo object so that the resulting tree is ultrametric.
#'
#' @param phylo An object of class "phylo" that is rooted.
#' @details
#' The final root-to-tip distance of each tip will be that of the furthest from the root.
#' @keywords None
#' @return An object of class "phylo", rooted and ultrametric
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

as.ultrametric <- function(phylo){
    if(!is.rooted(phylo)){
        stop("phylo object is not rooted!")
    }
    rtt <- diag(vcv(phylo))
    extra <- max(rtt)-rtt
    isTip <- phylo$edge[,2] <= Ntip(phylo)
    phylo$edge.length[isTip] <- phylo$edge.length[isTip]+extra
    return(phylo)
}
