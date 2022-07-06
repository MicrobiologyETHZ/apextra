

#' Function to determine the 'real' order of tips in a phylo object, which can change when a tree is rerooted.
#'
#' @param phylo An object of class "phylo".
#' @details
#' None
#' @keywords None
#' @return A vector with the tip numbers 1 to N in the order they are in the tree.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

tipOrder <- function(phylo){
    isTip <- phylo$edge[,2] <= Ntip(phylo)
    ord <- phylo$edge[isTip,2]
    return(ord)
}
