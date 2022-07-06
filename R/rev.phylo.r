

#' Function to reverse the structure of a phylogeny for plotting purposes.
#'
#' @param phylo An object of class "phylo".
#' @keywords None
#' @return An object of class "phylo".
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

rev.phylo <- function(phylo){
    new <- reorderTips(rotateConstr(phylo,rev(phylo$tip.label)))
    return(new)
}
