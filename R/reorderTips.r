

#' Function to reorder the tip labels of a tree to match the order in which they are plotted, which can change when a tree is rerooted.
#'
#' @param phylo An object of class "phylo".
#' @details
#' First the internal labels in the edges of the tree are relabelled to appear in ascending order, then the tip labels are reordered to match the correct internal labels.
#' @keywords None
#' @return An object of class "phylo"
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

reorderTips <- function(phylo){
    reord <- phylo
    reord$edge[reord$edge[,2]<=Ntip(reord),2] <- 1:Ntip(reord)
    reord$tip.label <- phylo$tip.label[tipOrder(phylo)]
    return(reord)
}
