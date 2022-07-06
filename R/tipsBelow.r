

#' Function to find the tips below a provided set of nodes
#'
#' @param phylo An object of class "phylo".
#' @param nodes A numeric vector indicating the nodes to start from.
#' @keywords None
#' @return An object of class "phylo".
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

tipsBelow <- function(phylo, nodes, named=FALSE){
    tips <- vector()
    depth <- max(node.depth(phylo,method=2)[nodes])
    for(i in 1:depth){
        tips <- c(tips,lowerNodes(phylo,nodes))
        nodes = tips
    }
    tips <- unique(tips[tips<Ntip(phylo)])
    if(named){
        tips <- phylo$tip.label[tips]
    }
    return(tips)
}
