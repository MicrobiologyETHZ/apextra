

#' Function to find the nodes one level up from a provided set of nodes
#'
#' @param phylo An object of class "phylo".
#' @param nodes A numeric vector indicating the nodes to start from. Tips are also nodes, numbered 1:N.
#' @keywords None
#' @return An object of class "phylo".
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

higherNodes <- function(phylo,nodes){
    return(unique(phylo$edge[phylo$edge[,2]%in%nodes,1]))
}
