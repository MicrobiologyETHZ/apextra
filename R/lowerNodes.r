

#' Function to find the nodes one down from a provided set of nodes
#'
#' @param phylo An object of class "phylo".
#' @param nodes A numeric vector indicating the nodes to start from.
#' @keywords None
#' @return An object of class "phylo".
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

lowerNodes <- function(phylo,nodes){
        return(unique(phylo$edge[phylo$edge[,1]%in%nodes,2]))
}
