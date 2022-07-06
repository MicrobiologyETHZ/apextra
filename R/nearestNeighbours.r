

#' Function to find the nearest neighbour(s) to a provided set of tips
#'
#' @param phylo An object of class "phylo".
#' @param tips A numeric vector indicating the tips to start from.
#' @param n A numeric indicating the number of nearest neighbours to return.
#' @keywords None
#' @return An object of class "phylo".
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

nearestNeighbours <- function(phylo,tips,n=1){
    dm <- cophenetic(tree)
    for(i in 1:n){
        nn <- sapply(tips,function(x) order(dm[x,])[1+1:n])
    }
    return(unique(as.vector(nn)))
}
