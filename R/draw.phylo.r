

#' Function to draw a phylogram on an existing plot in an arbitrary position with arbitrary scale.
#'
#' @param x1,y1,x2,y2 Coordinates of the rectangle that will contain the phylogram.
#' @param phylo An object of class "phylo".
#' @param direction A character string specifying the direction in which the phylogram will be drawn; it must be one of "r"ightwards (default), "l"eftwards, "u"pwards or "d"ownwards.
#' @param mirror A logical indicating whether the phylogeny should be plotted in reverse order, i.e.: mirrored.
#' @param show.tip.label A logical indicating whether tip labels should be drawn; defaults to FALSE. The labels will be placed outside of the coordinate rectangle provided.
#' @param label.offset A numeric giving the distance between the tips and their corresponding labels, defaults to 0.
#' @param tip.color A single color or vector of colors to be used for the tip labels, defaults to black.
#' @param align.tip.label A logical indicating whether tip labels should be aligned with the furthest tip rather than each tip individually, defaults to FALSE.
#' @details
#' The phylogram is laid out such that the outermost branches lie on the edges of the coordinate rectangle, with the root and furthest tip on the other edges.
#' @keywords None
#' @return None
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None

draw.phylo <- function(x1,y1,x2,y2,phylo,direction="r",mirror=F,show.tip.label=FALSE,label.offset=0,tip.color="black",align.tip.label=FALSE){
    if(mirror){
        phylo <- rev(phylo)
    }
    if(direction%in%c("r","u")){
        nodex <- node.depth.edgelength(phylo)
        nodey <- node.height(phylo)
        nodey <- nodey-min(nodey) # because the first branch is always offset for some reason
    }else if(direction%in%c("l","d")){
        nodex <- node.depth.edgelength(phylo)
        nodex <- max(nodex)-nodex
        nodey <- node.height(phylo)
        nodey <- nodey-min(nodey)
    }else{
        stop("Direction not recognised, must be \"r\", \"l\", \"u\" or \"d\"!")
    }

    edgex1 <- c(nodex[phylo$edge[,1]],nodex[phylo$edge[,1]])
    edgey1 <- c(nodey[phylo$edge[,1]],nodey[phylo$edge[,2]])
    edgex2 <- c(nodex[phylo$edge[,1]],nodex[phylo$edge[,2]])
    edgey2 <- c(nodey[phylo$edge[,2]],nodey[phylo$edge[,2]])

    xscale <- (x2-x1)/max(edgex1,edgex2)
    yscale <- (y2-y1)/max(edgey1,edgey2)

    if(direction%in%c("r","l")){
        xscale <- (x2-x1)/max(edgex1,edgex2)
        yscale <- (y2-y1)/max(edgey1,edgey2)

        plotx1 <- x1+(xscale*edgex1)
        ploty1 <- y1+(yscale*edgey1)
        plotx2 <- x1+(xscale*edgex2)
        ploty2 <- y1+(yscale*edgey2)
    }else{
        xscale <- (x2-x1)/max(edgey1,edgey2)
        yscale <- (y2-y1)/max(edgex1,edgex2)

        plotx1 <- x1+(xscale*edgey1)
        ploty1 <- y1+(yscale*edgex1)
        plotx2 <- x1+(xscale*edgey2)
        ploty2 <- y1+(yscale*edgex2)
    }

    segments(plotx1,ploty1,plotx2,ploty2)

    if(show.tip.label){
        labelx = matrix(plotx2,ncol=2)[,2][phylo$edge[,2]<=Ntip(phylo)]
        labely = matrix(ploty2,ncol=2)[,2][phylo$edge[,2]<=Ntip(phylo)]
        if(direction=="r"){
            if(align.tip.label){
                labelx[] <- max(labelx)
            }
            tpos=4
        }else if(direction=="l"){
            if(align.tip.label){
                labelx[] <- min(labelx)
            }
            tpos=2
        }else if(direction=="u"){
            if(align.tip.label){
                labely[] <- max(labely)
            }
            tpos=3
        }else if(direction=="d"){
            if(align.tip.label){
                labely[] <- min(labely)
            }
            tpos=1
        }
        text(labelx,labely,phylo$tip.label,pos=tpos,offset=label.offset,col=tip.color)
    }
}
