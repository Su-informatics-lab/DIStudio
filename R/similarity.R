#' Count annotation concept frequency 
#' 
#' Compute the annotation concept (of one kind) frequency of a model from a given list of drug concept vectors
#'
#' 
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param cui1.list A list of vectors containing a sequence of drug concepts
#' @param cui.counts Counter of all drug concepts if Counts presented  
#' @param Counts A frequency table to be added to. Default is Null. 
#' @return A data frame of the annotation concepts and their frequency (Counts).  
#' @export
#' 
Frequencies <- function(model, cui.list, cui.counts=NULL, Counts=NULL) {
   net <- model$graph
   kinds <- unique(igraph::V(net)$kind)

   if (length(kinds) > 1) {
      kinds <- kinds[!(kinds == "C8")]
   }

   if (is.null(Counts)) { 
      Counts <- igraph::as_data_frame(igraph::induced_subgraph(net, igraph::V(net)[igraph::V(net)$kind == kinds]), "both")
      Counts$vertices$count <- 0
      Counts$edges$count <- 0
      Counts$edges$fromto <- paste0(Counts$edges$from, Counts$edges$to)
      Counts$length <- 0
   }

   if (is.null(cui.counts)) {
      cui.counts <- rep(1, length(cui.list))
   }

   for (i in 1:length(cui.list)) {
      NodeArc <- igraph::as_data_frame(find_nodes(cui.list[i], net, UD="up", kinds, SubGraph=T), "both")

      if (!is.na(NodeArc$vertices[1, 1])){
         NodeArc$edges$fromto <- paste0(NodeArc$edges$from, NodeArc$edges$to)

         NodeMatch <- match(NodeArc$vertices$code, Counts$vertices$code)
         Counts$vertices$count[NodeMatch] <- Counts$vertices$count[NodeMatch]  + cui.counts[i]

         ArcMatch <- match(NodeArc$edges$fromto, Counts$edges$fromto)
         Counts$edges$count[ArcMatch] <- Counts$edges$count[ArcMatch] + cui.counts[i]

         Counts$length <- Counts$length + 1
      }
   }


   return(Counts)
}


## two CUIs from same kinds no need to consider arcs -- need error prevention later 
## or two list of CUIS -- count only by nodes same as two CUIs similarity 

#' Compute Similarity based on nodes  
#'
#' Compute node similarity between two drug lists. vectors containing a sequence of drug concepts
#' 
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param cui1,cui2 Vectors containing a sequence of drug concepts 
#' @param weight A data.frame of of all notes' weights 
#' @param PD A logical scalar. IF PD is true, weight take power of 2.
#' @return a network similarity. 
#' @export
Similarity <- function(model, cui1, cui2, weight=NULL, PD=F) {
   validateModel(model)

   # Get just the Drug & Mechanism of Action portion of the model
   model <- getKindsSubmodel(model, c(DRUG_KIND, MECHANISM_OF_ACTION_KIND))
   net <- model$graph

   if (length(cui1) == 1 & length(cui2) == 1) {
      kinds <- unique(igraph::V(net)$kind)
      if (length(kinds) > 1) {
         kinds <- kinds[!(kinds == DRUG_KIND)]
      }

      nodes1  <- find_nodes(cui1, net, UD="up", kinds, SubGraph=F, NodeOnly=T)
      nodes2  <- find_nodes(cui2, net, UD="up", kinds, SubGraph=F, NodeOnly=T)

      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)

   } else {
      F1<- Frequencies(model, cui1)
      F2<- Frequencies(model, cui2)

      nodes1 <- F1$vertices$code[F1$vertices$count>0]
      nodes2 <- F2$vertices$code[F2$vertices$count>0]

      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)
   } 

   if (is.null(weight)) {
      return(length(Int)/length(Uni))
   } else { 
      # TODO: Validate the weight data frame

      if(!PD) {
         return(sum(weight$dist[match(Int, weight$code)])/sum(weight$dist[match(Uni, weight$code)]))
      } else {
         return(sum(2^weight$dist[match(Int, weight$code)])/sum(2^weight$dist[match(Uni, weight$code)]))
      }
   }
}
