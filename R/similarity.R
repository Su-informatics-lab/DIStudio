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
#' @param PD A logical scalar. If PD is true, weight take power of 2.
#' @param View A logical scalar. If View is true, the drug lists will be visualized.
#' @return a network similarity. 
#' @export
Similarity <- function(model, cui1, cui2, weight=NULL, PD=F, View=F) {
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

      # TODO: Extract visualization for better separation of concerns
      if (View) {
         par(mar=c(1,1,1,1), mfrow=c(2,2))

         plot(igraph::induced_subgraph(net, nodes1), main = cui1)
         plot(igraph::induced_subgraph(net, nodes2), main = cui2)
         plot(igraph::induced_subgraph(net, Int), main = "Intersection")
         plot(igraph::induced_subgraph(net, Uni), main = "Union")
      }
   } else {
      F1<- Frequencies(model, cui1)
      F2<- Frequencies(model, cui2)

      nodes1 <- F1$vertices$code[F1$vertices$count>0]
      nodes2 <- F2$vertices$code[F2$vertices$count>0]

      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)
      count12 <- F1$vertices$count + F2$vertices$count

      if (View)  {
         F1.G <- igraph::induced_subgraph(net, F1$vertices$code[F1$vertices$count >0])
         F2.G <- igraph::induced_subgraph(net, F2$vertices$code[F2$vertices$count >0])
         F12.G <- igraph::induced_subgraph(net, F1$vertices$code[count12 >0])

         igraph::V(F1.G)$size <- ceiling(F1$vertices$count[F1$vertices$count >0]/F1$length *15)
         igraph::E(F1.G)$arrow.size <- .4
         plot(F1.G, main = "cui1")

         igraph::V(F12.G)$size <- ceiling(count12[count12>0]/(F1$length+F2$length) *15)
         igraph::E(F12.G)$arrow.size <- .4
         plot(F12.G, main = "cui1 + cui2")

         igraph::V(F2.G)$size <- ceiling(F2$vertices$count[F2$vertices$count >0]/F2$length *15)
         igraph::E(F2.G)$arrow.size <- .4
         plot(F2.G, main = "cui2")
      }
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

#jaccard similarity 
J_sim <- function(x=x,Y =Y) {
   l1_x <- sum(x)

   if (is.matrix(Y)) {
      d_xY <- apply(abs(sweep(Y, 2, x)), 1, sum)
      l1_Y<- apply(Y, 1, sum)
   } else {
      d_xY <- sum(abs(x-Y))
      l1_Y<- sum(Y)
   }

   return((l1_x+l1_Y-d_xY)/(l1_x+l1_Y+d_xY))
}

#' jaccard similarity based on frequency
#' TODO: Function description
#' @export
Jaccard.F <- function(F1,F2) {
   idx <- union(which(F1$vertices$count >0), which(F2$vertices$count >0))
   return(J_sim(F1$vertices$count[idx], F2$vertices$count[idx]))
}
