## 
Frequencies <- function(cui.list, cui.counts=NULL, net, Counts=NULL) {
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


## two CUIs from same kinds no need to consider arcs -- need eroor prevention later 
## or two list of CUIS -- count only by nodes same as two CUIs similarity 
Similarity <- function(cui1, cui2, net, weight=NULL, PD=F, View=F) {
   if (length(cui1) == 1 & length(cui2) == 1) {
      kinds <- unique(igraph::V(net)$kind)
      if (length(kinds) > 1) {
         kinds <- kinds[!(kinds == "C8")]
      }

      nodes1  <- find_nodes(cui1, net, UD="up", kinds, SubGraph=F, NodeOnly=T)
      nodes2  <- find_nodes(cui2, net, UD="up", kinds, SubGraph=F, NodeOnly=T)

      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)

      if (View) {
         par(mfrow=c(2, 2))

         plot(igraph::induced_subgraph(net, nodes1), main=cui1)
         plot(igraph::induced_subgraph(net, nodes2), main=cui2)
         plot(igraph::induced_subgraph(net, Int), main="Intersection")
         plot(igraph::induced_subgraph(net, Uni), main="Union")
      } 
   } else {
      F1<- Frequencies(cui1, net=net)
      F2<- Frequencies(cui2, net=net)

      nodes1 <- F1$vertices$code[F1$vertices$count>0]
      nodes2 <- F2$vertices$code[F2$vertices$count>0]

      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)
      Int <- intersect(nodes1, nodes2)
      Uni <- union(nodes1, nodes2)

      if (View) {
         par(mfcol=c(1, 2))
         F1.G <- igraph::induced_subgraph(net_Durg_MoA, F1$vertices$code[F1$vertices$count >0])
         F2.G <- igraph::induced_subgraph(net_Durg_MoA, F2$vertices$code[F2$vertices$count >0])

         igraph::V(F1.G)$size <- ceiling(F1$vertices$count[F1$vertices$count >0]/F1$length *15)
         igraph::E(F1.G)$width <- (F1$edges$count[F1$edges$count >0]/F1$length)*20
         igraph::V(F1.G)$label <- NA
         igraph::E(F1.G)$arrow.size <- .4
         plot(F1.G)

         igraph::V(F2.G)$size <- ceiling(F2$vertices$count[F2$vertices$count >0]/F2$length *15)
         igraph::E(F2.G)$width <- (F2$edges$count[F2$edges$count >0]/F2$length)*20
         igraph::V(F2.G)$label <- NA
         igraph::E(F2.G)$arrow.size <- .4
         plot(F2.G)
      }
   } 

   if (is.null(weight)) {
      return(length(Int)/length(Uni))
   } else { 
      if(!PD) {
         return(sum(weight$dist[match(Int, weight$code)])/sum(weight$dist[match(Uni, weight$code)]))
      } else {
      return(sum(2^weight$dist[match(Int, weight$code)])/sum(2^weight$dist[match(Uni, weight$code)]))
      }
   }
}
