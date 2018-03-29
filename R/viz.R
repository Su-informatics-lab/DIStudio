view.network <- function(Graph, showdata=T) {
   nV <- length(igraph::V(Graph))
   igraph::V(Graph)$PID <- 1:nV

   vs  <-  15;
   vlc <- .8;
   aw <- 1;
   ew <- 1;
   eas <- 0.8

   if (nV > 30) {
      vs  <-  vs *2/3
      vlc <- vlc*2/3
      aw <- aw *2/3
      ew <- ew*2/3
      eas <- eas*2/3
   }

   if (nV > 60) {
      vs  <-  vs/2
      vlc <- vlc/2
      aw <- aw/2
      ew <- ew/2
      eas <- eas/2
   } 

   if (showdata) {
      plot(Graph,
         vertex.size=vs, vertex.color=factor(igraph::V(Graph)$kind),
         vertex.label.cex=vlc, vertex.label.color= "black", vertex.label=igraph::V(Graph)$PID,
         edge.width=ew, edge.arrow.size=eas, edge.arrow.width= aw, edge.color=factor(igraph::E(Graph)$rel.code),
         edge.label=ifelse(
            igraph::E(Graph)$rel.code %in% c("R11", "R12"),
            NA, igraph::E(Graph)$rel.code
         )
      )

      Graph.data <- igraph::as_data_frame(Graph, "both")

      edges.label <- unique(Graph.data$edges[, c("rel", "rel.code")])
      rownames(edges.label) <- NULL
      names(edges.label) <- c("Relation", "Code")

      nodes.label <- Graph.data$vertices[, c("PID", "code", "c_name")]
      rownames(nodes.label) <- NULL

      View(nodes.label)
      View(edges.label)
   } else {
      plot(Graph,
         vertex.size=vs, vertex.color=factor(igraph::V(Graph)$kind),
         vertex.label.cex=vlc, vertex.label.color= "black",
         edge.width=ew, edge.arrow.size=eas, edge.arrow.width= aw, edge.color=factor(igraph::E(Graph)$rel.code),
         edge.label=ifelse(
            igraph::E(Graph)$rel.code %in% c("R11", "R12"),
            NA, igraph::E(Graph)$rel.code
         )
      )
   }
}

## Simple wrapper around the view.network function that accepts a model instead of a graph
viewModel <- function(model) {
   view.network(model$graph, showdata=F)
}
