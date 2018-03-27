.PACKAGE_NAME <- 'DIStudio'


# Helper function for getting a subset of the full data for creating
# focused unit tests
getDataSubset <- function(dataModel, nodes, order) {
   G <- dataModel$graph
   verts <- names(igraph::ego(G, order, nodes=nodes)[[1]])
   sg <- igraph::induced_subgraph(G, verts)

   DefConcept <- dataModel$DefConcept
   DefConcept <- DefConcept[DefConcept$code %in% verts, ]

   list(
      graph=sg,
      DefConcept=DefConcept
   )
}
