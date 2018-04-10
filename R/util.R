.PACKAGE_NAME <- 'DIStudio'


# Helper function for getting a submodel of the full data for creating
# focused unit tests
#' @export
getDataSubmodel <- function(model, verts) {
   if (!is.character(verts)) {
      verts <- names(verts)
   }

   sg <- igraph::induced_subgraph(model$graph, verts)

   DefConcept <- model$DefConcept
   DefConcept <- DefConcept[DefConcept$code %in% verts, ]

   list(
      graph=sg,
      DefConcept=DefConcept
   )
}


getEgoNetworkSubmodel <- function(model, nodes, order) {
   verts <- names(igraph::ego(model$graph, order, nodes=nodes)[[1]])
   getDataSubmodel(model, verts)
}


getKindsSubmodel <- function(model, kinds) {
   verts <- igraph::V(model$graph)
   verts <- subset(verts, verts$kind %in% kinds)

   getDataSubmodel(model, verts)
}


## TODO: Helper to validate supplied args:
##    •  kind
##    •  model
##    •  code
