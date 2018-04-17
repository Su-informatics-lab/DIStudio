.PACKAGE_NAME <- 'DIStudio'


# Helper function for getting a submodel of the full data for creating
# focused unit tests
#' @export
getDataSubmodel <- function(model, verts) {
   validateModel(model)

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


## ----------------
## Model Validation

validateGraph <- function(graph) {
   if (!igraph::is_igraph(graph)) {
      stop("graph field must be an igraph")
   }

   if (igraph::gsize(graph) == 0) {
      stop("graph must not be empty")
   }
}

validateConceptTable <- function(DC) {
   if (nrow(DC) == 0) {
      stop("concept table must not be empty")
   }
}

validateModelFields <- function(model) {
   expectedFields <- c('graph', 'DefConcept')
   missingFields <- setdiff(expectedFields, names(model))
   if (length(missingFields) > 0) {
      stop("missing required fields: ", missingFields)
   }
}

validateModel <- function(model) {
   tryCatch({
      validateModelFields(model)
      validateGraph(model$graph)
      validateConceptTable(model$DefConcept)
   }, error=function(e) {
      stop("invalid model: ", e$message)
   })
}

## TODO: Helper to validate supplied args:
##    •  kind
##    •  model
##    •  code
