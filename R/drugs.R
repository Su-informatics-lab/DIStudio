## TODO: Validate args
listRelatedDrugs <- function(code, model) {
   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   nodeCodes <- names(igraph::V(nodes))

   drugs <- LU_code.NDFRT(nodeCodes, model)
   drugs
}

## TODO: Validate args
listDrugAnnotations <- function(code, model, kinds=NULL) {
   if (is.null(kinds)) {
      # Exclude other drugs by default
      kinds = setdiff(ALL_KINDS, DRUG_KIND)
   }

   nodes <- find_nodes(code, model$graph, UD="both", SubGraph=T)
   nodeCodes <- setdiff(names(igraph::V(nodes)), code)

   annotations <- LU_code.NDFRT(nodeCodes, model)
   annotations <- annotations[annotations$kind %in% kinds, ]
   annotations
}
