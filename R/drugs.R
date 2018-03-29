## TODO: Validate args
listRelatedDrugs <- function(model, code) {
   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   nodeCodes <- names(igraph::V(nodes))

   drugs <- LU_code.NDFRT(model, nodeCodes)
   drugs
}

## TODO: Validate args
listDrugAnnotations <- function(model, code, kinds=NULL) {
   if (is.null(kinds)) {
      # Exclude other drugs by default
      kinds = setdiff(ALL_KINDS, DRUG_KIND)
   }

   nodes <- find_nodes(code, model$graph, UD="both", SubGraph=T)
   nodeCodes <- setdiff(names(igraph::V(nodes)), code)

   annotations <- LU_code.NDFRT(model, nodeCodes)
   annotations <- annotations[annotations$kind %in% kinds, ]
   annotations
}
