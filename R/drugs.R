#' @export
listRelatedDrugs <- function(model, code) {
   ## TODO: Validate args

   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   nodeCodes <- names(igraph::V(nodes))

   drugs <- LU_code.NDFRT(model, nodeCodes)
   drugs
}


#' @export
listDrugAnnotations <- function(model, code, kinds=NULL) {
   ## TODO: Validate args

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
