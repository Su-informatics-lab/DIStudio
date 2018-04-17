#' @export
listRelatedDrugs <- function(model, code) {
   validateModel(model)
   if (length(code) == 0) {
      stop("non-empty code must be supplied")
   }

   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   if (is.null(nodes)) {
      nodeCodes <- NULL
   } else {
      nodeCodes <- names(igraph::V(nodes))
   }

   drugs <- LU_code.NDFRT(model, nodeCodes)
   drugs
}


#' @export
listDrugAnnotations <- function(model, code, kinds=NULL) {
   validateModel(model)
   if (length(code) == 0) {
      stop("non-empty code must be supplied")
   }

   if (is.null(kinds)) {
      # Exclude other drugs by default
      kinds <- setdiff(ALL_KINDS, DRUG_KIND)
   } else {
      invalidKinds <- setdiff(kinds, ALL_KINDS)
      if (length(invalidKinds) > 0) {
         stop("unrecognized kind: ", invalidKinds)
      }
   }

   nodes <- find_nodes(code, model$graph, UD="both", SubGraph=T)
   nodeCodes <- setdiff(names(igraph::V(nodes)), code)

   annotations <- LU_code.NDFRT(model, nodeCodes)
   annotations <- annotations[annotations$kind %in% kinds, ]
   annotations
}
