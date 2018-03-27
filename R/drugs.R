listRelatedDrugs <- function(code, model) {
   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   nodeCodes <- names(igraph::V(nodes))

   drugs <- LU_code.NDFRT(nodeCodes, model)
   drugs
}

listDrugAnnotations <- function(code, model) {
   nodes <- find_nodes(code, model$graph, UD="both", SubGraph=T)
   nodeCodes <- setdiff(names(igraph::V(nodes)), code)

   annotations <- LU_code.NDFRT(nodeCodes, model)
   annotations
}
