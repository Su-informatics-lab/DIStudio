listRelatedDrugs <- function(code, model) {
   nodes <- find_nodes(code, model$graph, UD="down", SubGraph=T, kinds=DRUG_KIND)
   nodeCodes <- names(igraph::V(nodes))

   drugs <- LU_code.NDFRT(nodeCodes, model)
   drugs
}
