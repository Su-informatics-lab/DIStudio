#' List related drugs
#'
#' TODO: Function description
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param code A character vector. TODO
#' @return A data frame of the matching drugs.
#' @export
listRelatedDrugs <- function(model, code) {
   validateModel(model)
   if (length(code) == 0 || nchar(code) == 0) {
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


#' List drug annotations
#'
#' TODO: Function description
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param code A character vector. TODO
#' @param kinds A vector of concept kinds to include in the returned list.
#' @return A data frame of the matching annotation concepts.
#' @export
listDrugAnnotations <- function(model, code, kinds=NULL) {
   validateModel(model)
   if (length(code) == 0 || nchar(code) == 0) {
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
