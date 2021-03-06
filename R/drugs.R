#' List related nodes
#'
#' Identify related nodes of a given concept of an ontology. 
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param code A concept code (character vector) to look for. 
#' @param direction A character vector containing one of \code{'up'}, \code{'down'},
#'                  or \code{'both'}.
#' @param kinds A vector of concept kinds to include in the returned list.
#' @return A data frame of the matching nodes
#' @export
listRelatedNodes <- function(model, code, direction='down', kinds='All') {
   validateModel(model)
   if (length(code) == 0 || nchar(code) == 0) {
      stop("non-empty code must be supplied")
   }

   nodes <- find_nodes(code, model$graph, UD=direction, SubGraph=T, kinds=kinds)
   if (is.null(nodes)) {
      nodeCodes <- NULL
   } else {
      nodeCodes <- names(igraph::V(nodes))
   }

   drugs <- LU_code.NDFRT(model, nodeCodes)
   drugs
}


#' List related drugs
#'
#' Identify all related drugs of a given concept of an ontology. This function is
#' equivalent to invoking \code{listRelatedNodes(model, code direction='down', kinds=DRUG_KIND)}.
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param code A concept code (character vector) to look for. 
#' @return A data frame of the matching drugs.
#' @export
listRelatedDrugs <- function(model, code) {
   # TODO: Update description?
   listRelatedNodes(model, code, direction='down', kinds=DRUG_KIND)
}


#' List related annotations 
#'
#' Find all related concepts of a given concept, optionally restricted to set of concept kinds. 
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param code A concept code (character vector) to look for.
#' @param kinds A vector of concept kinds to include in the returned list.
#' @return A data frame of the matching annotation concepts.
#' @export
listDrugAnnotations <- function(model, code, kinds=NULL) {
   if (is.null(kinds)) {
      # Exclude other drugs by default
      kinds <- setdiff(ALL_KINDS, DRUG_KIND)
   } else {
      invalidKinds <- setdiff(kinds, ALL_KINDS)
      if (length(invalidKinds) > 0) {
         stop("unrecognized kind: ", invalidKinds)
      }
   }

   nodes <- listRelatedNodes(model, code, direction="both")
   nodeCodes <- setdiff(nodes$code, code)

   annotations <- LU_code.NDFRT(model, nodeCodes)
   annotations <- annotations[annotations$kind %in% kinds, ]
   annotations
}
