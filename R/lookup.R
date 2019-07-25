## look up for code or name 
LU_name.NDFRT <- function(model, x, ignore.case=T) {
   with(model, DefConcept[grep(x, DefConcept$name, ignore.case=ignore.case), ])
}

LU_code.NDFRT <- function(model, x) {
   with(model, subset(DefConcept, DefConcept$code %in% x))
}


#' List matching nodes
#'
#' Find nodes of a given network based on the search criterion.  
#'
#' @param model A model object like returned by \code{\link{loadDefaultModel}}.
#' @param kinds A vector of concept kinds to include in the returned list.
#' @param name A character vector containing a pattern that should be matched against concept names.
#' @param ignore.case A logical scalar. If \code{FALSE}, the pattern matching is case sensitive, and if \code{TRUE}, case is ignored during
#' matching.
#' @return A data frame of the concepts matching the supplied \code{name}.
#' @export
listNodes <- function(model, kinds=NULL, name=NULL, ignore.case=T) {
   validateModel(model)

   nodes <- model$DefConcept

   if (!is.null(kinds)) {
      nodes <- nodes[nodes$kind %in% kinds, ]
   }
   
   if (!is.null(name)) {
      nodes <- nodes[grep(name, nodes$name, ignore.case=ignore.case), ]
   }

   nodes
}
