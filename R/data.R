extdata.file <- function(filename) {
   system.file("extdata", filename, package=.PACKAGE_NAME)
}


#' Load the default model
#'
#' @return A list with two elements: \code{graph} and \code{DefConcept}. The \code{DefConcept} element is a data frame
#'   of concepts; the \code{graph} element is an \code{igraph} of the relationships between concepts.
#' @export
loadDefaultModel <- function() {
   filepath <- extdata.file("defaultModel.rds")
   readRDS(filepath)
}

#' Load the RxNorm CUI <-> Concept table
#'
#' @return A data frame listing the correspondence between RxNorm CUIs and NDF-RT concepts.
#' @export
loadRxNormMapping <- function() {
   filepath <- extdata.file("rxNormMapping.rds")
   readRDS(filepath)
}
