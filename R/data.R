extdata.file <- function(filename) {
   system.file("extdata", filename, package=.PACKAGE_NAME)
}


#' @export
loadDefaultModel <- function() {
   filepath <- extdata.file("defaultModel.rds")
   readRDS(filepath)
}
