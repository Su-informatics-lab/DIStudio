extdata.file <- function(filename) {
   system.file("extdata", filename, package=.PACKAGE_NAME)
}


loadDefaultModel <- function() {
   filepath <- extdata.file("defaultModel.rds")
   readRDS(filepath)
}
