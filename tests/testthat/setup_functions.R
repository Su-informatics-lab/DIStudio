# Helper functions for tests

readTestRDS <- function(filename) {
   filename <- file.path('testdata', filename)
   filename <- system.file(filename, package=.PACKAGE_NAME)

   readRDS(filename)
}
