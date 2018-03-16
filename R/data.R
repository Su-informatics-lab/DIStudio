extdata.file <- function(filename) {
   system.file("extdata", filename, package=.PACKAGE_NAME)
}


getDefaultPanGraph <- function() {
   filepath <- extdata.file("pan.rds")
   readRDS(filepath)
}

getDefaultMoAGraph <- function() {
   filepath <- extdata.file("moa.rds")
   readRDS(filepath)
}
