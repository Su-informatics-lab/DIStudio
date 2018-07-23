#' @export
loadMeshTree <- function(filename) {
   data <- read.table(
      filename, sep=';', stringsAsFactors=F,
      col.names=c('label', 'path'), quote=NULL
   )

   getMeshTreeAsGraph(data)
}

getMeshTermParentPath <- function(nodePath) {
   pathSegments <- unlist(strsplit(nodePath, '[.]'))

   if (length(pathSegments) == 1) {
      pathSegments <- NA
   } else {
      pathSegments <- paste(head(pathSegments, n=-1), collapse='.')
   }

   pathSegments
}

getMeshTreeEdges <- function(meshData) {
   meshData$parent <- sapply(meshData$path, getMeshTermParentPath)
   meshData <- na.omit(meshData[, c('path', 'parent')])

   meshData
}

getMeshTreeAsGraph <- function(meshData) {
   # Reorder the columns so that vertices are identified by their path
   vertices <- meshData[, c('path', 'label')]
   edges <- getMeshTreeEdges(meshData)

   igraph::graph_from_data_frame(edges, directed=T, vertices=vertices)
}
