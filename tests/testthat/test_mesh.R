context("MeSH Tree Functions")

test_that("getMeshTermParentPath returns NA for root nodes", {
   expect_true(is.na(getMeshTermParentPath('A01')))
})

test_that("getMeshTermParentPath returns parent path for leaf nodes", {
   expect_equal(getMeshTermParentPath('A01.111'), 'A01')
   expect_equal(getMeshTermParentPath('A01.236'), 'A01')
   expect_equal(getMeshTermParentPath('A01.236.249'), 'A01.236')
   expect_equal(getMeshTermParentPath('Z01.756.092.650'), 'Z01.756.092')
})

test_that("getMeshTreeEdges builds edge list", {
   meshData <- data.frame(
      label=c('BR', 'AL', 'B', 'MG, H'),
      path=c('A01', 'A01.111', 'A01.236', 'A01.236.249'),
      stringsAsFactors=F
   )

   # Root nodes don't have outgoing edges
   expected <- data.frame(
      path=c('A01.111', 'A01.236', 'A01.236.249'),
      parent=c('A01', 'A01', 'A01.236'),
      stringsAsFactors=F
   )

   edges <- getMeshTreeEdges(meshData)

   expect_true(all(edges == expected))
})

test_that("getMeshTreeAsGraph creates vertices for each row", {
   meshData <- data.frame(
      label=c('BR', 'AL', 'B', 'MG, H'),
      path=c('A01', 'A01.111', 'A01.236', 'A01.236.249'),
      stringsAsFactors=F
   )
   
   graph <- getMeshTreeAsGraph(meshData)

   expect_equal(igraph::V(graph)$label, c('BR', 'AL', 'B', 'MG, H'))
})
