context("NDFRT Graph Construction")


# Ontology.NDFRT Tests
#  Test data includes a subset of K20 (therapeutic category) and C18
#  (dose form) records. Data is complete and valid. All following tests
#  reference the K20 subset.

test_that("Ontology.NDFRT builds subgraph with only specified kind", {
   data <- readTestRDS('concepts.rds')
   ontology <- Ontology.NDFRT("K20", data$DC, data$DFC)

   expect_equal(unique(as.character(ontology$Nodes$kind)), c("K20"))
})

test_that("all arcs with no outbound edges are placed in $top", {
   data <- readRDS(system.file(file.path('testdata', 'concepts.rds'), package=.PACKAGE_NAME))
   ontology <- Ontology.NDFRT("K20", data$DC, data$DFC)

   expected <- c('C637766', 'C637763', 'C637747')
   actual <- ontology$top

   expect_equal(actual, expected)
})
