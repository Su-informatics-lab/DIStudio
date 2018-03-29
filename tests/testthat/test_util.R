context("Utility Functions")

test_that("getKindsSubmodel returns model with only vertices matching argument", {
   model <- readTestRDS("alzmodel.rds")
   kinds <- c("C8", "C12")

   submodel <- getKindsSubmodel(model, kinds)

   expect_equal(unique(igraph::V(submodel$graph)$kind), kinds)
   expect_true(all(names(igraph::V(submodel$graph)) %in% submodel$DefConcept$code))
})

test_that("getDataSubmodel works with igraph.vs vertices", {
   model <- readTestRDS("alzmodel.rds")
   verts <- head(igraph::V(model$graph))

   submodel <- getDataSubmodel(model, verts)

   expect_equal(names(igraph::V(submodel$graph)), names(verts))
})

test_that("getDataSubmodel works with vertex names", {
   model <- readTestRDS("alzmodel.rds")
   verts <- head(model$DefConcept$code)

   submodel <- getDataSubmodel(model, verts)

   expect_equal(submodel$DefConcept$code, verts)
})
