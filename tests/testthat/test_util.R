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


context("Model Validation")

test_that("model without graph element is invalid", {
   validModel <- readTestRDS("alzmodel.rds")
   model <- list(DefConcept=validModel$DefConcept)

   expect_error(
      validateModel(model),
      "invalid model: missing required fields: graph"
   )
})

test_that("model without DefConcept element is invalid", {
   validModel <- readTestRDS("alzmodel.rds")
   model <- list(graph=validModel$graph)

   expect_error(
      validateModel(model),
      "invalid model: missing required fields: DefConcept"
   )
})

test_that("model with non-igraph graph field is invalid", {
   validModel <- readTestRDS("alzmodel.rds")
   model <- list(graph=c(1, 2, 3), DefConcept=validModel$DefConcept)
   
   expect_error(
      validateModel(model),
      "invalid model: graph field must be an igraph"
   )
})

test_that("model with empty graph is invalid", {
   validModel <- readTestRDS("alzmodel.rds")
   model <- list(
      graph=igraph::induced.subgraph(validModel$graph, c()),
      DefConcept=validModel$DefConcept
   )
   
   expect_error(
      validateModel(model),
      "invalid model: graph must not be empty"
   )
})

test_that("model with empty concept table is invalid", {
   validModel <- readTestRDS("alzmodel.rds")
   model <- list(
      graph=validModel$graph,
      DefConcept=validModel$DefConcept[c(), ]
   )
   
   expect_error(
      validateModel(model),
      "invalid model: concept table must not be empty"
   )
})

test_that("test model is valid", {
   validModel <- readTestRDS("alzmodel.rds")
   validateModel(validModel)

   # If we get here, the test passed
   succeed()
})
