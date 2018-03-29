context("Utility Functions")

test_that("getKindsSubmodel returns model with only vertices matching argument", {
   model <- readTestRDS("alzmodel.rds")
   kinds <- c("C8", "C12")

   submodel <- getKindsSubmodel(model, kinds)

   expect_equal(unique(igraph::V(submodel$graph)$kind), kinds)
   expect_true(all(names(igraph::V(submodel$graph)) %in% submodel$DefConcept$code))
})
