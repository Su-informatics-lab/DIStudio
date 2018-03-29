context("Default Data Files")


# Verify that the data can be loaded

test_that("getDefaultModel returns a list with graph and DefConcept", {
   data <- loadDefaultModel()

   expect_is(data, "list")
   expect_equal(names(data), c("graph", "DefConcept"))
})
