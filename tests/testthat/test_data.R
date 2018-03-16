context("Default Data Files")


# Verify that the data can be loaded

test_that("getDefaultPanGraph returns an instance of igraph", {
   data <- getDefaultPanGraph()

   expect_is(data, "igraph")
})

test_that("getDefaultMoAGraph returns an instance of igraph", {
   data <- getDefaultMoAGraph()

   expect_is(data, "igraph")
})
