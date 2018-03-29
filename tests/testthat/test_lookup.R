# Lookup Function Tests

context("Internal Lookup Functions")

test_that("code lookup uses supplied model", {
   model <- readTestRDS("alzmodel.rds")
   
   expected <- c("C5904", "C7928")
   actual <- LU_name.NDFRT(model, "Progressive")

   expect_equal(actual$code, expected)
})

test_that("code lookup ignores case by default", {
   model <- readTestRDS("alzmodel.rds")
   
   actual <- LU_name.NDFRT(model, "Vitamin")
   expect_equal(nrow(actual), 78)
})

test_that("code lookup respects ignore.case argument", {
   model <- readTestRDS("alzmodel.rds")
   
   actual <- LU_name.NDFRT(model, "Vitamin", ignore.case=F)
   expect_equal(nrow(actual), 3)
})


test_that("name lookup uses supplied model", {
   model <- readTestRDS("alzmodel.rds")
   
   # C254 isn't in the Alzheimer's test model
   codes <- c("C4832", "C904", "C254") 
   expected <- c("C4832", "C904") 
   actual <- LU_code.NDFRT(model, codes)

   expect_equal(actual$code, expected)
})


context("Public Lookup Function")

test_that("listNodes returns all nodes with no additional arguments", {
   model <- readTestRDS("alzmodel.rds")

   nodes <- listNodes(model)

   expect_equal(nrow(nodes), 345)
})

test_that("listNodes filters requested kinds", {
   model <- readTestRDS("alzmodel.rds")

   nodes <- listNodes(model, kinds=c(INGREDIENT_KIND, DISEASE_KIND))

   expect_equal(nrow(nodes), 35)
})

test_that("listNodes filters requested names without case sensitivity", {
   model <- readTestRDS("alzmodel.rds")

   nodes <- listNodes(model, name='Vitamin')

   expect_equal(nrow(nodes), 78)
})

test_that("listNodes respects case sensitivity argument", {
   model <- readTestRDS("alzmodel.rds")

   nodes <- listNodes(model, name='Vitamin', ignore.case=F)

   expect_equal(nrow(nodes), 3)
})

test_that("listNodes with both kinds and names treats arguments as AND", {
   model <- readTestRDS("alzmodel.rds")

   nodes <- listNodes(model, kinds=c(INGREDIENT_KIND, DISEASE_KIND), name='Syndrome')

   expect_equal(nrow(nodes), 2)
})
