# Lookup Function Tests

context("Internal Lookup Functions")

test_that("code lookup uses supplied model", {
   model <- readTestRDS("alzmodel.rds")
   
   expected <- c("C5904", "C7928")
   actual <- LU_name.NDFRT("Progressive", model=model)

   expect_equal(actual$code, expected)
})

test_that("code lookup ignores case by default", {
   model <- readTestRDS("alzmodel.rds")
   
   actual <- LU_name.NDFRT("Vitamin", model=model)
   expect_equal(nrow(actual), 78)
})

test_that("code lookup respects ignore.case argument", {
   model <- readTestRDS("alzmodel.rds")
   
   actual <- LU_name.NDFRT("Vitamin", model=model, ignore.case=F)
   expect_equal(nrow(actual), 3)
})


test_that("name lookup uses supplied model", {
   model <- readTestRDS("alzmodel.rds")
   
   # C254 isn't in the Alzheimer's test model
   codes <- c("C4832", "C904", "C254") 
   expected <- c("C4832", "C904") 
   actual <- LU_code.NDFRT(codes, model=model)

   expect_equal(actual$code, expected)
})
