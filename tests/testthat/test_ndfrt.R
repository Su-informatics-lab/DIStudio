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
