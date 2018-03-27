context("Drug Lookup Functions")


test_that("listRelatedDrugs returns data frame with name, code, id, and kind", {
   model <- readTestRDS("alzmodel.rds")
   result <- listRelatedDrugs("C4832", model)

   expectedColumns <- c("name", "code", "id", "kind")
   
   expect_is(result, "data.frame")
   expect_true(all(expectedColumns %in% colnames(result)))
})


test_that("listRelatedDrugs returns only drugs for code", {
   model <- readTestRDS("alzmodel.rds")
   expectedCodes <- c("C19766", "C19768", "C50.68.17702", "C50.68.17701", "C50.68.17700")

   drugs <- listRelatedDrugs("C4832", model)

   expect_equal(drugs$code, expectedCodes)
   expect_true(all(drugs$kind == DRUG_KIND))
})
