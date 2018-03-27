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



context("Annotation Lookup Functions")

test_that("listDrugAnnotations returns all related entities", {
   model <- readTestRDS("alzmodel.rds")
   expectedCodes <- c("C1388", "C2054", "C2006", "C8130", "C29536")

   annotations <- listDrugAnnotations("C12894", model)

   expect_true(all(expectedCodes %in% annotations$code))
})

test_that("listDrugAnnotations excludes queried drug", {
   model <- readTestRDS("alzmodel.rds")

   annotations <- listDrugAnnotations("C12894", model)

   expect_false("C12894" %in% annotations$code)
})
