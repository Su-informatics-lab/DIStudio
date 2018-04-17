context("Drug Lookup Functions")

test_that("listRelatedDrugs returns data frame with name, code, id, and kind", {
   model <- readTestRDS("alzmodel.rds")

   # C4832 = Parkinson Disease
   result <- listRelatedDrugs(model, "C4832")

   expectedColumns <- c("name", "code", "id", "kind")
   
   expect_is(result, "data.frame")
   expect_true(all(expectedColumns %in% colnames(result)))
})


test_that("listRelatedDrugs returns only drugs for code", {
   model <- readTestRDS("alzmodel.rds")
   expectedCodes <- c("C19766", "C19768", "C50.68.17702", "C50.68.17701", "C50.68.17700")

   # C4832 = Parkinson Disease
   drugs <- listRelatedDrugs(model, "C4832")

   expect_equal(drugs$code, expectedCodes)
   expect_true(all(drugs$kind == DRUG_KIND))
})

test_that("listRelatedDrugs returns empty data frame when no matches found", {
   model <- readTestRDS("alzmodel.rds")
   
   # C5904 = Supranuclear Palsy; no matches in test dataset
   result <- listRelatedDrugs(model, "C5904")

   expect_is(result, "data.frame")
   expect_equal(nrow(result), 0)
})

test_that("listRelatedDrugs produces error when code is empty", {
   model <- readTestRDS("alzmodel.rds")
   
   expect_error(listRelatedDrugs(model, character()), "non-empty code must be supplied")
   expect_error(listRelatedDrugs(model, ""), "non-empty code must be supplied")
})



context("Annotation Lookup Functions")

test_that("listDrugAnnotations excludes related drugs by default", {
   model <- readTestRDS("alzmodel.rds")

   # C19766 = SELEGILINE
   annotations <- listDrugAnnotations(model, "C19766")

   expect_true(nrow(annotations) > 0)
   expect_false(any(annotations$kind == DRUG_KIND))
})

test_that("listDrugAnnotations restricts results to requested kind", {
   model <- readTestRDS("alzmodel.rds")

   # C19766 = SELEGILINE
   annotations <- listDrugAnnotations(model, "C19766", kinds=DRUG_KIND)

   expect_true(nrow(annotations) > 0)
   expect_true(all(annotations$kind == DRUG_KIND))
})

test_that("listDrugAnnotations supports multiple kinds", {
   model <- readTestRDS("alzmodel.rds")

   # C19766 = SELEGILINE
   annotations <- listDrugAnnotations(model, "C19766", kinds=c(DRUG_KIND, DISEASE_KIND))

   expect_true(nrow(annotations) > 0)
   expect_true(all(annotations$kind %in% c(DRUG_KIND, DISEASE_KIND)))
})

test_that("listDrugAnnotations excludes queried drug", {
   model <- readTestRDS("alzmodel.rds")

   # C12894 = CITALOPRAM
   annotations <- listDrugAnnotations(model, "C12894", kinds=ALL_KINDS)

   expect_true(nrow(annotations) > 0)
   expect_false("C12894" %in% annotations$code)
})

test_that("listDrugAnnotations produces error when code is empty", {
   model <- readTestRDS("alzmodel.rds")
   
   expect_error(listDrugAnnotations(model, character()), "non-empty code must be supplied")
   expect_error(listDrugAnnotations(model, ""), "non-empty code must be supplied")
})

test_that("listDrugAnnotations produces error when kind is invalid", {
   model <- readTestRDS("alzmodel.rds")
   
   expect_error(listDrugAnnotations(model, "C12894", kind='C5'), "unrecognized kind: C5")
})
