context("Frequency Calculations")

test_that("Frequencies calculates the number of relationships a node has", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635170 = MAO Type-B Inhibitor
   frequencies <- Frequencies(model, "C635170")
   verts <- frequencies$vertices

   # C546 = MAOIs; C635169 = MAO-BIs
   expect_true('C546' %in% verts$code)
   expect_true('C635169' %in% verts$code)
   expect_equal(sum(verts$count), 2)
})

test_that("Frequencies calculates the total number of relationships for all nodes", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635150 = MAOI; C635170 = MAO Type-B Inhibitor
   frequencies <- Frequencies(model, c("C635170", "C635150"))
   verts <- frequencies$vertices

   # C546 = MAOIs; C635169 = MAO-BIs
   expect_equal(verts[verts$code == 'C546', 'count'], 2)
   expect_equal(verts[verts$code == 'C635169', 'count'], 1)
   expect_equal(sum(frequencies$vertices$count), 3)
})


context("Drug Similarity")

test_that("Similarity function works with models containing extraneous node kinds", {
   model <- readTestRDS("alzmodel.rds")

   expect_equal(Similarity(model, c('C20562', 'C635150'), c('C635170', 'C21508')), 0.25)
})

## Drug Pair Similarity

test_that("Similarity between drugs with identical MoA is 1", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C20562 = TACRINE; C14954 = GALANTAMINE
   # Both are Cholinesterase Inhibitors
   score <- Similarity(model, "C20562", "C14954")

   expect_equal(score, 1)
})

test_that("Similarity between drugs with disjoint MoA is 0", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C20562 = TACRINE; C635150 = MAOI
   # Former is Cholinesterase Inhibitors; latter is MAOI
   score <- Similarity(model, "C20562", "C635150")

   expect_equal(score, 0)
})

test_that("Similarity between drugs with one common MoA and one disjoint MoA is 0.5", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635150 = MAOI; C635170 = MAO Type-B Inhibitor
   score <- Similarity(model, "C635150", "C635170")

   expect_equal(score, 0.5)
})


## Drug Sets Similarity

test_that("Similarity between sets of drugs is proportion of common MoAs", {
   model <- readTestRDS("alzMoAmodel.rds")

   expect_equal(Similarity(model, c('C20562'), c('C635170', 'C21508')), 0)
   expect_equal(Similarity(model, c('C20562', 'C635150'), c('C635170', 'C21508')), 0.25)
   expect_equal(Similarity(model, c('C20562', 'C635150'), c('C635170')), 0.3333, tolerance=1e-3)
   expect_equal(Similarity(model, c('C20562', 'C635150'), c('C14954')), 0.5)
   expect_equal(Similarity(model, c('C20562', 'C635150'), c('C14954', 'C635170')), 0.6667, tolerance=1e-3)
   expect_equal(Similarity(model, c('C20562', 'C19766'), c('C14954', 'C635170')), 1)
})


## Weight Argument

test_that("Similarity increases with higher weight on common MoAs", {
   model <- readTestRDS("alzMoAmodel.rds")

   weight <- data.frame(code=c('C546', 'C635169'), dist=c(2, 1))
   score <- Similarity(model, "C635150", "C635170", weight=weight)

   expect_equal(score, 0.6667, tolerance=1e-3)
})

test_that("Similarity decreases with higher weight on disjoint MoAs", {
   model <- readTestRDS("alzMoAmodel.rds")

   weight <- data.frame(code=c('C546', 'C635169'), dist=c(1, 2))
   score <- Similarity(model, "C635150", "C635170", weight=weight)

   expect_equal(score, 0.3333, tolerance=1e-3)
})

test_that("Similarity PD argument ", {
   model <- readTestRDS("alzMoAmodel.rds")

   weight <- data.frame(code=c('C546', 'C635169'), dist=c(2, 4))
   scoreWithPD <- Similarity(model, "C635150", "C635170", weight=weight, PD=T)

   weight <- data.frame(code=c('C546', 'C635169'), dist=c(1, 4))
   scoreWithoutPD <- Similarity(model, "C635150", "C635170", weight=weight, PD=F)

   expect_equal(scoreWithPD, scoreWithoutPD)
})


## PD Argument
