context("Frequency Calculations")

test_that("Frequencies calculates the number of relationships a node has", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635170 = MAO Type-B Inhibitor
   frequencies <- Frequencies("C635170", model)
   verts <- frequencies$vertices

   # C546 = MAOIs; C635169 = MAO-BIs
   expect_true('C546' %in% verts$code)
   expect_true('C635169' %in% verts$code)
   expect_equal(sum(verts$count), 2)
})

test_that("Frequencies calculates the total number of relationships for all nodes", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635150 = MAOI; C635170 = MAO Type-B Inhibitor
   frequencies <- Frequencies(c("C635170", "C635150"), model)
   verts <- frequencies$vertices

   # C546 = MAOIs; C635169 = MAO-BIs
   expect_equal(verts[verts$code == 'C546', 'count'], 2)
   expect_equal(verts[verts$code == 'C635169', 'count'], 1)
   expect_equal(sum(frequencies$vertices$count), 3)
})


context("Drug Similarity")

test_that("Similarity between drugs with identical MoA is 1", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C20562 = TACRINE; C14954 = GALANTAMINE
   # Both are Cholinesterase Inhibitors
   score <- Similarity("C20562", "C14954", model)

   expect_equal(score, 1)
})

test_that("Similarity between drugs with disjoint MoA is 0", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C20562 = TACRINE; C635150 = MAOI
   # Former is Cholinesterase Inhibitors; latter is MAOI
   score <- Similarity("C20562", "C635150", model)

   expect_equal(score, 0)
})

test_that("Similarity between drugs with one common MoA and one disjoint MoA is 0.5", {
   model <- readTestRDS("alzMoAmodel.rds")

   # C635150 = MAOI; C635170 = MAO Type-B Inhibitor
   score <- Similarity("C635150", "C635170", model)

   expect_equal(score, 0.5)
})

test_that("Similarity between sets of drugs is proportion of common MoAs", {
   model <- readTestRDS("alzMoAmodel.rds")

   expect_equal(Similarity(c('C20562'), c('C635170', 'C21508'), model), 0)
   expect_equal(Similarity(c('C20562', 'C635150'), c('C635170', 'C21508'), model), 0.25)
   expect_equal(Similarity(c('C20562', 'C635150'), c('C635170'), model), 0.3333, tolerance=1e-3)
   expect_equal(Similarity(c('C20562', 'C635150'), c('C14954'), model), 0.5)
   expect_equal(Similarity(c('C20562', 'C635150'), c('C14954', 'C635170'), model), 0.6667, tolerance=1e-3)
   expect_equal(Similarity(c('C20562', 'C19766'), c('C14954', 'C635170'), model), 1)
})
