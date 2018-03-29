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
