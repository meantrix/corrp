test_that("Tests on acca and sil_acca functions", {
  # data to tests
  df <- iris
  k <- 2
  corr <- corrp(df, comp = "g", alternative = "t")
  m <- corr_matrix(corr)
  acca.res <- acca(m, k)
  # test class acca
  expect_s3_class(acca.res, "acca_list")
  # number of cluster k
  expect_true(length(acca.res) == k)
  # colnames
  expect_true(all(unlist(acca.res) %in% colnames(m)))
  s <- sil_acca(acca.res, m)
  expect_true(inherits(s, "corrpstat"))
})
