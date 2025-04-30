test_that("Tests on corr_rm", {
  # data to tests
  df <- iris

  corr <- corrp(df, comp = "g", alternative = "t", verbose = T, parallel = F)
  corr2 <- corr
  class(corr2) <- "list"

  m <- corr_matrix(corr)
  m2 <- m
  class(m2) <- "matrix"



  df2 <- corr_rm(df = df, c = corr)
  df3 <- corr_rm(df = df, c = m)
  df4 <- corr_rm(df = df, c = m)
  df5 <- corr_rm(df = df, c = corr2)
  expect_warning(df4 <- corr_rm(df = df, c = m2))

  expect_equal(df2, df3)
  expect_equal(df2, df4)
  expect_equal(df2, df5)
})
