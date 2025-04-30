test_that("Tests on corr_matrix", {
  # data to tests
  df <- iris

  corr <- corrp(df, comp = "g", alternative = "t", parallel = F, verbose = T)
  corr2 <- corr
  class(corr2) <- "list"

  expect_error(corr_matrix(isig = T))

  m1 <- corr_matrix(corr, isig = T)
  m2 <- corr_matrix(corr, isig = F)
  suppressWarnings({
    m3 <- corr_matrix(corr2, isig = T)
    m4 <- corr_matrix(corr2, isig = T, col = "isig")
  })

  # class test
  expect_s3_class(m1, "cmatrix")
  expect_s3_class(m2, "cmatrix")
  expect_s3_class(m3, "cmatrix")
  expect_s3_class(m4, "cmatrix")

  expect_true(all(m4 %in% c(T, F)))
  expect_false(all(m1 %in% m2))
  expect_true(all(m1 %in% m3))

  si <- sample(seq_len(NROW(corr$data)), 3)

  inf <- corr$data[si, c("varx", "vary", "infer.value")]

  for (k in seq_len(NROW(inf))) {
    expect_equal(m2[as.character(inf$vary[k]), as.character(inf$varx[k])], inf$infer.value[k])
  }
})
