test_that("Tests on corr_matrix", {
  # data to tests
  df <- iris

  corr <- corrp(df, comp = "g", alternative = "t")
  m <- corr_matrix(corr, isig = F)

  # class test
  expect_s3_class(m, "cmatrix")

  si <- sample(1:NROW(corr$data), 3)

  inf <- corr$data[si, c("varx", "vary", "infer.value")]

  for (k in 1:NROW(inf)) {
    expect_equal(m[as.character(inf$vary[k]), as.character(inf$varx[k])], inf$infer.value[k])
  }
})
