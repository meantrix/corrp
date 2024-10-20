test_that("Tests on acca and sil_acca functions", {
  # data to tests
  df <- iris
  k <- 2
  corr <- corrp(df, comp = "g", alternative = "t")

  m <- corr_matrix(corr)
  m2 <- m
  class(m2) <- "matrix"
  set.seed(1)
  acca.res <- acca(m, k)
  set.seed(1)
  expect_warning(acca.res2 <-  acca(m2, k))
  expect_true(all(acca.res[[2]] %in% acca.res2[[2]]))

  # test class acca
  expect_s3_class(acca.res, "acca_list")
  # number of clusters k
  expect_true(length(acca.res) == k)
  # colnames
  expect_true(all(unlist(acca.res) %in% colnames(m)))
  s <- sil_acca(acca.res, m)
  expect_true(inherits(s, "corrpstat"))

  # check also when results is a normal list
  acca.res.list <- acca.res
  class(acca.res.list) <- "list"
  expect_warning(s.list <- sil_acca(acca.res.list, m))
  expect_equal(s,s.list)

})


test_that("Checks if best_acca works", {
  # data to tests
  df <- iris
  corr <- corrp(df, comp = "g", alternative = "t")

  m <- corr_matrix(corr)
  expect_s3_class(m, "cmatrix")

  m2 <- m
  class(m2) <- "matrix"

  set.seed(1)
  best.acca1 <- best_acca(m, mink = 2 , maxk = 10)
  set.seed(1)
  expect_warning(best.acca2 <- best_acca(m2, mink = 2 , maxk = 10))

  expect_equal(best.acca1$k,best.acca2$k)



})

