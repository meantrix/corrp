test_that("Tests on corr_rm", {

  #data to tests
  df = iris

  corr = corrp(df,comp = 'g', alternative = 't')
  m = corr_matrix(corr)
  df2 = corr_rm(df =df,c = corr)
  df3 = corr_rm(df =df,c = m)

  expect_equal(df2,df3)

})
