test_that("Tests on corrp and cor_fun functions", {
  # data to tests
  df <- iris


  # class test
  corr <- corrp(df, comp = "g", alternative = "t")
  expect_s3_class(corr, "clist")

  # parallel test
  corr2 <- corrp(df, comp = "g", alternative = "t", parallel = F)
  expect_s3_class(corr2, "clist")
  expect_equal(corr, corr2)

  # P-Value test
  data <- corr$data
  data.p1 <- subset(data, stat.value < 0.05)
  expect_true(all(data.p1$isig))


  # Inferences test 1: corrp
  data.pearson <- subset(data, infer == "Pearson Correlation")
  data.cramer <- subset(data, infer == "Cramer's V")
  data.linear <- subset(data, infer == "Linear Model")

  # Pearson Correlation
  for (i in seq_len(NROW(data.pearson))) {
    i.test <- stats::cor.test(df[[as.character(data.pearson$varx[i])]], df[[as.character(data.pearson$vary[i])]], alternative = "t")

    expect_equal(i.test$p.value, data.pearson$stat.value[i])
    expect_equal(as.numeric(i.test$"estimate"), data.pearson$infer.value[i])
  }

  # Cramer's V
  for (i in seq_len(NROW(data.cramer))) {
    i.pv <- stats::chisq.test(df[[as.character(data.cramer$varx[i])]], df[[as.character(data.cramer$vary[i])]],
      simulate.p.value = TRUE
    )$p.value
    i.test <- lsr::cramersV(iris[[as.character(data.cramer$varx[i])]], iris[[as.character(data.cramer$vary[i])]])

    expect_equal(i.pv, data.cramer$stat.value[i])
    expect_equal(i.test, data.pearson$infer.value[i])
  }

  # Linear Model
  for (i in seq_len(NROW(data.linear))) {
    x <- df[[as.character(data.linear$varx[i])]]
    y <- df[[as.character(data.linear$vary[i])]]
    clx <- class(x)
    cly <- class(y)

    cond.cn <- (cly %in% c("factor", "character") && clx %in% c("integer", "numeric"))

    if (cond.cn) {
      z <- x
      clz <- clx

      x <- y
      clx <- cly
      y <- z
      cly <- clz
    }



    i.test <- summary(stats::lm(y ~ as.factor(x)))

    expect_equal(as.numeric(stats::pf(i.test$fstatistic[1], i.test$fstatistic[2],
      i.test$fstatistic[3],
      lower.tail = F
    )), data.linear$stat.value[i])
    expect_equal(sqrt(i.test[["r.squared"]]), data.linear$infer.value[i])
  }

  # Inferences test 2: cor_fun

  # sample numerical correlations to test
  data.num <- data.pearson[sample(seq_len(NROW(data.pearson)), 3), ]

  # Numerical
  # MIC
  for (i in seq_len(NROW(data.num))) {
    i.test <- minerva::mine(df[[as.character(data.num$varx[i])]], df[[as.character(data.num$vary[i])]])$MIC
    i.pv <- ptest(df[[as.character(data.num$varx[i])]], df[[as.character(data.num$vary[i])]],
      FUN = function(...) {
        z <- minerva::mine(...)
        return(z$MIC)
      }, alternative = "t"
    )

    i.fun <- corr_fun(df,
      nx = as.character(data.num$varx[i]), ny = as.character(data.num$vary[i]), alternative = "t", cor.nn = "mic",
      verbose = F
    )

    expect_equal(i.test, i.fun$infer.value)
    expect_equal(i.pv, i.fun$stat.value)
  }

  # Dcor
  for (i in seq_len(NROW(data.num))) {
    i.test <- dcor_t_test(df[[as.character(data.num$varx[i])]], df[[as.character(data.num$vary[i])]])

    i.fun <- corr_fun(df,
      nx = as.character(data.num$varx[i]), ny = as.character(data.num$vary[i]), alternative = "t", cor.nn = "dcor",
      verbose = F
    )

    expect_equal(as.numeric(i.test$estimate), i.fun$infer.value)
    expect_equal(i.test$p.value, i.fun$stat.value)
  }

  # PPS
  for (i in seq_len(NROW(data.num))) {
    i.test <- ppsr::score(df, x = as.character(data.num$varx[i]), y = as.character(data.num$vary[i]))

    i.fun <- corr_fun(df,
      nx = as.character(data.num$varx[i]), ny = as.character(data.num$vary[i]), alternative = "t", cor.nn = "pps",
      verbose = F
    )



    expect_equal(i.test$pps, i.fun$infer.value)
    expect_equal(i.test$metric, i.fun$stat)
    expect_equal(i.test$model_score, i.fun$stat.value)
  }

  # Categorical

  # PPS
  for (i in seq_len(NROW(data.cramer))) {
    i.test <- ppsr::score(df, x = as.character(data.cramer$varx[i]), y = as.character(data.cramer$vary[i]))

    i.fun <- corr_fun(df,
      nx = as.character(data.cramer$varx[i]), ny = as.character(data.cramer$vary[i]), alternative = "t", cor.cc = "pps",
      verbose = F
    )



    expect_equal(i.test$pps, i.fun$infer.value)
    expect_equal(i.test$metric, i.fun$stat)
    expect_equal(i.test$model_score, i.fun$stat.value)
  }

  # Uncoef
  for (i in seq_len(NROW(data.cramer))) {
    i.test <- DescTools::UncertCoef(df[[as.character(data.cramer$varx[i])]], df[[as.character(data.cramer$vary[i])]])
    i.pv <- ptest(df[[as.character(data.cramer$varx[i])]], df[[as.character(data.cramer$vary[i])]],
      FUN = DescTools::UncertCoef, alternative = "t", rk = TRUE
    )

    i.fun <- corr_fun(df,
      nx = as.character(data.cramer$varx[i]), ny = as.character(data.cramer$vary[i]), alternative = "t", cor.cc = "uncoef",
      verbose = F
    )

    expect_equal(i.test, i.fun$infer.value)
    expect_equal(i.pv, i.fun$stat.value)
  }
})
