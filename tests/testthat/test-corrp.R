test_that("Tests on corrp and cor_fun functions", {
  # data to tests
  df <- iris
  # class test
  corr <- corrp(df, comp = "g", alternative = "t", verbose = T)
  expect_s3_class(corr, "clist")

  # parallel test
  corr2 <- corrp(df, comp = "g", alternative = "t", parallel = F, verbose = T)

  expect_s3_class(corr2, "clist")
  expect_equal(corr, corr2)

  # correlation methods alternatives
  corr3 <- corrp(df,
    comp = "l",
    alternative = "g",
    parallel = T,
    verbose = T,
    pps.args = list(ptest = TRUE, num.s = 10),
    cor.nn = "mic",
    cor.nc = "pps",
    cor.cc = "uncoef"
  )

  expect_s3_class(corr3, "clist")

  corr4 <- corrp(df,
    comp = "g",
    alternative = "l",
    parallel = T,
    verbose = T,
    pps.args = list(ptest = TRUE, num.s = 10),
    cor.nn = "dcor",
    cor.nc = "pps",
    cor.cc = "cramersV"
  )

  expect_s3_class(corr4, "clist")

  corr5 <- corrp(df,
    comp = "g",
    alternative = "l",
    parallel = F,
    verbose = T,
    cor.nn = "dcor",
    cor.nc = "lm",
    cor.cc = "pps"
  )

  expect_s3_class(corr5, "clist")


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
      verbose = T
    )

    expect_equal(i.test, i.fun$infer.value)
    expect_equal(i.pv, i.fun$stat.value)
  }

  # Dcor
  for (i in seq_len(NROW(data.num))) {
    i.test <- dcor_t_test(df[[as.character(data.num$varx[i])]], df[[as.character(data.num$vary[i])]])

    i.fun <- corr_fun(df,
      nx = as.character(data.num$varx[i]), ny = as.character(data.num$vary[i]), alternative = "t", cor.nn = "dcor",
      verbose = T
    )

    expect_equal(as.numeric(i.test$estimate), i.fun$infer.value)
    expect_equal(i.test$p.value, i.fun$stat.value)
  }

  # PPS
  for (i in seq_len(NROW(data.num))) {
    i.test <- ppsr::score(df, x = as.character(data.num$varx[i]), y = as.character(data.num$vary[i]))

    i.fun <- corr_fun(df,
      nx = as.character(data.num$varx[i]), ny = as.character(data.num$vary[i]), alternative = "t", cor.nn = "pps",
      verbose = T
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
      verbose = T
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
      verbose = T
    )

    expect_equal(i.test, i.fun$infer.value)
    expect_equal(i.pv, i.fun$stat.value)
  }
})

test_that("corrp and corr_fun handle messy data", {
  df.bad <- data.frame(
    A = c(1:2, rep(NA, 13)),
    B = rep("a", 15),
    C = c(1:5, rep(NA, 10)),
    D = rep(c("a", "b", "c"), 5),
    A_dup = c(1:10, rep(NA, 5)),
    E = 1:15
  )

  df.bad2 <- data.frame(
    x = 1:5,
    y = as.Date("2023-01-01") + 0:4,
    z = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  set.seed(1)
  df.bad3 <- data.frame(
    num1 = runif(50),
    num2 = rnorm(50),
    cat1 = sample(c("A", "B", "C"), 50, replace = TRUE),
    cat2 = sample(c("X", "Y"), 50, replace = TRUE)
  )

  expect_equal(corr_fun(df.bad, "A", "D", verbose = TRUE)$stat.value, NaN)

  expect_warning(corrp(
    df.bad,
    verbose = FALSE,
    cor.nn = "mic",
    cor.nc = "pps",
    cor.cc = "uncoef",
    parallel = F
  ))

  expect_error(corrp(df.bad2, verbose = FALSE))


  corr <- corrp(df.bad3,
    comp = "l",
    alternative = "g",
    parallel = T,
    verbose = T,
    pps.args = list(ptest = TRUE, num.s = 10),
    cor.nn = "dcor",
    cor.nc = "pps",
    cor.cc = "cramersV"
  )

  corr2 <- corrp(df.bad3, comp = "g", alternative = "t", verbose = T)


  expect_s3_class(corr, "clist")
  expect_s3_class(corr2, "clist")
})
