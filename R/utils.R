# auxiliar functions

# linear regression Calculations
.corlm <- function(x, y, nx, ny, p.value, comp, verbose, lm.args = list(), ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]

  infer <- "Linear Model"
  stat <- "P-value"

  args <- c(list(y ~ as.factor(x)), lm.args)

  sum.res <- summary(
    do.call(stats::lm, args)
  )

  pv <- stats::pf(sum.res$fstatistic[1], sum.res$fstatistic[2],
    sum.res$fstatistic[3],
    lower.tail = FALSE
  )

  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  r <- sqrt(sum.res[["r.squared"]])
  msg <- ""


  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }


  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}

# CramersV Calculations
.cramersvp <- function(x, y, nx, ny, p.value, comp, verbose, cramersV.args = list(), ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]

  infer <- "Cramer\'s V"
  stat <- "P-value"

  args <- c(list(x), list(y), cramersV.args)

  pv <- stats::chisq.test(x, y, simulate.p.value = TRUE)$p.value
  r <- do.call(lsr::cramersV, args)

  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  msg <- ""


  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }

  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}

# Distance Correlation Calculations
.dcorp <- function(x, y, nx, ny, p.value, comp, verbose, dcor.args = list(), ...) {
  infer <- "Distance Correlation"
  stat <- "P-value"

  args <- c(list(x), list(y), dcor.args)


  dc <- do.call(dcorT_test, args)

  pv <- dc$p.value
  r <- as.numeric(dc$estimate)
  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  msg <- ""

  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }

  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}
# Pearson Calculations
.corperp <- function(x, y, nx, ny, p.value, comp, verbose, alternative, pearson.args = list(), ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]

  infer <- "Pearson Correlation"
  stat <- "P-value"

  pearson.args$alternative <- alternative # from global
  pearson.args$method <- "pearson"
  args <- c(list(x), list(y), pearson.args)

  res <- do.call(stats::cor.test, args = args)
  pv <- res[["p.value"]]
  r <- as.numeric(res[["estimate"]])
  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  msg <- ""


  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }

  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}


# MIC calculations
.micorp <- function(x, y, nx, ny, p.value, comp, verbose, alternative, num.s, rk, mic.args = list(), ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]

  infer <- "Maximal Information Coefficient"
  stat <- "P-value"

  args <- c(list(x), list(y), mic.args)

  pv <- ptest(x, y, FUN = function(y, x) {
    args <- c(list(x), list(y), mic.args)
    do.call(function(...) {
      z <- minerva::mine(...)
      return(z$MIC)
    }, args)
  }, rk = rk, num.s = num.s, alternative = alternative)
  # ptest(y,x,FUN = function(y,x) {minerva::mine(y,x)$MIC} )
  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  r <- do.call(function(...) {
    z <- minerva::mine(...)
    return(z$MIC)
  }, args)
  msg <- ""

  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }

  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}


# Uncertainty coefficient Calculations
.uncorp <- function(x, y, nx, ny, p.value, comp, verbose, alternative, num.s, rk, uncoef.args = list(), ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]

  args <- c(list(x), list(y), uncoef.args)

  pv <- ptest(y, x, FUN = function(x, y) {
    args <- c(list(x), list(y), uncoef.args)
    do.call(DescTools::UncertCoef, args)
  }, rk = rk, num.s = num.s, alternative = alternative)
  # pv = ptest(y,x,FUN = function(y,x) DescTools::UncertCoef(y,x) )

  infer <- "Uncertainty coefficient"
  stat <- "P-value"
  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  r <- do.call(DescTools::UncertCoef, args)
  msg <- ""

  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )

      message(msg)
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )

      message(msg)
    }
  }

  return(list(
    infer = infer, infer.value = r, stat = stat, stat.value = pv,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}

# Predictive Power Score Calculations
.corpps <- function(x, y, nx, ny, p.value, comp, verbose, alternative, num.s, rk, pps.args = list(), ...) {
  
  args <- c(list(data.frame(x = unlist(x), y = unlist(y))), list("x", "y"), pps.args)
  
  pv <- ptest(y, x, FUN = function(x, y) {    
    args <- c(list(data.frame(x = x, y = y)), list("x", "y"), pps.args)    
    r = do.call(ppsr::score, args)
    return(r$pps)
  }, rk = rk, num.s = num.s, alternative = alternative)
  
  compare <- .comparepv(x = pv, pv = p.value, comp = comp)
  r <- do.call(ppsr::score, args)

  msg <- ""
  infer <- "Predictive Power Score"
  infer.value <- r$pps
  stat <- "P-value"
  stat.value <- pv
  isig <- TRUE

  if (compare$comp) {
    isig <- TRUE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "Alternative hypothesis: true ", infer, " is not equal to 0. ",
        "P-value: ", pv, "."
      )
    }
  } else {
    isig <- FALSE

    if (verbose) {
      msg <- paste0(
        ny, " vs. ", nx, ". ",
        "There is no correlation at the confidence level p-value. ",
        "P-value:", p.value, " ", compare$str, " estimated p-value: ", pv, "."
      )
     
    }
  }

  if (verbose) {
    msg = paste0(msg,
      "\nModel Parameters:",
      "\nbaseline_score: ", r$baseline_score, 
      "\ncv_folds: ", r$cv_folds, ";",
      "\nalgorithm: ", r$algorithm, ";",
      "\nmodel_type: ", r$model_type, ".")

    message(msg)
  }

  return(list(
    infer = infer, infer.value = infer.value, stat = stat, stat.value = stat.value,
    isig = isig, msg = msg, varx = nx, vary = ny
  ))
}


# compare p-value alternatives
.comparepv <- function(x, pv, comp = c("l", "g")) {
  comp <- match.arg(comp)

  if (comp == "g") {
    str <- "<"
    comp <- pv > x
  } else {
    str <- ">"
    comp <- pv < x
  }

  return(list("comp" = comp, "str" = str))
}

# Convert NULL list elements to NA
.null.to.na <- function(x) {
  null_indices <- vapply(x, is.null, logical(1))
  x[null_indices] <- NA
  return(x)
}
