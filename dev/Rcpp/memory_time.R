future::plan(strategy = future::multicore)
options(future.globals.maxSize = Inf)

#' @title Calculate Memory Peak
#' @param expr expression to evaluate
calculate_memory <- function(expr) {
  # https://stackoverflow.com/questions/58250531/memory-profiling-in-r-how-to-find-the-place-of-maximum-memory-usage
  node_size <- function() {
    bit <- 8L * .Machine$sizeof.pointer
    if (!(bit == 32L || bit == 64L)) {
      stop("Unknown architecture", call. = FALSE)
    }
    if (bit == 32L)
      28L
    else 56L
  }
  gc1 <- gc(reset = TRUE)

  promise = future::future(globals = FALSE, seed = TRUE, {
    res = expr
    gc(reset = TRUE)
    mallinfo::malloc.trim()
    res
  }) 

  print(paste("Se\u00e7\u00e3o de R de monitoramento:",  Sys.getpid()))
  print(paste("Se\u00e7\u00e3o de R de execu\u00e7\u00e3o:",  promise$job$pid))
  max_mem_used = 0
  while (TRUE) {
    Sys.sleep(0.001)
    current_mem = as.numeric((system(paste("ps -p", promise$job$pid, "-o rss="), intern = TRUE))) / 1024

    if (current_mem > max_mem_used)
      max_mem_used = current_mem

    if (future::resolved(promise))
      break
  }

  res = future::value(promise)
  rm(promise)
  gc2 <- invisible(gc())
  max_mem_used_delta <- invisible(sum(gc2[, 6] - gc1[, 2]))
  # max_mem_used_gc = invisible(sum(gc()[, 5] * c(node_size(), 8)) / 1024^2)

  gc(reset = TRUE)
  mallinfo::malloc.trim()
  # cat(sprintf("mem: %.1fMb.\n", res))
  return(
    list(
      # Máximo de memória menos memória utilizada no início da rotina
      max_mem_used_delta = max_mem_used_delta,
      # Máximo de memória
      max_mem_used = max_mem_used,
      # Resultado da expressão
      res = res
    )
  )
}

#' @title  Calculate runtime
#' @param expr expression to evaluate
#' @param msg \[\code{character(1)}\]\cr msg
#' @param quiet \[\code{logical(1)}\]\cr logical
calculate_runtime <- function(expr, msg = "Time", quiet = TRUE) {
  tictoc::tic(msg, quiet = quiet)
  res = expr
  t <- tictoc::toc()
  time <- round(t$toc - t$tic, 3)
  list(res = res, runtime = time)
}

#' @title  Calculate runtime and memory peak
#' @param expr expression to evaluate
#' @param msg \[\code{character(1)}\]\cr msg
#' @export
calculate_memory_runtime <- function(expr, msg = deparse(substitute(expr))) {
  m <- calculate_memory(
    calculate_runtime(expr, msg = msg)
  )
  m$runtime = m[[3]]$runtime
  m$res = m[[3]]$res
  message("MEMORY DELTA(mb): ", m[[1]])
  message("MEMORY PEAK(mb): ", m[[2]])
  message("TIME (S): ", m$runtime)
  
  return(m)
}
