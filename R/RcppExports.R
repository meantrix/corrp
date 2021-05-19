



acca <- function(m, k, maxrep = 2L, maxiter = 100L) {
    .Call(`_corrp_acca_main`, m, k, maxrep, maxiter)
}




silhouette <- function(acca, m) {
    .Call(`_corrp_silhouette_main`, acca, m)
}

