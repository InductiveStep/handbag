e <- function(l,n) {
  if (l == 1) {
    list(n)
  } else {
    reses = list()

    for (k in 0:n) {
      res <- e(l-1, n-k)

      for (i in 1:length(res)) {
        res[[i]] <- c(k, res[[i]])
      }
      reses <- c(reses, res)
    }
    reses
  }
}

#' Enumerate two dimensional contingency tables with given
#' dimensions and fixed grand total
#'
#' This function enumerates all contingency tables of a particular
#' size all summing to a grand total.
#'
#' @param nrow Number of rows in each matrix
#' @param ncol Number of columns
#' @param total Grand total the cells sum to
#' @return A list of nrow by ncol matrices
#' #' @examples
#' enum_contingency_tables(1, 4, 1)
#' enum_contingency_tables(2, 2, 4)
#' @export
enum_contingency_tables <- function(nrow, ncol, total) {
  stopifnot(nrow >= 1)
  stopifnot(ncol >= 1)
  stopifnot(nrow %% 1 == 0)
  stopifnot(ncol %% 1 == 0)

  results <- e(nrow * ncol, total)

  lapply(results, function (x) matrix(x, nrow, ncol))
}


