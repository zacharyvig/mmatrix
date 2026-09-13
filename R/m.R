#' Create an mmatrix, a base R matrix with simplified printing
#'
#' @param ... Row inputs. Supply one row per argument (vectors, matrices, or data frames),
#'   or combine rows with \code{\%;\%} and pass the result as a single argument.
#'
#' @return A matrix with mmatrix printing properties
#'
#' @examples
#' m(c(1, 2, 3), c(4, 5, 6))
#' m(c(1, 2, 3) %;% c(4, 5, 6))
#'
#' @export
#' @author Zach Vig
m <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    stop("m() requires at least one row input.", call. = FALSE)
  }

  if (length(args) == 1 && is.matrix(args[[1]])) {
    mat <- args[[1]]
  } else {
    to_rows <- function(x) {
      if (is.list(x) && !is.data.frame(x)) {
        return(unlist(lapply(x, to_rows), recursive = FALSE))
      }
      if (is.data.frame(x) || is.matrix(x)) {
        return(lapply(seq_len(nrow(x)), function(i) x[i, , drop = TRUE]))
      }
      list(x)
    }

    rows <- unlist(lapply(args, to_rows), recursive = FALSE)
    row_lengths <- vapply(rows, length, integer(1))

    if (length(unique(row_lengths)) != 1L) {
      stop("All rows supplied to m() must have the same length.", call. = FALSE)
    }

    mat <- do.call(rbind, lapply(rows, function(row) as.vector(row)))
  }

  class(mat) <- unique(c("mmatrix", class(mat)))
  mat
}
