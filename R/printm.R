#' Print method for mmatrix
#' @export
#' @author Zach Vig
print.mmatrix <- function(mat) {
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  ms <- sapply(
    1:ncol,
    function(col) max(stringr::str_length(mat[, col]))
  )
  toprint <- c()
  for (row in 1:nrow) {
    l <- stringr::str_length(mat[row, ])
    spaces <- sapply(
      1:ncol,
      function(col) paste0(rep(" ", ms[col] - l[col]), collapse = "")
    )
    r <- paste0(spaces, mat[row, ], collapse = " ")
    if (nrow == 1) {
      toprint <- append(toprint, c("[ ", r, " ]"))
    } else if (row == 1) {
      toprint <- append(toprint, c("\u23a1 ", r, " \u23a4", "\n"))
    } else if (row == nrow) {
      toprint <- append(toprint, c("\u23a3 ", r, " \u23a6"))
    } else {
      toprint <- append(toprint, c("\u2502 ", r, " \u2502", "\n"))
    }
  }
  cat(paste(toprint, collapse = ""))
}
