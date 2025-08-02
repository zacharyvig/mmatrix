#' Create an mmatrix, a base R matrix with simplied printing
#'
#' @param ... The matrix elements separated by commas
#'            Separate new rows with the "percent sign"-"semicolon"-"percent sign" operator
#'
#' @return A matrix with mmatrix printing properties
#'
#' @examples
#' \code{m(1, 2, 3 %;% 4, 5, 6)}
#'
#' @export
#' @author Zach Vig
m <- function(...) {
  if ("data.frame" %in% sapply(list(...), class)) {
    return(data.frame(...))
  } else if (isFALSE("matrix" %in% sapply(list(...), class))) {
    dat <- eval(c(...))
    nrow <- sum(grepl(";", dat)) + 1
    dat <- dat[which(dat != ";")]
    if (is.character(dat)) {
      dat <- readr::parse_guess(dat)
    }
    mat <- matrix(
      dat, nrow = nrow, byrow = TRUE
    )
  } else {
    mat <- as.matrix(...)
  }
  class(mat) <- list("matrix", "mmatrix")
  return(mat)
}
