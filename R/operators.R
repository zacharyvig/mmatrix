#' New row operator for composing matrix rows
#' @export
#' @author Zach Vig
`%;%` <- function(a, b) {
  to_rows <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      x
    } else {
      list(x)
    }
  }
  c(to_rows(a), to_rows(b))
}

#' Inverse matrix operator alternative
#' @export
#' @author Zach Vig
inv <- function(mat) {
  return(m(solve(mat)))
}


#' Matrix product operator for mmatrix class
#' @export
#' @author Zach Vig
`%*%.mmatrix` <- function(lhs, rhs) {
  result <- NextMethod("%*%")
  m(result)
}
