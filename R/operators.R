#' New row operator for simple matrix definiton
#' @export
#' @author Zach Vig
`%;%` <- function(a, b) {
  return(c(a, ";", b))
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
  return(m(result))
}
