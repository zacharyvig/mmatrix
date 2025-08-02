#' New row operator for simple matrix definiton
#' @export
#' @author Zach Vig
`%;%` <- function(a, b) {
  return(c(a, ";", b))
}
