#' Print method for mmatrix
#' @export
#' @author Zach Vig
print.mmatrix <- function(mat) {
  lines <- apply(format(mat, justify = "right"), 1, paste, collapse = " ")
  if (nrow(mat) == 1) {
    cat(paste0("[ ", lines[1], " ]\n"))
    return(invisible(mat))
  }

  top <- paste0("\u23a1 ", lines[1], " \u23a4")
  middle <- if (nrow(mat) > 2) paste0("\u2502 ", lines[2:(nrow(mat) - 1)], " \u2502") else character(0)
  bottom <- paste0("\u23a3 ", lines[nrow(mat)], " \u23a6")
  cat(paste(c(top, middle, bottom), collapse = "\n"), "\n", sep = "")
  invisible(mat)
}
