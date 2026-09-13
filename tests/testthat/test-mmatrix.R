test_that("m() builds matrices from row vectors", {
  mat <- m(c(1, 2, 3), c(4, 5, 6))

  expect_s3_class(mat, "mmatrix")
  expect_identical(unclass(mat), matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
})

test_that("m() supports row composition with %;%", {
  mat <- m(c(1, 2, 3) %;% c(4, 5, 6))

  expect_identical(unclass(mat), matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
})

test_that("m() rejects inconsistent row lengths", {
  expect_error(
    m(c(1, 2), c(3, 4, 5)),
    "same length"
  )
})

test_that("%*%.mmatrix preserves mmatrix class", {
  lhs <- m(c(1, 2), c(3, 4))
  rhs <- m(c(5, 6), c(7, 8))

  result <- lhs %*% rhs

  expect_s3_class(result, "mmatrix")
  expect_identical(unclass(result), unclass(as.matrix(lhs) %*% as.matrix(rhs)))
})

test_that("inv() returns an mmatrix inverse", {
  mat <- m(c(4, 7), c(2, 6))
  inv_mat <- inv(mat)

  expect_s3_class(inv_mat, "mmatrix")
  expect_equal(as.matrix(inv_mat), solve(as.matrix(mat)))
})

test_that("print.mmatrix uses bracket formatting", {
  mat <- m(c(1, 2), c(3, 4))

  output <- capture.output(print(mat))

  expect_equal(length(output), 2)
  expect_match(output[1], "⎡")
  expect_match(output[2], "⎣")
})
