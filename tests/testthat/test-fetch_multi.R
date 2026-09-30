test_that("data frame results are bound with plain row names", {
  f <- function(id, good_manners = 0) data.frame(id = id, x = 1:2)
  r <- fetch_multi(c("a", "b"), f)
  expect_equal(nrow(r), 4)
  expect_equal(rownames(r), as.character(1:4))
  expect_equal(r$id, c("a", "a", "b", "b"))
})

test_that("failed ids give a warning and do not discard the others", {
  f <- function(id, good_manners = 0) if(id == "bad") stop("404") else data.frame(id = id)
  expect_warning(r <- fetch_multi(c("a", "bad", "b"), f), "id 'bad' failed")
  expect_equal(r$id, c("a", "b"))
})

test_that("list results are returned as a named list", {
  f <- function(id, good_manners = 0) list(root = id)
  r <- fetch_multi(c("a", "b"), f, .combine = NULL)
  expect_equal(names(r), c("a", "b"))
  expect_equal(r$b$root, "b")
})

test_that("further arguments are passed on to every call", {
  f <- function(id, extra, good_manners = 0) data.frame(id = id, extra = extra)
  expect_equal(fetch_multi(c("a", "b"), f, extra = "x")$extra, c("x", "x"))
})
