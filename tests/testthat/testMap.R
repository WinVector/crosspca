context("Check Map")

test_that("testMap: mapping works", {
  x <- USArrests
  perm <- c(2,3,4,1)
  x2 <- x[,perm]
  mp <- crosspca:::mapcols(x2,x)
  check <- mp[perm[seq_len(length(perm))]]
  expect_true(all(check==seq_len(length(perm))))
})