
context("Check Res")

test_that("testP1: Works As Expected", {
  x <- USArrests
  p1 <- prcomp(x,center=TRUE,scale.=TRUE)
  x1 <- predict(p1,x)
  p2 <- crosspca::sprcomp(x,k=2,center=TRUE,scale.=TRUE)
  x2 <- predict(p2,x)
  for(i in seq_len(2)) {
    rat <- x1[,i]/x2[,i]
    expect_true(length(unique(sign(rat)))==1)
    expect_true(all(abs(abs(rat)-1)<1.e-6))
  }
})