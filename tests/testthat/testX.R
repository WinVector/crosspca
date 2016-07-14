library('crosspca')

context("Check X")

test_that("testX: x works", {
  set.seed(23552)
  splitPlan <- vtreat::kWayCrossValidation(nrow(USArrests),3,NULL,NULL)
  dc <- xprcomp(USArrests,k=2,crossplan=splitPlan,center=TRUE,scale.=TRUE)
})