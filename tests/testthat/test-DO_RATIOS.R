test_that("DO_RATIOS calculates the landings multiplier", {
  expect_equal(DO_RATIOS(df=data.frame(COD=c(1,1,1,1),HAD=c(1,1,1,1),OBS=c("N","N","Y","Y"))), data.frame(MULTIPLIER=1))
})
