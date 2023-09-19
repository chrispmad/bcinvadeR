test_that("The function works", {
  expect_equal(
    intuit_lng_col(
      data.frame(boop = c('This','is','text'),
                 Latitude = c(49.213,30.112,-120.423),
                 longiTUDE = c(-139.21,-32.19,-88.90))
    ),
    'longiTUDE'
    )
})
