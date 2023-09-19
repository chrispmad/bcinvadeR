test_that("The function works", {

  expect_equal(
    intuit_lat_col(
      data.frame(boop = c('This','is','text'),
                 Latitude = c(49.213,30.112,-120.423),
                 longiTUDE = c(-139.21,-32.19,-88.90))
    ),
    'Latitude'
  )
})

test_that("Errors as expected", {
  # Input should not be vector.
  expect_error(
    intuit_lat_col(c(49.12,90.11,43.02))
  )

  # Input table should have 2+ columns.
  expect_error(
    intuit_lat_col(data.frame(mystery = c(49.32,80.21)))
  )

  # No latitude column
  expect_error(
    intuit_lat_col(data.frame(zip = c('goop','zoop','zoom'),
                              numbies = c(2,1,5),
                              dog = c('little','big','medium')))
  )
})
