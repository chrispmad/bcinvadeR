test_that("Function works", {
  expect_equal(
    nrow(
      get_waterbody_polygon(
        'Shuswap Lake',
        focus_wb_coordinates = c(-119.1728817828854, 50.94813575724836))
      ),
    1
    )
})

test_that("Coordinates have precedence over waterbody name", {
  expect_equal(
    get_waterbody_polygon(
      'Arney Lake',
      focus_wb_coordinates = c(-119.1728817828854, 50.94813575724836)) |>
      dplyr::pull(GNIS_NAME_1),
    'Shuswap Lake'
  )
})
