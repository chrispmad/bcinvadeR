test_that("multiplication works", {

  # Does it error if lat and long columns could not be identified?
  expect_error(
    clean_coords(
      data.frame(Latitude = c('48.19N','38.92','-112.10'),
                 Candy = c('gummy bears','jujubes','smarties'))
    )
  )

  # What if a column has some non-coordinate data accidentally in there?
  expect_equal(
    length(na.omit(clean_coords(
      data.frame(Latitude = c('48.19N','38.92','-112.10'),
                 Candy = c('-120.12W','-120.323','smarties'))
    )$lat)),
    2
  )

  # Can it accept eastings/northings? It should not be able to (for now?)
  expect_error(
    clean_coords(
      data.frame(easting = 2153953,
                 northing = 480457.1)
    )
  )

  expect_equal(
    clean_coords(
      data.frame(Latitude = c('48.19N','38.92','-112.10'),
                 Longitude = c('110.392W','-123.11','49.32'))
    )$lat,
    c(48.19,38.92,49.32))

})
