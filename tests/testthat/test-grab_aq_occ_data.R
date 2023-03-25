test_that("Grab aquatic occurrence data input tests", {
  #Input errors
  expect_error(grab_aq_occ_data(), "Enter the species' common name")
  expect_error(grab_aq_occ_data(common_names = 10), "Species name must be a character string")
})
