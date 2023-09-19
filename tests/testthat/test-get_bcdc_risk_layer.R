geogs = get_waterbodies_in_area(area_type = 'regions',
                                specific_areas = 'Thompson-Okanagan',
                                size_threshold = 100)

test_that("multiplication works", {
  expect_equal(
    get_bcdc_risk_layer(geogs, "GNIS_NAME_1", layer = "prox_to_settlements")$prox_to_settlements_raw,
    c(14.197, 0.312, 0.713)
  )
})
