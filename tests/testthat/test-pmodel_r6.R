bass = grab_aq_occ_data('smallmouth bass')

geogs = get_waterbodies_in_area(area_type = 'regions',
                                specific_areas = 'Thompson-Okanagan',
                                size_threshold = 100)

prox = get_bcdc_risk_layer(geogs, 'GNIS_NAME_1', 'prox_to_settlements')

lake_size = geogs |> dplyr::select(GNIS_NAME_1, area_m2 = FEATURE_AREA_SQM) |> sf::st_drop_geometry()

goldfish = grab_aq_occ_data('goldfish') |>
  dplyr::select(Species) |>
  dplyr::rename(goldfish = Species)

test_that("Functions works properly", {
    bass_model = pmodel$new()
    bass_model$add(role = 'occurrence', bass)
    bass_model$add(role = 'geog_units', geogs)
    bass_model$add(role = 'risk', prox)
    bass_model$add(role = 'risk', lake_size)
    bass_model$add(role = 'risk', goldfish)
    bass_model$add(role = 'weights', x = c(1,2,1))
    bass_model
    # bass_model$add(x = 'GNIS_NAME_1', role = 'geog_id_col')

    bass_model$run(n_bins = 5)

    expect_identical(
      nrow(bass_model$occurrence_data),
      nrow(bass)
    )
})

test_that("boop", {

  bass_model = pmodel$new()

  # Predictable errors
  expect_error(
    # Incorrectly typed roles are non-starters.
    bass_model$add(bass, role = 'occ'),
    "Please choose one of occurrence, geog_units, or risk for the 'role'"
)

})
