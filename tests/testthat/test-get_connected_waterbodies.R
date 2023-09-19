# test_that("does the function work properly?", {
#
#   # Does the function work properly when supplied with a name and coordinates?
#   expect_equal(
#     nrow(get_connected_waterbodies('Stave Lake',
#                                    waterbody_coordinates = c(-122.2837065448555,49.37115621966073),
#                                    quiet = F)
#     ),
#     2481)
#
# })
#
# test_that("does the function not work, in predictable ways?", {
#   # Does the function error if just a waterbody name is supplied?
#   expect_error(
#     get_connected_waterbodies(waterbody_name = 'Stave Lake')
#   )
#
#   # Does the function error if no waterbody can be found with that name + coordinate combo?
#   expect_error(
#     get_connected_waterbodies(waterbody_name = 'Stave Lake',
#                               waterbody_coordinates = c(0, 0))
#   )
#
#   # Does the function work properly when supplied with just a waterbody polygon?
#   expect_equal(
#     nrow(get_connected_waterbodies(waterbody_polygon = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
#                                      bcdata::filter(GNIS_NAME_1 == 'Stave Lake') |>
#                                      bcdata::collect(),
#                                    quiet = F)
#     ),
#     2481)
#
# })
