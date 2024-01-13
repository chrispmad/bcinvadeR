# Load in data
shus = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
  bcdata::filter(GNIS_NAME_1 == 'Shuswap Lake') |>
  bcdata::collect()

paul = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |>
  bcdata::filter(GNIS_NAME == 'Paul River') |>
  bcdata::collect()

from_shus = find_downstream_course(shus, ggplot_fig = T)

paul_res = find_downstream_course(paul, wb_is_lake = F, ggplot_fig = T)

paul_res_merged = find_downstream_course(paul, wb_is_lake = F,
                                         ggplot_fig = T, wb_separate = F)

test_that("Merging option works", {
  testthat::expect_equal(
    nrow(paul_res_merged$downstream_course),
    1
  )
})

test_that("We get the right number of rows for Shuswap Lake", {
  testthat::expect_equal(
    nrow(from_shus$downstream_course),
    7
  )
})

test_that("We get the right number of rows for Paul River", {
  testthat::expect_equal(
    nrow(paul_res$downstream_course),
    287
  )
})
