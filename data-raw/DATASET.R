## code to prepare `DATASET` dataset goes here
goldfish = grab_aq_occ_data('goldfish') |>
  dplyr::sample_n(size = 25)

lizards = grab_terr_occ_data(common_names = 'common wall lizard', scientific_name = 'Podarcis muralis') |>
  dplyr::sample_n(size = 20)

usethis::use_data(goldfish, lizards, overwrite = TRUE)
