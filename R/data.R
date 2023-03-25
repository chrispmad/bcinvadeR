#' Lizards toy dataset
#'
#' 20 rows of lizard occurrence data grabbed from the BCG Warehouse.
#'
#' @format A simple feature sensu {sf}, that is to say, a spatial tidy table.
#'  \describe
#'    \item{DataSource}{Source layer in BCG Warehouse or Excel file}
#'    \item{Date}{Occurrence date}
#'    \item{Species}{Common name}
#'    \item{Scientific}{Scientific name}
#'    \item{Location}{Text description of occurrence location}
#'    \item{geometry}{simple feature point geometry}
#'
#' @source {Created in-house using grab_terr_occ_data function}
"lizards"

#' Goldfish toy dataset
#'
#' 25 rows of goldfish occurrence data grabbed from the BCG Warehouse.
#'
#' @format A simple feature sensu {sf}, that is to say, a spatial tidy table.
#'  \describe
#'    \item{DataSource}{Source layer in BCG Warehouse or Excel file}
#'    \item{Date}{Occurrence date}
#'    \item{Species}{Common name}
#'    \item{Scientific}{Scientific name}
#'    \item{Location}{Text description of occurrence location}
#'    \item{geometry}{simple feature point geometry}
#' @source {Created in-house using grab_aq_occ_data function}

"goldfish"
