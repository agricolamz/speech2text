#' Convert .csv to the raw text
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file character vector with the name of the .csv file
#'
#' @return writes a csv file with the annotation
#'
#' @importFrom stringr str_glue
#' @importFrom stringr str_ends
#' @importFrom stringr str_c
#' @importFrom dplyr pull
#' @importFrom readr read_csv
#'
#' @export

csv2raw_text <- function(file){

  file <- ifelse(stringr::str_ends(file, "\\.csv"),
                 file,
                 stringr::str_glue("{file}.csv"))
  file |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::pull(text) |>
    stringr::str_c(collapse = " ")
}
