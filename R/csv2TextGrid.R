#' Convert .csv to the Praat TextGrid
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file character vector with the name of the .csv file
#' @param speaker character vector with the speaker name that is used as the tier name.
#'
#' @return writes a csv file with the annotation
#'
#' @importFrom stringr str_glue
#' @importFrom readr read_csv
#' @importFrom tools file_ext
#' @importFrom phonfieldwork get_sound_duration
#' @importFrom phonfieldwork create_empty_textgrid
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom phonfieldwork df_to_tier
#'
#' @export

csv2TextGrid <- function(file, speaker = "speaker"){

  file <- str_remove(file, "\\.csv")

  stringr::str_glue("{file}.csv") |>
    readr::read_csv(show_col_types = FALSE) ->
    df

  files <- list.files(pattern = file)

  audio <- files[which(tolower(tools::file_ext(files)) %in%
                        c("wav", "wave", "mp3", "mp4", "ape",
                          "m4a", "flac", "aiff", "ogg"))]
  audio |>
    phonfieldwork::get_sound_duration() |>
    dplyr::pull(duration) ->
    duration

  phonfieldwork::create_empty_textgrid(duration,
                        tier_name = "Interviewer",
                        path = getwd(),
                        result_file_name = file)
  df |>
    dplyr::rename(content = text,
                  time_start = from,
                  time_end = to) |>
    dplyr::select(time_start, time_end, content) |>
    dplyr::mutate(content = stringr::str_replace_all(content, '"', "'"),
                  time_start = ifelse(time_start == 0,
                                      0,
                                      as.double(time_start)),
                  time_end = as.double(time_end)) |>
    phonfieldwork::df_to_tier(textgrid = stringr::str_glue("{file}.TextGrid"),
                              tier_name = speaker)
}
