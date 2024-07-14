#' Convert audio to the anotated .csv file
#'
#' Adds glosses to the glosses list and adds small capitals to glosses. Escapes strings that begins and ends with curly brackets.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param name character vector with the name of the .csv file
#' @param speaker character vector with the speaker name that is used as the tier name.
#'
#' @return writes a csv file with the annotation
#'
#' @importFrom stringr str_glue
#' @importFrom tools file_ext
#' @importFrom phonfieldwork get_sound_duration
#' @importFrom dplyr pull
#' @importFrom phonfieldwork create_empty_textgrid
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom phonfieldwork df_to_tier
#'
#'
#' @export

csv2TextGrid <- function(name, speaker = "speaker"){

  stringr::str_glue("{name}.csv") |>
    readr::read_csv(show_col_types = FALSE) ->
    df

  files <- list.files(pattern = name)

  file <- files[which(tolower(tools::file_ext(files)) %in%
                        c("wav", "wave", "mp3", "mp4", "ape",
                          "m4a", "flac", "aiff", "ogg"))]
  file |>
    phonfieldwork::get_sound_duration() |>
    dplyr::pull(duration) ->
    duration

  phonfieldwork::create_empty_textgrid(duration,
                        tier_name = "Interviewer",
                        path = getwd(),
                        result_file_name = name)
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
    phonfieldwork::df_to_tier(textgrid = stringr::str_glue("{name}.TextGrid"),
                              tier_name = speaker)
}
