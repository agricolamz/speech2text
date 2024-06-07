#' Generate code for analysis of audio from the folder
#'
#' Since speech2text procedures can take a lot of time, it make sense to create code for analyzis rather then using loops. This function automatically parses the folder for audio and creates code for speech2text functions. It is also possible to add lines for adding TextGrid files.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path character vector with the path to the file
#' @param model_path character vector with the path to the downloded model.
#' @param language character vector with the code for the language. See \code{audio.whisper::whisper_languages()} for the list of languages.
#' @param speakers character vector with the speakers for TextGrid creation function.
#'
#' @return writes a csv file with the annotation
#'
#' @importFrom stringr str_glue_data
#' @importFrom stringr str_remove
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tools file_ext
#'
#' @export

folder2code <- function(path = getwd(),
                        model_path,
                        language = "ru",
                        speakers = "") {

    data.frame(file = list.files(path)) |>
      dplyr::mutate(ext = tools::file_ext(file),
                    output_name = stringr::str_remove(file, ext),
                    output_name = stringr::str_remove(output_name, "\\.")) |>
      dplyr::filter(tolower(ext) %in% c("wav", "wave", "mp3", "mp4", "ape",
                                        "m4a", "flac", "aiff", "ogg")) |>
      stringr::str_glue_data(
        "

speech2text(path = '{file}',
            output_name = '{output_name}',
            model_path = '{model_path}',
            language = '{language}',
            speakers = '{speakers}')
"
      )
}
