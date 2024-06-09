#' Convert audio to the anotated .csv file
#'
#' Adds glosses to the glosses list and adds small capitals to glosses. Escapes strings that begins and ends with curly brackets.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param audio character vector with the path to the file
#' @param output_name character vector with the name of the result csv file.
#' @param model_path character vector with the path to the downloded model.
#' @param language character vector with the code for the language. See \code{audio.whisper::whisper_languages()} for the list of languages.
#' @param speaker character vector with the speaker name that is used as the tier name. \code{NULL} by default. If this argument is filled then TextGrid will be created.
#' @param restart_r_session logical. Restarts R session in order to fully clean the memory/RAM.
#'
#' @return writes a csv file with the annotation
#'
#' @importFrom stringr str_glue
#' @importFrom audio.whisper whisper
#' @importFrom readr write_csv
#'
#' @export

speech2text <- function(audio,
                        output_name = "output",
                        model_path,
                        language = "ru",
                        speaker = "",
                        restart_r_session = FALSE){

  # convert to the format specs ---------------------------------------------
  tmp <- tempdir()
  stringr::str_glue("ffmpeg -i '{audio}' -ar 16000 -ac 1 -c:a pcm_s16le '{tmp}/{output_name}.wav'") |>
    system()

  on.exit(file.remove(stringr::str_glue("{tmp}/{output_name}.wav")))

  # load and run model ------------------------------------------------------

  model <- audio.whisper::whisper(model_path)
  library(audio.whisper)
  result <- predict(model,
                    stringr::str_glue("{tmp}/{output_name}.wav"),
                    language = language)

  # write down results ------------------------------------------------------
  result$data |>
    as.data.frame() |>
    readr::write_csv(stringr::str_glue("{output_name}.csv"))

  result$params
  print(result$timing)
  if(speaker != ""){
    csv2TextGrid(name = output_name, speaker = speaker)
  }

  if(restart_r_session){
    .rs.restartR()
  } else {
    gc(verbose = FALSE)
  }
}
