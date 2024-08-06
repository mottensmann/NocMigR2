#' Create custom species list by picking species from `BirdNET_GLOBAL_6K_V2.4_Labels`
#'
#' @description
#' Loads \strong{BirdNET_GLOBAL_6K_V2.4_Labels} from [BirdNET-Analyzer](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4) and filters for target species.
#'
#' @details
#' Requires that BirdNET_GLOBAL_6K_V2.4_Labels are locally available. The easiest way is to simply clone the entire [BirdNET-Analyzer] (https://github.com/kahst/BirdNET-Analyzer) repository. Alternatively manually download the needed folders [checkpoints/V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/checkpoints/V2.4) and [labels/V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4) and save them within one directory, maintaining the original subfolders.
#'
#' @param names species names as character vector. Either scientific name or common names according to specific \code{lang}.
#' @param sciNames logical. default \code{FALSE}
#' @param lang Two-letter language code denoting a label file available for [BirdNET_Global_6K_V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4)
#' @param BirdNET_path path to BirdNET-Analyzer.
#' @param species_list path for exporting the species list
#' @param .write_text logical. defaults to \code{TRUE}
#' @return data frame
#' @examples
#' \dontrun{BirdNET_species.list(names = c("Accipiter nisus"), sciNames = T, .write_text = F)}
#'
#' @export
#'
BirdNET_species.list <- function(
    names = NULL,
    sciNames = FALSE,
    lang = 'de',
    BirdNET_path = "../BirdNET-Analyzer/",
    species_list = "my_custom_species_list.txt",
    .write_text = TRUE) {

  ## check function call
  ## ---------------------------------------------------------------------------
  if (is.null(names)) stop("Specfiy a character vector of target species")
  if (nchar(lang) > 2) {
    stop("Specify a two-letter langugage code: ", lang, " does not work")
  } else {
    ## force to lower-case
    lang <- tolower(lang)

    ## try to find corresponding labels
    path2labels_en <- file.path(
      BirdNET_path, 'checkpoints/V2.4',
      paste0('BirdNET_GLOBAL_6K_V2.4_Labels', ".txt"))

    if (!file.exists(path2labels_en)) {
      stop("Trying to access ", path2labels_en, " failed!")
    } else {
      labels_en <- utils::read.delim(path2labels_en, sep = "_", col.names = c("sciName", "comName"))
    }

    if (lang != "en") {
      path2labels_xy <- file.path(
        BirdNET_path, 'labels/V2.4',
        paste0('BirdNET_GLOBAL_6K_V2.4_Labels_', lang, ".txt"))

      if (!file.exists(path2labels_xy)) {
        stop("Trying to access ", path2labels_xy, " failed!")
      }  else {
        labels_xy <- utils::read.delim(path2labels_xy, sep = "_", col.names = c("sciName", "comNameXY"))
      }

      labels <- dplyr::left_join(labels_en, labels_xy, by = "sciName")

    } else {
      labels <- labels_en
      names(labels)[2] <- "comNameXY"
    }

    ## subset according to name vector

  if (isFALSE(sciNames)) {
    ## subset species list
    ## ---------------------------------------------------------------------------
    comNameXY <- NA
    df <- dplyr::filter(labels, comNameXY %in% names)
  } else {
    sciName <- NA
    df <- dplyr::filter(labels, sciName %in% names)
  }
    df <- df[,c("sciName", "comName")]

  ## write to file
  ## ---------------------------------------------------------------------------
  if (isTRUE(.write_text)) utils::write.table(x = df, sep = "_", col.names = FALSE, row.names = FALSE, file = species_list, quote = FALSE)
  return(df)
}
}
