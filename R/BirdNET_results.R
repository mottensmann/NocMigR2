#' Modify 'BirdNET.results.txt' created by BirdNET-Analyzer
#'
#' @param path path to text files
#' @param recursive logical. Should the listing recurse into directories?
#' @description
#' Tweaks audacity labels by composing a string consisting of species name, recording date and time and detection score (e.g. "Blackbird 2023-08-15 [0.89]").
#'
#' @details
#' #' Reads text file containing audacity labels that were created by running the BirdNET analyzer script. Note, specifying rtype 'audacity' is required. Output is reformatted by adding the correct time stamp (estimated from file names) to the detected events. Empty files are ignored
#'
#' @return data frame
#'
#' @keywords internal
#'
BirdNET_results2txt <- function(path = NULL, recursive = FALSE) {

  if (!dir.exists(path)) stop("provide valid path")

  ## Check 'BirdNET.results.txt'
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "BirdNET.results.txt",
                                      full.names = T,
                                      recursive = recursive)

  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))

  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }


  if (length(BirdNET.results.files) >= 1) {
    BirdNET.results.list <- lapply(BirdNET.results.files, seewave::read.audacity)
    results <- data.frame()

    for (i in 1:length(BirdNET.results.files)) {
      BirdNET.results.df <- BirdNET.results.list[[i]]
      names(BirdNET.results.df)[5] <- "Score"
      BirdNET.results.df$Score <- round(BirdNET.results.df$Score, 3)
      ## prepare head
      BirdNET.results.df$x <- sapply(BirdNET.results.df$file, function(x) {
        out <- stringr::str_split(x, "/")[[1]]
        as.character(out[length(out)])
      })

      ## Get recording time from head
      BirdNET.results.df$Start <- RecreateDateTime(BirdNET.results.df$x)

      ## extract common name form label
      BirdNET.results.df$label2 <- sapply(BirdNET.results.df$label, function(x) {
        t <- stringr::str_split(x, ", ")[[1]]
        return(t[length(t)])
      })

      ## check for special case: 00:00:00 & t1 = 0
      if (any(BirdNET.results.df$t1 == 0)) {
        indices <- which(BirdNET.results.df$t1 == 0)
        for (i in indices) {
          time.sum <-
            lubridate::hour(BirdNET.results.df$Start[i]) +
            lubridate::minute(BirdNET.results.df$Start[i]) +
            lubridate::second(BirdNET.results.df$Start[i])
          if (time.sum == 0) BirdNET.results.df$t1[i] <- 1
        }
      }

      BirdNET.results.df$labelNEW <- paste(
        BirdNET.results.df$label2,
        BirdNET.results.df[["Start"]] + BirdNET.results.df[["t1"]],
        "[", BirdNET.results.df[["Score"]],"]")

      ## write to file
      seewave::write.audacity(
        x = data.frame(
          label = BirdNET.results.df[["labelNEW"]],
          t1 = BirdNET.results.df[["t1"]],
          t2 = BirdNET.results.df[["t2"]]),
        filename = stringr::str_replace(string = BirdNET.results.df$file[1],
                                        pattern = "BirdNET.results.txt" ,
                                        replacement = "BirdNET.labels.txt"))
      results <- rbind(results, BirdNET.results.df)
    }


  } else {
    warning("Did not find any BirdNET.results.txt files!")
  }
  return(results)
}
#' Modify 'BirdNET.results.txt' created by BirdNET-Analyzer
#'
#' @param path path to text files
#' @param recursive logical. Should the listing recurse into directories?
#' @description
#' Tweaks audacity labels by composing a string consisting of species name, recording date and time and detection score (e.g. "Blackbird 2023-08-15 [0.89]").
#'
#' @details
#' #' Reads text file containing audacity labels that were created by running the BirdNET analyzer script. Note, specifying rtype 'audacity' is required. Output is reformatted by adding the correct time stamp (estimated from file names) to the detected events. Empty files are ignored
#'
#' @return data frame
#'
#' @keywords internal
#'
BattyBirdNET_results2txt <- function(path = NULL, recursive = FALSE) {

  if (!dir.exists(path)) stop("provide valid path")

  ## list results
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "bat.results.txt",
                                      full.names = T,
                                      recursive = recursive)

  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))

  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }


  if (length(BirdNET.results.files >= 1)) {
    #  df with n-rows and 5-columns
    BirdNET.results.list <- lapply(BirdNET.results.files, seewave::read.audacity)
    results <- data.frame()

    for (i in 1:length(BirdNET.results.files)) {
      BirdNET.results.df <- BirdNET.results.list[[i]]
      names(BirdNET.results.df)[5] <- "Score"
      BirdNET.results.df$Score <- round(BirdNET.results.df$Score, 3)
      ## prepare head
      BirdNET.results.df$x <- sapply(BirdNET.results.df$file, function(x) {
        out <- stringr::str_split(x, "/")[[1]]
        as.character(out[length(out)])
      })

      ## Get recording time from head
      BirdNET.results.df$Start <- RecreateDateTime(BirdNET.results.df$x)

      ## extract common name form label
      BirdNET.results.df$label2 <- sapply(BirdNET.results.df$label, function(x) {
        t <- stringr::str_split(x, ", ")[[1]]
        return(t[length(t)])
      })

      ## check for special case: 00:00:00 & t1 = 0
      if (any(BirdNET.results.df$t1 == 0)) {
        indices <- which(BirdNET.results.df$t1 == 0)
        for (i in indices) {
          time.sum <-
            lubridate::hour(BirdNET.results.df$Start[i]) +
            lubridate::minute(BirdNET.results.df$Start[i]) +
            lubridate::second(BirdNET.results.df$Start[i])
          if (time.sum == 0) BirdNET.results.df$t1[i] <- 1
        }
      }

      BirdNET.results.df$labelNEW <- paste(
        BirdNET.results.df$label2,
        BirdNET.results.df[["Start"]] + BirdNET.results.df[["t1"]],
        "[", BirdNET.results.df[["Score"]],"]")

      ## write to file
      seewave::write.audacity(
        x = data.frame(
          label = BirdNET.results.df[["labelNEW"]],
          t1 = BirdNET.results.df[["t1"]],
          t2 = BirdNET.results.df[["t2"]]),
        filename = stringr::str_replace(string = BirdNET.results.df$file[1],
                                        pattern = "bat.results.txt" ,
                                        replacement = "bat.labels.txt"))
      results <- rbind(results, BirdNET.results.df)
    }


  } else {
    warning("Did not find any bat.results.txt files!")
  }
  return(results)
}
