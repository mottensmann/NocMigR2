#' Visualise BirdNET results
#'
#' @param path path to BirdNET.xlsx file
#' @param taxa character
#' @import ggplot2 magrittr
#' @export
BirdNET_figure <- function(path, taxa) {

  ## binding for global variables to please checks ...
  mutate <- Taxon <- T1 <- CET <- Date <- Stunde <- sunrise <- sunset <- N <- NA
  ## retrieve lat, lon from Meta data if available, otherwise stick to defaults
  meta <- readxl::read_xlsx(path, "Meta")
  lat <- meta[["Lat"]]; lon <- meta[["Lon"]]

  ## create plot
  if (length(taxa) == 1) {
    ## read and format
    df <- readxl::read_xlsx(path, "Records") %>%
      dplyr::filter(Taxon %in% taxa) %>%
      dplyr::mutate(Date = lubridate::date(T1)) %>%
      dplyr::mutate(Stunde = lubridate::hour(T1)) %>%
      dplyr::group_by(Date, Stunde) %>%
      dplyr::summarise(N = length(Stunde))

    ## add sunrise and sunset
    df2 <- suncalc::getSunlightTimes(date = df$Date, lat = lat, lon = lon) %>%
      unique.data.frame() %>%
      dplyr::mutate(sunrise = lubridate::hour(sunrise) + lubridate::minute(sunrise)/60) %>%
      dplyr::mutate(sunset = lubridate::hour(sunset) + lubridate::minute(sunset)/60)
    df <- dplyr::left_join(df, df2, by = c("Date" = "date"))

    ggplot(df, aes(x = Stunde , y = Date, fill = N)) +
      geom_rect(aes(xmin = 0, xmax = sunrise, ymin = min(Date) - .5, ymax = max(Date) + .5),
                fill = "grey15", alpha = .01) +
      geom_rect(aes(xmin = sunset, xmax = 24, ymin = min(Date) - .5, ymax = max(Date) + .5),
                fill = "grey15", alpha = .01) +
      geom_tile(colour = "White") +
      scale_fill_binned(type = 'gradient') +
      geom_text(aes(label = N), col = "White") +
      scale_x_continuous(expand = c(0,0), limits = c(0,23), breaks = seq(0,23,3)) +
      scale_y_date(name = "", date_breaks = 'day', expand = c(0,0)) +
      egg::theme_article() +
      theme(panel.grid.minor.y = element_line(colour = "grey")) +
      labs(title = paste0(taxa, ': Nachweise pro Stunde'),x = "")
  } else {
    ## read and format
    df <- readxl::read_xlsx(path, "Records") %>%
      dplyr::filter(Taxon %in% taxa) %>%
      dplyr::mutate(Date = lubridate::date(T1)) %>%
      dplyr::mutate(Stunde = lubridate::hour(T1)) %>%
      dplyr::group_by(Taxon, Stunde) %>%
      dplyr::summarise(N = length(Stunde)) %>%
      cbind(Date = lubridate::date(meta[["From"]]))

    ## add sunrise and sunset
    df2 <- suncalc::getSunlightTimes(date = df$Date, lat = lat, lon = lon) %>%
      unique.data.frame() %>%
      dplyr::mutate(sunrise = lubridate::hour(sunrise) + lubridate::minute(sunrise)/60) %>%
      dplyr::mutate(sunset = lubridate::hour(sunset) + lubridate::minute(sunset)/60)
    df <- dplyr::left_join(df, df2, by = c("Date" = "date"))
    ## sort taxa
    df$Taxon <- factor(df$Taxon, levels = sort(df$Taxon, decreasing = T) %>% unique())
    ggplot(df, aes(x = Stunde , y = Taxon, fill = N)) +

      geom_tile(colour = "White") +
      scale_fill_binned(type = 'gradient') +
      geom_text(aes(label = N), col = "White") +
      scale_x_continuous(expand = c(0,0), limits = c(0,23), breaks = seq(0,23,3)) +
      egg::theme_article() +
      theme(panel.grid.minor.y = element_line(colour = "grey")) +
      labs(x = "", caption = paste0(meta$From, " - ", meta$To), y = "")
  }

}



