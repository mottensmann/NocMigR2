#' Format database (db.xlsx)
#'
#' @param path path
#' @import openxlsx2
#'
reformat_db <- function(path) {

  if (!file.exists(path)) stop("provide valid path")

  wb <- openxlsx2::wb_load(path)

  ## Get data for dimensions --
  data <- openxlsx2::wb_read(
    path,
    show_hyperlinks = TRUE,
    sheet = "Records")
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)

  if (all(is.na(data[["File"]]))) {
    stop('Colum "File" formated as hyperlink but no cached result.
         Open and save file in Excel or LibreOffice and try again')
  }

  ## Add filter to header row --
  wb <- openxlsx2::wb_add_filter(wb,
                                 sheet = "Records",
                                 rows  = 1,
                                 cols  = 1:n_cols)

  ## Freeze first row --
  wb <- openxlsx2::wb_freeze_pane(wb,
                                  sheet            = "Records",
                                  first_active_row = 2)
  ## Hyperlink --
  hyperlink_col <- which(names(data) == "File")
  urls_full     <- data[["File"]]
  urls_short    <- basename(urls_full)

  wb <- openxlsx2::wb_add_formula(
    wb, sheet = "Records",
    dims = openxlsx2::wb_dims(rows = 2:n_rows, cols = hyperlink_col),
    x = paste0('HYPERLINK("', urls_full, '","', urls_short, '")'),
    array = FALSE
  )

  ## Auto adjust column width --
  wb <- openxlsx2::wb_set_col_widths(wb = wb, sheet = "Records",
                                     cols = 1:n_cols, widths = "auto")

  ## Add borders to all cells --
  ## Add borders to all cells --
  black <- openxlsx2::wb_color(name = "black")

  wb <- openxlsx2::wb_add_border(
    wb, sheet = "Records",
    dims          = openxlsx2::wb_dims(rows = 1:n_rows, cols = 1:n_cols),
    bottom_border = 'thick', bottom_color = black,
    left_border   = 'thin', left_color    = black,
    top_border    = 'thick', top_color    = black,
    right_border  = 'thin', right_color   = black,
    inner_hgrid   = "thin", inner_hcolor  = black,
    inner_vgrid   = "thin", inner_vcolor  = black)

  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
}

#' Format BirdNET.xlsx in a nice style
#'
#' @param path path
#' @inheritParams BirdNET
#' @importFrom openxlsx2 wb_load wb_add_border wb_set_col_widths wb_add_filter wb_freeze_pane
#' @export
#'
reformat_xlsx <- function(path, model = c('BirdNET v2.4', 'Perch v2')) {

  if (!dir.exists(path)) stop("provide valid path")
  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    xlsx <- 'BirdNET.xlsx'
  } else if (model == 'Perch v2') {
    xlsx <- 'Perch.xlsx'
  }

  #message("load ", file.path(path, xlsx))
  wb <- openxlsx2::wb_load(file.path(path, xlsx))

  # List of sheets to format
  sheets <- c("Records", "Meta")

  for (sh in sheets) {

    # Get number of rows/cols in the sheet
    data <- openxlsx2::read_xlsx(file.path(path, xlsx), sheet = sh)
    n_rows <- nrow(data) + 1   # +1 for header
    n_cols <- ncol(data)

    # Auto column width
    # message("set colum width")
    wb$set_col_widths(sheet = sh, cols = 1:n_cols, widths = "auto")

    # Add filter to header row
    #message("Add filter")
    wb$add_filter(sheet = sh, rows = 1, cols = 1:n_cols)

    # Freeze first row
    #message("Free first row")
    wb$freeze_pane(sheet = sh, first_active_row = 2)
  }

  # Save workbook
  #message("save workbook")
  wb$save(file.path(path, xlsx))

  ## Close connection
  return("Finished")
}
