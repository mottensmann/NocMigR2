#' Format database (db.xlsx)
#'
#' @param path path
#' @import openxlsx2
#'
reformat_db <- function(path) {

  if (!file.exists(path)) stop("provide valid path")

  wb <- openxlsx2::wb_load(path)

  # Get data for dimensions
  data <- openxlsx2::wb_read(path, sheet = "Records")
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)

  # (i) Auto column width
  wb <- openxlsx2::wb_set_col_widths(wb = wb, sheet = "Records",
                                     cols = 1:n_cols, widths = "auto")

  # (ii) Add borders to all cells
  border_style <- openxlsx2::wb_get_base_font(wb)  # just a carrier
  wb <- openxlsx2::wb_add_border(
    wb, sheet = "Records",
    dims = openxlsx2::wb_dims(rows = 1:n_rows, cols = 1:n_cols),
    left_border = "medium", right_border = "medium",
    top_border = "medium", bottom_border = "medium"
  )

  # (iii) Add filter to header row
  wb <- openxlsx2::wb_add_filter(wb, sheet = "Records",
                                 rows = 1, cols = 1:n_cols)

  # (iv) Freeze first row
  wb <- openxlsx2::wb_freeze_pane(wb, sheet = "Records", first_active_row = 2)

  # (v) Hyperlink
  hyperlink_col <- which(names(data) == "File")
  urls_full     <- data[["File"]]
  urls_short    <- basename(urls_full)

  wb <- openxlsx2::wb_add_formula(
    wb, sheet = "Records",
    dims = openxlsx2::wb_dims(rows = 2:(n_rows), cols = hyperlink_col),
    x = paste0('=HYPERLINK("', urls_full, '","', urls_short, '")')
  )

  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
}

#' Format BirdNET.xlsx in a nice style
#'
#' @param path path
#' @inheritParams BirdNET
#' @import openxlsx
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

  wb <- openxlsx::loadWorkbook(file.path(path, xlsx))

  # List of sheets to format
  sheets <- c("Records", "Meta")

  # Define border style once
  border_style <- openxlsx::createStyle(border = "TopBottomLeftRight")

  for (sh in sheets) {

    # Get number of rows/cols in the sheet
    data <- openxlsx::readWorkbook(file.path(path, xlsx), sheet = sh)
    n_rows <- nrow(data) + 1   # +1 for header
    n_cols <- ncol(data)

    # (i) Auto column width
    openxlsx::setColWidths(wb, sheet = sh, cols = 1:n_cols, widths = "auto")

    # (ii) Add borders to all cells
    openxlsx::addStyle(
      wb, sheet = sh, style = border_style,
      rows = 1:n_rows, cols = 1:n_cols,
      gridExpand = TRUE, stack = TRUE
    )

    # (iii) Add filter to header row
    openxlsx::addFilter(wb, sheet = sh, rows = 1, cols = 1:n_cols)

    # (iv) Freeze first row
    openxlsx::freezePane(wb, sheet = sh, firstActiveRow = 2)
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file.path(path, xlsx), overwrite = TRUE)
}
