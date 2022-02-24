################
#
# Common methods to deal with a TAMM Spreadsheet Model
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 12, 2017
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

kDecimalRegEx <- "^[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?$"



#' A helper function loading the total mortalities from the TAMM excel spreadsheet
#' based on a the worksheet name, column, and row of the cell value defined in the tamm.ref
#' data frame
#'
#' @param tamm.ref A data frame of cell references to load from the TAMM Excel document
#' @param tamm.filename: The file name of the TAMM Excel document
#'
#' @note  If a cell reference is NA for the worksheet, column, and row, a zero value is automatically filled.
#'
#' @return A dataframe with the FRAM fisheries and associated TAMM mortalties
#'
#' @export
#'
GetTammValues <- function (tamm.ref, tamm.filename) {
  tamm.ref <- arrange(tamm.ref, tamm.worksheet.name)
  tamm.ref$tamm.value <- as.character(NA)

  prev.worksheet.name <- ""

  worksheet.data <- NA

  for(ref.idx in 1:nrow(tamm.ref)) {
    if (is.na(tamm.ref$tamm.worksheet.name[ref.idx]) &
        is.na(tamm.ref$tamm.cell.row[ref.idx]) &
        is.na(tamm.ref$tamm.cell.col[ref.idx])) {
      #No Cell Reference provided, so zero out the original FRAM value
      tamm.ref$tamm.value[ref.idx] <- "0"

    } else {
      if (tamm.ref$tamm.worksheet.name[ref.idx] != prev.worksheet.name) {
        worksheet.data <- read_excel(tamm.filename,
                                     tamm.ref$tamm.worksheet.name[ref.idx],
                                     col_names = FALSE)

        cat(sprintf("Loading data from excel worksheet: %s\n",
                    tamm.ref$tamm.worksheet.name[ref.idx]))

        prev.worksheet.name <- tamm.ref$tamm.worksheet.name[ref.idx]

      }

      tamm.ref$tamm.value[ref.idx] <- worksheet.data[tamm.ref$tamm.cell.row[ref.idx],
                                                     tamm.ref$tamm.cell.col[ref.idx]][[1]]
    }
  }

  decimal.check <- grepl(kDecimalRegEx, tamm.ref$tamm.value)

  if (any(!decimal.check)) {

    cat("The following TAMM References do no return decimal values:\n\n")
    cat(paste(names(tamm.ref), collapse=","))
    cat("\n")

    cat(paste(tamm.ref[!decimal.check,], collapse=",", sep="\n"))
    cat("\n")

    stop("A TAMM cell reference is not returing a decimal value, this must be fixed continue generating the report.")
  }

  tamm.ref$tamm.value <- as.numeric(tamm.ref$tamm.value)
  return(tamm.ref)
}

#' Reads the specific Stock/Fishery mortality values from a TAMM model defined in the
#' provided file.
#'
#' @param tamm.filename The file name of TAMM excel spreadsheet
#' @param tamm.fishery.ref.filename The file name containing
#'
#' @return A dataframe with the FRAM fisheries/stock combination and associated TAMM mortalties
#'
#' @note The method checks that values read for the Excel spreadsheet are numeric values.
#'
#' @export
#'
GetTammFisheryMortality <- function (tamm.filename,
                                     tamm.fishery.ref.filename) {
  tamm.fishery.ref <-
    ReadCsv(tamm.fishery.ref.filename, NA, unique.col.names=c("fram.stock.id", "fram.fishery.id")) %>%
    mutate(tamm.worksheet.name = as.character(tamm.worksheet.name)) %>%
    GetTammValues(tamm.filename) %>%
    select(fram.stock.id, fram.fishery.id, tamm.value)

  return (tamm.fishery.ref)
}

#' Reads the specific Escapement values from a TAMM model defined in the
#' tamm.esc.ref.filename
#'
#' @param tamm.filename The file name of TAMM excel spreadsheet
#' @param tamm.esc.ref.filename The file name of TAMM excel spreadsheet
#'
#' @returns  A dataframe with the FRAM stock ID and associated TAMM escapement
#'
#' @note The method checks that values read for the Excel spreadsheet are numeric values.
#'
#' @export
#'
GetTammEscapement <- function (tamm.filename,
                               tamm.esc.ref.filename) {
  tamm.esc.ref <-
    ReadCsv(tamm.esc.ref.filename, NA, unique.col.names=c("fram.stock.id")) %>%
    mutate(tamm.worksheet.name = as.character(tamm.worksheet.name)) %>%
    GetTammValues(tamm.filename) %>%
    select(fram.stock.id, tamm.value)

  return (tamm.esc.ref)
}

#' Reads the various values of the TAMM spreadsheet and packages data into a list
#'
#' @param tamm.filename The file name of TAMM excel spreadsheet
#' @param data.dir Directory that TAMM reference files are defined
#'
#' @return A list with a dataframe for fishery mortalities and escapement values
#'
#' @export
#'
GetTammData <- function (tamm.filename, tamm.fishery.ref.filename, tamm.esc.ref.filename) {
  result.list <- list(tamm.fishery.mortalities = GetTammFisheryMortality(tamm.filename, tamm.fishery.ref.filename),
                      tamm.escapement = GetTammEscapement(tamm.filename, tamm.esc.ref.filename))

  return (result.list)
}

