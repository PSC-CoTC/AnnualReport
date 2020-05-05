#' Writes .pdf document of the annual report
#'
#'
#' @param fram_db Data base with the FRAM runs
#' @param run_names Vector of run names from the FRAM database
#'
#' @importFrom openxlsx addWorksheet createWorkbook saveWorkbook writeData
#'
#' @export


exportFisheryERs <- function(fram.db = NA, run.names, report.dir = "./report/"){

  if (exists("report.dir") == FALSE) {
    report.dir <- "./report/"
    dir.create(report.dir, showWarnings = FALSE)
  }

  if (is.na(fram.db)){
    fram.db <- choose.files(caption = "choose POSTSEASON FRAM DB")
  }

  fram.db.conn <- odbcConnectAccess(normalizePath(fram.db))

  fisheries <- getFramFisheries(fram.db.conn)
  fram.stocks <- getFramStocks(fram.db.conn)

  wb <- openxlsx::createWorkbook()


  for (run_index in seq_along(run.names)){

    run.name <- run.names[run_index]

    run.info <- getFramRunInfo(fram.db.conn, run.name)

    run.year <- run.info$run.year
    run.id <- run.info$fram.run.id

    fishery.mortality <- getFramFisheryMortality(fram.db.conn, run.name, run.year)

    by.stock <- group_by(fishery.mortality, fram.stock.id)
    stock.mort <- summarise(by.stock, total.fishery.mortality = sum(fishery.mortality, na.rm=TRUE))

    escapement <- getFramTotalEscapement (fram.db.conn, run.name, run.year) %>%
      left_join(stock.mort, by=c("fram.stock.id")) %>%
      left_join(fram.stocks, by=c("fram.stock.id"))

    fishery.mortality <- left_join(fishery.mortality, escapement, by=c("fram.run.id", "run.year", "fram.stock.id")) %>%
      mutate(cohort.age.3 = total.fishery.mortality + escapement) %>%
      mutate(fishery.er = fishery.mortality / cohort.age.3)


    if(exists("er.table")){
      er.table <-  bind_rows(er.table, fishery.mortality)
    }else{
      er.table <- fishery.mortality
    }

    addWorksheet(wb = wb, sheetName = run.name)
    openxlsx::writeData(wb, run.name, fishery.mortality)

    max.run.year <- max(er.table$run.year)
    min.run.year <- min(er.table$run.year)

  }

  odbcClose(fram.db.conn)


  file.name <- paste0(report.dir, "Fishery_ERs_", min.run.year, "-", max.run.year, "_", GetTimeStampText(), ".xlsx")

  addWorksheet(wb = wb, sheetName = paste0("Fishery_ERs_", min.run.year, "-", max.run.year))
  openxlsx::writeData(wb, sheet = paste0("Fishery_ERs_", min.run.year, "-", max.run.year), er.table)

  saveWorkbook(wb =  wb, file = file.name)

}

#' Creates a .xlsx workbook of the "Table3" from the annual report (post season ERs)
#'
#' @param fram_db Data base with the FRAM runs
#' @param run_names Vector of run names from the FRAM database
#'
#'
#'
#' @export
#'


postSeasonTable3Summary <- function(fram.db = NA, run.names, combine.GS = TRUE,   data.dir = "./csv/"){

  if (exists("report.dir") == FALSE) {
    report.dir <- "./report/"
    dir.create(report.dir, showWarnings = FALSE)
  }

  if (is.na(fram.db)){
    fram.db <- choose.files(caption = "choose POSTSEASON FRAM DB")
  }

  fram.db.conn <- odbcConnectAccess(normalizePath(fram.db))


  psc.data.list <- loadPscData(data.dir)


  wb <- openxlsx::createWorkbook()

  for (run_index in seq_along(run.names)){

    run.name <- run.names[run_index]

    run.info <- getFramRunInfo(fram.db.conn, run.name)

    run.year <- run.info$run.year

    if(exists("run.year.vec")){
      run.year.vec <- c(run.year, run.year.vec)
    }else{
      run.year.vec <- run.year
    }

    run.id <- run.info$fram.run.id

    post.season.data <- CompilePscData(fram.db.conn, run.name, run.year, psc.data.list, tamm.data.list = NULL,
                                       report.dir = report.dir, combine.GS = combine.GS)

    table.3 <- CreateTable3(post.season.data)


    addWorksheet(wb, sheetName = run.name)
    openxlsx::writeData(wb, run.name, table.3)


    max.run.year <- max(run.year.vec)
    min.run.year <- min(run.year.vec)

  }

  file.name <- paste0(report.dir, "Table3_", min.run.year, "-", max.run.year, "_", GetTimeStampText(), ".xlsx")

  saveWorkbook(wb, file = file.name)

}





