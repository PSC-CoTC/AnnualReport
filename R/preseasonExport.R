#' Writes of preseaon FRAM data
#'
#'
#' @param run.year The fishery year for the model run
#' @param pre.season.run.name Run name of the pre season run fron the RunID table
#' @param pre.season.fram.db Fram DB containing the pre season run.  NA opens file selection
#' @param pre.season.tamm Tamm excel for from pre season run.  NA opens file selection
#' @param pre.season.tamm.fishery.ref Fishery references for pre season TAMM file
#' @param pre.season.tamm.esc.ref Escapement references for pre season TAMM file
#' @param template.dir Template folder
#' @param report.dir directory to write reports to
#' @param data.dir folder containing the reference .csv files
#' @param combine.GS logical if GS MUs should be combined
#'
#' @importFrom RODBC odbcConnectAccess odbcClose
#' @importFrom rmarkdown render
#' @importFrom dplyr mutate_if bind_rows rename first group_by summarise ungroup if_else
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#'
#' @export
#'
#'




getPreseasonERsfunction <- function(run.year,
                        pre.season.run.name,
                        pre.season.fram.db = NA,
                        pre.season.tamm = NA,
                        pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                        pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                        template.dir = "./templates/",
                        report.dir = "./report/",
                        data.dir = "./csv/",
                        combine.GS = TRUE) {

  wb <- openxlsx::createWorkbook()

  if (is.na(pre.season.fram.db)){
    pre.season.fram.db <- choose.files(caption = "choose PRESEASON FRAM DB")
  }

  if (is.na(pre.season.tamm)) {
    pre.season.tamm <- choose.files(caption = "choose PRESEASON tamm")
  }


  psc.data.list <- loadPscData(data.dir)

  pre.season.db.conn <- odbcConnectAccess(pre.season.fram.db)


  fisheries <- getFramFisheries(pre.season.db.conn)
  fram.stocks <- getFramStocks(pre.season.db.conn)


  if (!is.na(pre.season.tamm)) {
    pre.season.tamm <- normalizePath(pre.season.tamm)
    if (!exists("pre.season.tamm.fishery.ref")) {
      stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.fishery.ref\"")
    }
    if (!exists("pre.season.tamm.esc.ref")) {
      stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.esc.ref\"")
    }
  }

  pre.tamm.list <- GetTammData(pre.season.tamm,
                               pre.season.tamm.fishery.ref,
                               pre.season.tamm.esc.ref)




  run.name <- pre.season.run.name

  run.info <- getFramRunInfo(pre.season.db.conn, run.name)

  run.year <- run.info$run.year
  run.id <- run.info$fram.run.id

  fishery.mortality <- getFramFisheryMortality(pre.season.db.conn, run.name, run.year)

  by.stock <- group_by(fishery.mortality, fram.stock.id)
  stock.mort <- summarise(by.stock, total.fishery.mortality = sum(fishery.mortality, na.rm=TRUE))

  escapement <- getFramTotalEscapement (pre.season.db.conn, run.name, run.year) %>%
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

  max.run.year <- max(er.table$run.year)
  min.run.year <- min(er.table$run.year)


  mort.stock.list <- CompilePscData(fram.db.conn = pre.season.db.conn, run.name = run.name, run.year = 2018, psc.data.list = psc.data.list, tamm.data.list = pre.tamm.list,  report.dir = "./report/", combine.GS = TRUE)

  mort <- mort.stock.list$fishery.mortality
  stocks <- mort.stock.list$stock.summary
  run.info <- mort.stock.list$run.info


  file.name <- paste0(report.dir, "preseason_Fishery_ERs_", min.run.year, "-", max.run.year, "_", GetTimeStampText(), ".xlsx")

  addWorksheet(wb = wb, sheetName = paste0("Fishery_ERs_", min.run.year, "-", max.run.year))
  writeData(wb, sheet = paste0("Fishery_ERs_", min.run.year, "-", max.run.year), er.table)

  addWorksheet(wb, sheetName = "Fishery_mortality")
  writeData(wb, sheet =  "Fishery_mortality", x = mort)

  addWorksheet(wb, sheetName = "stock_summary")
  writeData(wb, "stock_summary", stocks)

  addWorksheet(wb, "Run_info")
  writeData(wb, "Run_info", run.info)

  file.name <- paste0(".//report//preseaon_export_",pre.season.run.name, "_", GetTimeStampText() ,".xlsx")

  saveWorkbook(wb =  wb, file = file.name)

}
