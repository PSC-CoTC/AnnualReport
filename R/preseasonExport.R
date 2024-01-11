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
#' @importFrom odbc dbConnect dbDisconnect odbc
#' @importFrom rmarkdown render
#' @importFrom dplyr mutate_if bind_rows rename first group_by summarise ungroup if_else select arrange
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom knitr kable
#' @importFrom kableExtra add_header_above column_spec landscape kable_styling footnote footnote_marker_symbol row_spec column_spec linebreak kbl
#' @importFrom readxl read_excel
#' @importFrom scales percent
#' @importFrom tidyselect everything
#' @importFrom stringr str_replace
#'
#' @export
#'
#'




getPreseasonERs <- function(run.year,
                        pre.season.run.name,
                        pre.season.fram.db = NA,
                        pre.season.tamm = NA,
                        pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                        pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                        template.dir = "./templates/",
                        report.dir = "./report/",
                        data.dir = "./csv/",
                        combine.GS = TRUE,
                        big.bar.esc = 0,
                        big.bar.morts = 0,
                        connection_driver = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};") {

  wb <- openxlsx::createWorkbook()

  if (is.na(pre.season.fram.db)){
    pre.season.fram.db <- choose.files(caption = "choose PRESEASON FRAM DB")
  }

  if (is.na(pre.season.tamm)) {
    pre.season.tamm <- choose.files(caption = "choose PRESEASON tamm")
  }


  psc.data.list <- loadPscData(data.dir)

  pre.season.connect <-
    paste0(connection_driver,
           "DBQ=",
           pre.season.fram.db)

  pre.season.db.conn <- dbConnect(odbc(), .connection_string = pre.season.connect)



  pre.season.db.name <- basename(pre.season.fram.db)

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

  run.info <- getFramRunInfo(pre.season.db.conn, fram.run.name = run.name)

  run.year <- run.info$run.year
  run.id <- run.info$fram.run.id

  fishery.mortality <- getFramFisheryMortality(pre.season.db.conn, run.name, run.year)

  if (is.null(pre.tamm.list) == FALSE) {
    tamm.fishery <- pre.tamm.list$tamm.fishery.mortalities
    fishery.mortality <- left_join(fishery.mortality,
                                   tamm.fishery,
                                   by=c("fram.fishery.id", "fram.stock.id"))
    tamm.value.row <- !is.na(fishery.mortality$tamm.value)
    fishery.mortality$total.mortality[tamm.value.row] <- fishery.mortality$tamm.value[tamm.value.row]
    fishery.mortality <- select(fishery.mortality, -one_of("tamm.value")) %>%
      mutate(fishery.mortality = if_else(!is.na(total.mortality), total.mortality, fishery.mortality)) %>%
      select(-total.mortality)
  }



  by.stock <- group_by(fishery.mortality, fram.stock.id)
  stock.mort <- summarise(by.stock, total.fishery.mortality = sum(fishery.mortality, na.rm=TRUE))

  escapement <- getFramTotalEscapement (pre.season.db.conn, run.name, run.year) %>%
    left_join(stock.mort, by=c("fram.stock.id")) %>%
    left_join(fram.stocks, by=c("fram.stock.id"))

  if (is.null(pre.tamm.list) == FALSE) {
    tamm.esc <- pre.tamm.list$tamm.escapement
    escapement <- left_join(escapement,
                            tamm.esc,
                            by=c("fram.stock.id"))
    tamm.value.row <- !is.na(escapement$tamm.value)
    escapement$escapement[tamm.value.row] <- escapement$tamm.value[tamm.value.row]
    escapement <- select(escapement, -one_of("tamm.value"))
  }

  fishery.mortality <- left_join(fishery.mortality, escapement, by=c("fram.run.id", "run.year", "fram.stock.id")) %>%
    mutate(cohort.age.3 = total.fishery.mortality + escapement) %>%
    mutate(fishery.er = fishery.mortality / cohort.age.3) %>%
    left_join(fisheries, by=c("fram.fishery.id"))



  if(exists("er.table")){
    er.table <-  bind_rows(er.table, fishery.mortality)
  }else{
    er.table <- fishery.mortality
  }

  er.table <- er.table %>%
    select(fram.run.id:fram.stock.id, fram.stock.name, fram.fishery.long.name, everything()) %>%
    select(-fram.fishery.name)

  max.run.year <- max(er.table$run.year)
  min.run.year <- min(er.table$run.year)


  pre.season.data <- CompilePscData(fram.db.conn = pre.season.db.conn, run.name = run.name, run.year = run.year, psc.data.list = psc.data.list, tamm.data.list = pre.tamm.list,  report.dir = report.dir, combine.GS = combine.GS)

  mort <- pre.season.data$fishery.mortality %>%
    arrange(psc.stock.order, psc.fishery.order) %>%
    mutate(group.code = if_else(psc.fishery.name == "SEAK All", "SEAK", group.code))

  mort_country <- mort %>%
    group_by(psc.stock.name, group.code) %>%
    mutate(group.code = if_else(group.code == "Southern U.S.", "SUSA", group.code)) %>%
    summarise(fishery.mortality = sum(fishery.mortality),
              er = sum(er)) %>%
    ungroup() %>%
    pivot_wider(., names_from = group.code, values_from = c(fishery.mortality, er), names_sep = ".")


  stocks <- pre.season.data$stock.summary %>%
    left_join(mort_country, by = c("psc.stock.name")) %>%
    select(psc.stock.id:run.year, cohort, escapement,
           "fishery.mortality.total"=fishery.mortality, fishery.mortality.BC, fishery.mortality.SUSA,  fishery.mortality.SEAK,
           er.total = er, er.BC,  er.SUSA, er.SEAK,
           everything())

  run.info <- pre.season.data$run.info


  file.name <- paste0(report.dir, "preseason_Fishery_ERs_", min.run.year, "-", max.run.year, "_", GetTimeStampText(), ".xlsx")

  addWorksheet(wb = wb, sheetName = paste0("Fishery_ERs_", min.run.year, "-", max.run.year))
  writeData(wb, sheet = paste0("Fishery_ERs_", min.run.year, "-", max.run.year), er.table)

  addWorksheet(wb, sheetName = "Fishery_mortality")
  writeData(wb, sheet =  "Fishery_mortality", x = mort)

  addWorksheet(wb, sheetName = "stock_summary")
  writeData(wb, "stock_summary", stocks)

  addWorksheet(wb, "Run_info")
  writeData(wb, "Run_info", run.info)

  file.name <- paste0(".//report//preseason_export_", pre.season.run.name, "_", GetTimeStampText() ,".xlsx")

  saveWorkbook(wb =  wb, file = file.name)


  output_file_name <- paste0(run.year, "_", "Preseason_report", "_", GetTimeStampText(), ".pdf")

  if(big.bar.esc == 0){
    system.file("preseasonReport.Rmd", package = packageName()) %>%
      rmarkdown::render(.,
                        output_file = output_file_name, output_dir = report.dir)
  }else{
    system.file("PreseasonReport_BigBar.Rmd", package = packageName()) %>%
      rmarkdown::render(.,
                        output_file = output_file_name, output_dir = report.dir)

  }


}
