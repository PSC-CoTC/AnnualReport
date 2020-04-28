#' Writes .pdf document of the annual report
#'
#'
#' @param run.year The fishery year for the model run
#' @param post.season.run.name Run name of the post season run fron the RunID table
#' @param pre.season.run.name Run name of the pre season run fron the RunID table
#' @param post.season.fram.db Fram DB containing the post season run.  Deafaults to NA which prompts a selection to dialog
#' @param post.season.tamm Tamm excel file containing the post season data. Deafaults to NA which prompts a selection to dialog
#' @param post.season.tamm.fishery.ref Fishery references for post season TAMM file
#' @param post.season.tamm.esc.ref Escapement reference of post season TAMM
#' @param pre.season.fram.db Fram DB containing the pre season run.  NA opens file selection
#' @param pre.season.tamm Tamm excel for from pre season run.  NA opens file selection
#' @param pre.season.tamm.fishery.ref Fishery references for pre season TAMM file
#' @param pre.season.tamm.esc.ref Escapement references for pre season TAMM file
#' @param template.dir Template folder
#' @param report.dir directory to write reports to
#' @param data.dir folder containing the reference .csv files
#'
#' @importFrom RODBC odbcConnectAccess odbcClose
#' @importFrom rmarkdown render
#' @importFrom dplyr mutate_if bind_rows rename first group_by summarise ungroup if_else
#' @importFrom knitr kable
#' @importFrom kableExtra add_header_above column_spec landscape kable_styling footnote footnote_marker_symbol row_spec
#' @importFrom scales percent comma
#' @importFrom stringr str_sub str_locate str_replace str_detect
#' @importFrom readxl read_excel
#'
#' @export
#'
#'



writeAnnualReport <- function(run.year, post.season.run.name, pre.season.run.name,
                              post.season.fram.db = NA,
                              post.season.tamm = NA,
                              post.season.tamm.fishery.ref = "./csv/TammFisheryQueetsRef.csv",
                              post.season.tamm.esc.ref = "./csv/TammEscQueetsRef.csv",
                              pre.season.fram.db = NA,
                              pre.season.tamm = NA,
                              pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                              pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                              template.dir = "./templates/",
                              report.dir = "./report/",
                              data.dir = "./csv/",
                              combine.GS = NA) {


#
#   cat(header)
#   cat("\n")

  if (is.na(pre.season.tamm)) {
    pre.season.tamm <- choose.files(caption = "choose PRESEASON tamm")
  }

  if (is.na(post.season.tamm)) {
    post.season.tamm <- choose.files(caption = "choose POSTSEASON tamm")
  }

  if (is.na(pre.season.tamm) || is.na(post.season.tamm)) {
    if ((is.na(pre.season.tamm) && is.na(post.season.tamm)) == FALSE) {
      stop("Both the Pre and Post season tamm spreadsheet must be defined and not just one.")
    }
  }

  if(is.na(combine.GS)) {
    combine.GS <- if_else(run.year >= 2020, TRUE, FALSE)
  }


  psc.data.list <- loadPscData(data.dir)

  pre.season.db.conn <- odbcConnectAccess(pre.season.fram.db)

  if (!is.na(pre.season.tamm)) {
    pre.season.tamm <- normalizePath(pre.season.tamm)
    if (!exists("pre.season.tamm.fishery.ref")) {
      stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.fishery.ref\"")
    }
    if (!exists("pre.season.tamm.esc.ref")) {
      stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.esc.ref\"")
    }

    pre.tamm.list <- GetTammData(pre.season.tamm,
                                 pre.season.tamm.fishery.ref,
                                 pre.season.tamm.esc.ref)
  } else {
    pre.tamm.list <- NULL
  }

  pre.season.data <- CompilePscData(pre.season.db.conn, pre.season.run.name, run.year, psc.data.list, pre.tamm.list,
                                    report.dir, combine.GS = combine.GS)

  pre.season.data.orginal <-  CompilePscData(pre.season.db.conn, pre.season.run.name, run.year, psc.data.list, pre.tamm.list,
                                             report.dir, combine.GS = FALSE)

  odbcClose(pre.season.db.conn)

  post.season.db.conn <- odbcConnectAccess(post.season.fram.db)
  if (!is.na(post.season.tamm)) {
    post.season.tamm <- normalizePath(post.season.tamm)
    if (!exists("post.season.tamm.fishery.ref")) {
      stop("If the post season TAMM is defined, the you must define \"post.season.tamm.fishery.ref\"")
    }
    if (!exists("post.season.tamm.esc.ref")) {
      stop("If the post season TAMM is defined, the you must define \"post.season.tamm.esc.ref\"")
    }
    post.tamm.list <- GetTammData(post.season.tamm,
                                  post.season.tamm.fishery.ref,
                                  post.season.tamm.esc.ref)
  } else {
    post.tamm.list <- NULL
  }

  post.season.data <- CompilePscData(post.season.db.conn, post.season.run.name, run.year, psc.data.list, post.tamm.list,
                                     report.dir = report.dir, combine.GS = combine.GS)

  post.season.data.orginal <-  CompilePscData(post.season.db.conn, post.season.run.name, run.year, psc.data.list, post.tamm.list,
                                              report.dir = report.dir, combine.GS = FALSE)

  odbcClose(post.season.db.conn)

  annual.tbl.third <- CreateTable3(post.season.data)
  report.filename <- file.path(report.dir, paste0(run.year, "_annual_table3.csv"))
  WriteCsv(report.filename, annual.tbl.third)
  cat(sprintf("The annual report table 3 written to:\n\t%s\n\n", normalizePath(report.filename)))

  annual.tbl.second <- CreateTable2(pre.season.data, post.season.data, run.year)
  report.filename <- file.path(report.dir, paste0(run.year, "_annual_table2.csv"))
  WriteCsv(report.filename, annual.tbl.second)
  cat(sprintf("The annual report table 2 written to:\n\t%s\n\n", normalizePath(report.filename)))

  annual.tbl.first <- CreateTable1(pre.season.data, post.season.data, run.year)
  report.filename <- file.path(report.dir, paste0(run.year, "_annual_table1.csv"))
  WriteCsv(report.filename, annual.tbl.first)
  cat(sprintf("The annual report table 1 written to:\n\t%s\n\n", normalizePath(report.filename)))

  # report.filename <- paste0(report.dir, run.year,"_", kAnnualReportBaseName, ".html")
  # report.template.filename <- paste0(template.dir, run.year, "_", kAnnualReportBaseName, ".rhtml")
  # if (file.exists(report.template.filename) == FALSE) {
  #   #No year specific template file, so use default annual report file template.
  #   report.template.filename <- paste0(template.dir, kAnnualReportBaseName, ".rhtml")
  #   cat(sprintf("\nWARNING: NO sepcifc annual report template, using default template: %s\n\n",
  #               report.template.filename))
  # }


  stock_summaries <- post.season.data$stock.summary %>%
    filter(psc.stock.name == "Georgia Strait") %>%
    bind_rows(post.season.data.orginal$stock.summary)

  WriteCsv(file = "report/stock_summaries.csv", stock_summaries)

  fishery_morts <- post.season.data$fishery.mortality %>%
    filter(psc.stock.name == "Georgia Strait") %>%
    bind_rows(post.season.data.orginal$fishery.mortality)

  WriteCsv(file = "report/fishery_mortalities.csv", fishery_morts)


  system.file("AnnualReport.rmd", package = packageName()) %>%
    rmarkdown::render(.,
                      output_file = "annualreport.pdf", output_dir = getwd())


}