#' Create Import Files
#'
#' @param fram.db.name .mdb file that contains the preason runs
#' @param fram.run.name Preseason run name to use to
#' @param report.dir the path where the import files are written to
#' @param data.dir path where the .csv files for the package are located
#' @importFrom RODBC odbcConnectAccess sqlColumns
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr inner_join right_join left_join select filter if_else select arrange mutate
#' @importFrom tidyselect one_of
#' @importFrom stringr str_replace_all
#' @importFrom tidyr replace_na
#' @importFrom tidyselect everything
#'
#' @export

createImportFiles <- function(fram.run.name, fram.db.name = NA, report.dir = "./import_files/",  data.dir = "./csv/"){


  if(version$arch != "i386"){
    cat("change to 32 bit R!")
    stop()
  }

  if (is.na(fram.db.name)){
    fram.db.name <- file.choose()
  }

  fram.db.conn <- odbcConnectAccess(fram.db.name)

  #~~~~~initialize fishery data

  checkFramCommentCol(fram.db.conn)

  base.fishery <- getFramBaseFisheries(fram.db.conn = fram.db.conn, fram.run.name = fram.run.name)

  person.fishery <- getPersonFramFisheries(data_dir = data.dir)

  fishery.scalars <- getFramFisheryScalars(fram.db.conn = fram.db.conn, fram.run.name = fram.run.name) %>%
    select(-one_of("fram.fishery.name")) %>%
    right_join(base.fishery, by=c("fram.run.id", "fram.fishery.id", "fram.time.step")) %>%
    arrange(fram.run.id, fram.fishery.id, fram.time.step) %>%
    inner_join(person.fishery, by=c("fram.fishery.id")) %>%
    mutate(cnr.mortalities = if_else(is.na(cnr.mortalities), 0, cnr.mortalities)) %>%
    select(fram.fishery.name, everything())

  fram.run.id <- unique(fishery.scalars$fram.run.id)

  if (length(fram.run.id) > 1) {
    stop("ERROR - there is more then one run found, this is a major issue to debug")
  }


  fishery.scalars %<>% select(-one_of("fram.run.id"))

  #~~~~~~initialize escapement

  backward.esc <- getFramBackwardEscapement(fram.db.conn = fram.db.conn, fram.run.name = fram.run.name)


  stock.recruit <- getFramStockRecruitScalars(fram.db.conn = fram.db.conn, fram.run.name = fram.run.name) %>%
    select(-one_of("fram.run.id"))

  stocks <- getFramStocks(fram.db.conn = fram.db.conn)

  fram_run_tbl <- getFramRunTable(fram.db.conn = fram.db.conn)

  header <- "Create Import package v0.1 beta"


  ###### Compile Escapement/Recruitment Data Frame  #####################
  person.stocks <- getPersonFramStocks(data_dir = data.dir)


  escapement <- left_join(stocks, backward.esc, by=c("fram.stock.id")) %>%
    left_join(stock.recruit, by=c("fram.stock.id")) %>%
    replace_na(list(target.escapement = 0,
                    escapement.flag = FramTargetNotUsedFlag,
                    recruit.scalar = 0,
                    fram.run.id = fram.run.id)) %>%
    inner_join( person.stocks, by=c("fram.stock.id"))

    comment.col.name <- "comment"
    escapement <- escapement[,c(setdiff(names(escapement), comment.col.name), comment.col.name)]

    unique.person <- unique(c(person.fishery$person.name, escapement$person.name))
    unique.person <- unique.person[nchar(unique.person) > 0 & !is.na(unique.person)]


    writeImportFile("ALL",
                   fram.run.name,
                   fram.run.id,
                   fram.db.name,
                   select(fishery.scalars,
                          -one_of("fram.run.name", "person.name")),
                   select(escapement,
                          -one_of("person.name", "fram.run.id", "run.year")))


    for (this.person.name in unique.person) {
      person.fishery.scalars <- filter(fishery.scalars,
                                       tolower(person.name) == tolower(this.person.name))

      person.fishery.scalars <- select(person.fishery.scalars,
                                       -one_of("fram.run.name", "person.name"))

      person.escapement <- filter(escapement,
                                  tolower(person.name) == tolower(this.person.name))

      person.escapement <- select(person.escapement,
                                  -one_of("person.name", "fram.run.id", "run.year"))

      writeImportFile(this.person.name,
                      fram.run.name,
                      fram.run.id,
                      fram.db.name,
                      person.fishery.scalars,
                      person.escapement)
    }


}



#' Functions for writing .csv files for CoTC scientists to update and then import to FRAM db
#' used in a loop in creatImportFiles fuction
#'
#' @param person_name the individual scientist to whom the file is sent.  USsd for naming
#' @param fram_run_name the fram run name used to creat the files
#' @param fram_run_id from run number used to create the files
#' @param fram_db_name FRAM database used to create the files database
#' @param person_fishery_scalars dataframe of scalars
#' @param person_escapement dataframe of escapement
#'
#'
#' @export


writeImportFile <- function (person_name,
                             fram_run_name,
                             fram_run_id,
                             fram_db_name,
                             person_fishery_scalars,
                             person_escapement) {
  section_div_line <- "-------------------------------------------------------------\n"
  import.file.name <- sprintf("./import_templates/%s_%s_%s.csv", person_name, fram_run_name, GetTimeStampText())

  cat(sprintf("Creating import file: %s\n", import.file.name))
  import.file <- file(import.file.name, "w+")

  cat(paste0("Person Name=", person_name, "\n"), file = import.file)
  cat(paste0("FRAM Run Name=", fram_run_name, "\n"), file = import.file)
  cat(paste0("FRAM Run ID=", fram_run_id, "\n"), file = import.file)
  cat(paste0("FRAM DB Name=", fram_db_name, "\n"), file = import.file)

  if (nrow(person_fishery_scalars) > 0) {
    cat(section_div_line, file = import.file)

    catch.csv.text <- WriteMemoryCsv(person_fishery_scalars)
    cat(paste0(catch.csv.text, collapse="\n"), "\n", file = import.file)
  }

  if (nrow(person_escapement) > 0) {
    cat(section_div_line, file = import.file)
    esc.csv.text <- WriteMemoryCsv(person_escapement)
    cat(paste0(esc.csv.text, collapse="\n"), "\n", file = import.file)
  }
  close(import.file)
}

