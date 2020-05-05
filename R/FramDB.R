#'
#'
#' Constants for dealing with a FRAM Database
#'


FisheryMortSqlFilename <- "FisheryMortalities.sql"
kTotalFisheryMortSqlFilename <- "TotalFisheryMortalities.sql"
kEscapementSqlFilename <- "TotalEscapement.sql"
kFramStockSqlFilename <- "FramStocks.sql"
kFramFisherySqlFilename <- "FramFisheries.sql"
kFramRunInfoSqlFilename <- "RunInfo.sql"
kFramRunTableSqlFilename <- "RunTable.sql"
kFramGetFisheryScalars <- "GetFramFisheryScalars.sql"
kFramGetRunBaseFisheries <- "GetFramRunBaseFisheries.sql"
FramGetRunBaseStocks <- "GetFramRunBaseStocks.sql"

#' Recruit Scalar FRAM SQL scripts  ---------------------------------------------------------
FramGetSingleRecruitScalar <- "GetFramSingleRecruitScalar.sql"
kFramUpdateFisheryScalars <- "UpdateFramFisheryScalars.sql"
kFramGetStockRecSqlFilename <- "GetFramStockRecruitScalars.sql"
FramUpdateRecruitScalars <- "UpdateFramStockRecruitScalars.sql"

#' Non-Retention FRAM SQL scripts  ---------------------------------------------------------
kFramGetSingleNonRetention <- "GetFramSingleNonRetention.sql"
kFramUpdateNonRetention <- "UpdateFramNonRetention.sql"
kFramInsertNonRetention <- "InsertFramNonRetention.sql"
kFramDeleteNonRetention <- "DeleteFramNonRetention.sql"

#' Backward FRAM SQL scripts  ---------------------------------------------------------
kFramBackwardEscSqlFilename <- "FramBackwardEscapement.sql"
FramUpdateBackwardEsc <- "UpdateFramBackwardEsc.sql"
FramInsertBackwardEsc <- "InsertFramBackwardEsc.sql"
FramDeleteBackwardEsc <- "DeleteFramBackwardEsc.sql"
FramGetSingleBackwardEsc <- "GetFramSingleBackwardEscapement.sql"

kCohoSpeciesName <- "COHO"

kFramNonSelectiveScalarFlag <- 1L
kFramNonSelectiveQuotaFlag <- 2L
kFramMsfScalarFlag <- 7L
kFramMsfQuotaFlag <- 8L

FramTargetNotUsedFlag <- 0L
FramTargetEscExactFlag <- 1L
FramTargetEscSplitFlag <- 2L
FramRecruitScalarOverwriteFlag <- 9L


TranslateDbColumnNames <- function(data) {
  names(data)<- gsub("_", ".", names(data))
  return (data)
}


# ===================================================================================================
#' Generic function that retrieves data from database using sql file
#'
#' @param db_conn A connection to the MRP Operational database
#' @param sql_filename A text file containing SQL file
#' @param params Parameters to insert into the SQL statement
#'
#' @importFrom magrittr %>%
#' @importFrom RODBC sqlQuery
#'
#' @export
#'
getFramData <- function(db_conn, sql_file_name, params = NULL, sql_dir = "sql", clean_sql = TRUE, package_name = packageName(parent.frame())) {
  sql <- formatSql(sql_file_name, params, sql_dir, clean_sql, package_name = package_name)

  data_result <- sqlQuery(db_conn, sql) %>%
    TranslateDbColumnNames()

  return(data_result)
}



#' Check if the FRAM database as the new Comments fields for importing, if not add
#' the columns to the appropriate tables
#'
#' @param fram_db_conn An ODBC connection to the FRAM MS Access database
#'
#' @importFrom RODBC sqlColumns sqlQuery
#'
#' @export
#'
checkFramCommentCol <- function(fram_db_conn) {
  error <- FALSE
  if ("Comment" %notin% sqlColumns(fram_db_conn, "FisheryScalers")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE FisheryScalers ADD COLUMN Comment TEXT(255);")
  }

  if ("Comment" %notin% sqlColumns(fram_db_conn, "BackwardsFRAM")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE BackwardsFRAM ADD COLUMN Comment TEXT(255);")
  }

  if ("Comment" %notin% sqlColumns(fram_db_conn, "NonRetention")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE NonRetention ADD COLUMN Comment TEXT(255);")
  }


  if ("Comment" %notin% sqlColumns(fram_db_conn, "NonRetention")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE NonRetention ADD COLUMN Comment TEXT(255);")
  }


}



#' Retrieve all FRAM runs in a specific data base and species
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param species.name The species for the query
#'
#' @return FRAM run details in a data frame
#' @export

getFramRunTable <- function (fram.db.conn, species.name = kCohoSpeciesName) {
  params <- list(SPECIESNAME=species.name)
  data <- getFramData(fram.db.conn, sql_file_name = kFramRunTableSqlFilename, params = params,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}

#' Retrieve the details about a specific FRAM run, by run name
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The FRAM run name that details are requested for
#'
#' @return FRAM run details in a data frame
#'
#' @export

getFramRunInfo <- function (fram.db.conn, fram.run.name) {
  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, sql_file_name = kFramRunInfoSqlFilename,
                      params =   params <- list(RUNNAME=fram.run.name),
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}

#' A helper function loading the list of FRAM stocks
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#'
#' @return A data fram of FRAM stocks
#'
#' @export
#'
getFramStocks <- function (fram.db.conn) {

  data <- getFramData(fram.db.conn, sql_file_name = kFramStockSqlFilename)
  return (data)
}

#' A helper function loading the list of FRAM fisheries in the database
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#'
#' @return A data frame of FRAM fisheries
#' @export
#'
getFramFisheries <- function (fram.db.conn) {

  data <- getFramData(fram.db.conn, sql_file_name = kFramFisherySqlFilename)
  return (data)
}

#' Get the dataframe of fishery scalars used to parameterize model runs
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#' @param run.name The name of the model run you would like to retrive fishery scalars from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#' @export
#'
getFramFisheryScalars <- function (fram.db.conn, fram.run.name) {

  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                      sql_file_name = kFramGetFisheryScalars,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}

#' Get the data frame of stock recruit scalars for particular model runs
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A data frame with the stock recruit scalars for a specific model run name
#' @export
#'
getFramStockRecruitScalars <- function (fram.db.conn, fram.run.name) {
  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                      sql_file_name = kFramGetStockRecSqlFilename,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}


#' Get the dataframe of valid fisheries and time steps from the base period of a specific model run
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#' @export
#'
getFramBaseFisheries <- function (fram.db.conn, fram.run.name) {

  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                      sql_file_name = kFramGetRunBaseFisheries,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}


#' Get the data frame of valid stocks from the base period of a specific model run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#' @export
#'
getFramBaseStocks <- function (fram.db.conn, fram.run.name) {

  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                      sql_file_name = FramGetRunBaseStocks,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}


#' Update the fishery scalars and non retention values for an identified model run based on
#' values in a dataframe.  The Non-Retention CNR mortalities updates more intellegently (e.g.
#' remove/adding/updating DB rows based on values provided and values within the database run)
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.id The ID of the FRAM model run to update fishery scalars for
#' @param fishery.scalars The name of the model run you would like to retrive fishery scalars from
#' @export
#'
updateFisheryScalars <- function (fram.db.conn, fram.run.id, fishery.scalars) {


  fishery.scalars$comment.catch[is.na(fishery.scalars$comment.catch)] <- ""
  fishery.scalars$comment.cnr[is.na(fishery.scalars$comment.cnr)] <- ""

  for (row.idx in 1:nrow(fishery.scalars)) {

    params <- list(RUNID = fram.run.id,
                   FISHERYID = fishery.scalars$fram.fishery.id[row.idx],
                   TIMESTEP = fishery.scalars$fram.time.step[row.idx],
                   FISHERYFLAG = fishery.scalars$fishery.flag[row.idx],
                   NONSELECTIVECATCH = fishery.scalars$nonselective.catch[row.idx],
                   MSFCATCH = fishery.scalars$msf.catch[row.idx],
                   MARKRELEASERATE = fishery.scalars$mark.release.rate[row.idx],
                   MARKMISIDRATE = fishery.scalars$mark.missid.rate[row.idx],
                   UNMARKMISSIDRATE = fishery.scalars$unmark.missid.rate[row.idx],
                   MARKINCIDENTALRATE = fishery.scalars$mark.incidental.rate[row.idx],
                   COMMENT=fishery.scalars$comment.catch[row.idx])

    data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                        sql_file_name = kFramUpdateFisheryScalars,
                        package_name = packageName(parent.frame()))
    # package_name = "CotcAnnualReport")


    cnr.mortalities <- as.numeric(fishery.scalars$cnr.mortalities[row.idx])

    params <- list(RUNID = fram.run.id,
                   FISHERYID = fishery.scalars$fram.fishery.id[row.idx],
                   TIMESTEP = fishery.scalars$fram.time.step[row.idx])

    nonret.data <- getFramData(fram.db.conn, kFramGetSingleNonRetention, params = params, package_name = "CotcAnnualReport")

    if (is.na(cnr.mortalities)) {
      if (nrow(nonret.data) > 0) {
        #remove the CNR Mortality entry
        params <- list(RUNID = fram.run.id,
                       FISHERYID = fishery.scalars$fram.fishery.id[row.idx],
                       TIMESTEP = fishery.scalars$fram.time.step[row.idx])

        data <- getFramData(fram.db.conn, kFramDeleteNonRetention, params = params)
      } else {
        #no data provided and no data in DB, so nothing to do.
      }
    } else {
      params <- list(RUNID = fram.run.id,
                     FISHERYID = fishery.scalars$fram.fishery.id[row.idx],
                     TIMESTEP = fishery.scalars$fram.time.step[row.idx],
                     CNRMORTALITIES = cnr.mortalities,
                     COMMENT = fishery.scalars$comment.cnr[row.idx])
      if (nrow(nonret.data) > 0){

        if (cnr.mortalities != nonret.data$cnr.mortalities) {
          #Updating the CNR value becaues it has changed
          data <- getFramData(fram.db.conn, kFramUpdateNonRetention, params = params)
        } else {
          #Value hasn't changed so do nothing.
        }
      } else {
        #Insert a new NonRetention row into the database.
        data <- getFramData(fram.db.conn, kFramInsertNonRetention, params = params)
      }
    }
  }
  return ()
}


#' Update the stock recruit scalars and the backward FRAM target escapement values/flags.
#' The target escapement updates more intellegently (e.g. remove/adding/updating DB rows based on
#' flag and values provided and values within the database run)
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.id The ID of the FRAM model run to update fishery scalars for
#' @param fishery.scalars The name of the model run you would like to retrive fishery scalars from
#' @export
#'
updateTargetEscapement <- function (fram.db.conn, fram.run.id, escapement_df) {

  for (row_idx in 1:nrow(escapement_df)) {



    params <- list(RUNID = fram.run.id,
                   STOCKID = escapement_df$fram.stock.id[row_idx])

    db_recruit <- getFramData(fram.db.conn, FramGetSingleRecruitScalar, params = params)

    if(nrow(db_recruit) == 0) {
      db_recruit <- data.frame(recruit.scalar = as.numeric(NA))
    }



    if (escapement_df$escapement.flag[row_idx] == FramRecruitScalarOverwriteFlag)
    {

      RS.error <- FALSE

      if(escapement_df$recruit.scalar[row_idx] == 0){
        cat(sprintf("ERROR - '%s' recruit scalar set to update but recruit scalar set to 0",
                    escapement_df$fram.stock.name[row_idx]))
      }

      if(is.na(db_recruit$recruit.scalar)) {
        cat(sprintf("ERROR - '%s' recruit scalar not defined in the database, but a value has been provided in import (%f).\n\n",
                    escapement_df$fram.stock.name[row_idx],
                    escapement_df$recruit.scalar[row_idx]))
        RS.error <- TRUE
      }

      if(escapement_df$target.escapement[row_idx] > 0){
        cat(sprintf("ERROR - Recruit scalar update flag set but target escapement value provided - please change flag for '%s'\n\n",
                    escapement_df$fram.stock.name[row_idx]))
        RS.error <- TRUE
      }

      if(RS.error == TRUE){
        stop("error with recruit scalar flagging \n")
      }else{

        if(escapement_df$recruit.scalar[row_idx] ==  db_recruit$recruit.scalar){
          cat(sprintf("WARNING - Requested recruit scalar update for '%s' but provided and db recruit sclars are the same.\n\n",
                      escapement_df$fram.stock.name[row_idx]))
        }

        if(escapement_df$recruit.scalar[row_idx] !=  db_recruit$recruit.scalar & !is.na(db_recruit$recruit.scalar)){

          cat(sprintf("INFO - Recruit scalar being updated for '%s' \n\n",
                      escapement_df$fram.stock.name[row_idx]))

          variables <- list(RUNID = fram.run.id,
                            STOCKID = escapement_df$fram.stock.id[row_idx],
                            RECRUITSCALAR = escapement_df$recruit.scalar[row_idx])

          data <- RunSqlFile(fram.db.conn, FramUpdateRecruitScalars, variables)
        }
      }
    }

    if(!is.na(db_recruit) & escapement_df$escapement.flag[row_idx] == FramTargetNotUsedFlag &  escapement_df$pair_esc_flag[row_idx] != 2){
      cat(sprintf("Warning - stock '%s' requires flag \n\n",  escapement_df$fram.stock.name[row_idx]))
      # stop("No flag provided on escapement stock")
    }

    esc.flag <- as.numeric(escapement_df$escapement.flag[row_idx])
    if (esc.flag == FramRecruitScalarOverwriteFlag){
      esc.flag <- FramTargetNotUsedFlag
    }


    target.escapement <- as.numeric(escapement_df$target.escapement[row_idx])
    if (is.na(target.escapement)) {
      target.escapement <- 0
    }

    comment <- escapement_df$comment[row_idx]
    if (is.na(comment)) {
      comment <- ""
    }

    params <- list(RUNID = fram.run.id,
                      STOCKID = escapement_df$fram.stock.id[row_idx])

    esc.data <- getFramData(fram.db.conn, FramGetSingleBackwardEsc, params = params)


    params <- list(RUNID = fram.run.id,
                      STOCKID = escapement_df$fram.stock.id[row_idx],
                      ESCAPEMENTFLAG = esc.flag,
                      TARGETESCAPEMENT = target.escapement,
                      COMMENT=comment)
    if (nrow(esc.data) > 0) {
      data <- getFramData(fram.db.conn, FramUpdateBackwardEsc, params  = params)
    } else {
      #Insert a new Backward FRAM Escapement Target row into the database.
      data <- getFramData(fram.db.conn, FramInsertBackwardEsc, params = params)
    }
  }

  return ()
}

#' A helper function loading the total mortalities for all fisheries and time steps within a FRAM model run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to load fishery mortalities for
#'
#' @return A dataframe with the mortalities from the FRAM fisheries and time steps for a specific model run name
#'
#' @section Exceptions:
#'   The method checks the run year of the model run against a provided value, if they don't match
#'   then the method throws an exception.
#'
#' @export
#'
getFramFisheryMortality <- function (fram.db.conn, run.name, run.year) {

  params <- list(RUNNAME=run.name)
  data <- getFramData(fram.db.conn, FisheryMortSqlFilename, params = params)

  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for fishery mortality, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }
  return (data)
}


#' A helper function loading the total mortalities for all fisheries within a FRAM model run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to load fishery mortalities for
#' @param run.year return year for the run.  Usually the previous calender year
#'
#' @return A dataframe with the mortalities from the FRAM fisheries for a specific model run name
#'
#' @section Exceptions:
#'   The method checks the run year of the model run against a provided value, if they don't match
#'   then the method throws an exception.
#'
#' @export
#'

getFramTotalFisheryMortality <- function (fram.db.conn, run.name, run.year) {

  params <- list(RUNNAME=run.name)
  data <- getFramData(fram.db.conn, kTotalFisheryMortSqlFilename, params = params)

  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for fishery mortality, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }
  return (data)
}

#' A helper function loading the stock specific escapement from a FRAM model run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to load fishery mortalities for
#' @param run.year The run year for the run name, used as a cross check when loading the data
#'
#' @return A dataframe with the mortalities from the FRAM fisheries for a specific model run name
#'
#' @section Exceptions:
#'   The method checks the run year of the model run against a provided value, if they don't match
#'   then the method throws an exception.
#'
#' @export
#'
getFramTotalEscapement <- function (fram.db.conn, run.name, run.year) {
  params <- list(RUNNAME=run.name)
  data <- getFramData(fram.db.conn, kEscapementSqlFilename, params = params)

  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for escapement, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }

  return (data)
}

#' A helper function retrieving the escapement values used by the backward FRAM during post-season run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to retrieve backward FRAM Escapement values for
#'
#' @return A data frame with the Backward FRAM escapement data, based on the model run name provided
#'
#' @export
#'
getFramBackwardEscapement <- function (fram.db.conn, fram.run.name) {
  params <- list(RUNNAME=fram.run.name)
  data <- getFramData(fram.db.conn, params = params, sql_dir = "sql",
                      sql_file_name = kFramBackwardEscSqlFilename,
                      package_name = packageName(parent.frame()))
  # package_name = "CotcAnnualReport")
  return (data)
}


#' A function that loads the PSC stock and fishery reference tables from CSV files.
#' The resulting tables are combined into a list
#'
#' @param data.dir The directory where there reference csv files are saved
#'
#' @return A list with the psc.stock, psc.stock.map, psc.fishery, and psc.fishery.map dataframes
#'
#' @export
#'
loadPscData <- function(data.dir) {


  psc.fishery <- ReadCsv("PSCFisheries.csv", data.dir, unique.col.names=c("psc.fishery.id"))
  psc.fishery.map <- ReadCsv("PSCFisheryMap.csv", data.dir, unique.col.names=c("fram.fishery.id"))
  psc.stock <- ReadCsv("PSCStocks.csv", data.dir, unique.col.names=c("psc.stock.id"))
  psc.stock.map <- ReadCsv("PSCStockMap.csv", data.dir, unique.col.names=c("fram.stock.id"))

  result.list <- list(psc.fishery = psc.fishery,
                      psc.fishery.map = psc.fishery.map,
                      psc.stock = psc.stock,
                      psc.stock.map = psc.stock.map)

  return (result.list)
}
