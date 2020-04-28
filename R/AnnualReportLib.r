################
#
# Code to generate annual report tables for the Coho Technical Committee.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 14, 2015
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

#constants section

#Constants to identfy sub totals for rows.
kCanadaGroupCode <- "BC"
kUSGroupCode <- "Southern U.S."
kTotalFisheryName <- "TOTAL"

kCapMethodImu <- "imu" #US Inside Management Unit
kCapMethodOmu <- "omu" #US Outside Management Unit
kCapMethodFixed <- "fixed" #Fixed Canadian ER Management Unit

kUSCapMethods <- c(kCapMethodImu, kCapMethodOmu)
kCdnCapMethods <- c(kCapMethodFixed)

kPstLowStatusER <- 0.2
kPstModerateStatusER <- 0.4
kPstAbundantStatusER <- 0.6

kStatusAbundantCode <- "A"
kStatusModerateCode <- "M"
kStatusLowCode <- "L"



#' Sum columns
#'
#' @param data data frame
#' @param label.column labels
#' @param label.value value
#'
#'
#' @export
#'

GetColumnSums <- function(data, label.column, label.value) {
  column.sums <- t(colSums(data))
  label <- data.frame(label.value)
  names(label) <- label.column
  column.sums <- cbind(label, column.sums)
}


#' Sum up the fishery mortalities from a fram run to PSC Stocks
#'
#'
#' @param  fishery.mortality Fishery mortality values from a FRAM run
#' @param  psc.fishery The list of PSC fisheries
#' @param  psc.fishery.map Relates FRAM fisheries to PSC fisheries
#' @param  psc.stock The list of PSC stocks
#' @param  psc.stock.map Relates FRAM stocks to PSC stocks
#'
#' @return A data frame with PSC fishery mortalities by PSC Stock
#'
#'
#' @export
#'
#'
GetPscMortality <- function(fishery.mortality, psc.fishery, psc.fishery.map, psc.stock, psc.stock.map) {

  psc.stock <- psc.stock[,names(psc.stock) %in% c("psc.stock.id", "psc.stock.name", "psc.stock.order")]

  psc.full.fishery <- merge(psc.fishery, psc.fishery.map, by=c("psc.fishery.id"))

  psc.full.stock <- merge(psc.stock, psc.stock.map, by=c("psc.stock.id"))
  psc.full.fishery <- merge(psc.full.fishery, psc.full.stock, all=TRUE, by=c())


  psc.full.fishery <- merge(fishery.mortality, psc.full.fishery, by=c("fram.fishery.id", "fram.stock.id"), all.y=TRUE)

  psc.full.fishery$total.mortality[is.na(psc.full.fishery$total.mortality)] <- 0
  psc.full.fishery$group.code[is.na(psc.full.fishery$group.code)] <- ""

  aggr.factors <- with(psc.full.fishery,
                       list(run.id=fram.run.id,
                            run.year=run.year,
                            group.code=group.code,
                            psc.fishery.name=psc.fishery.name,
                            psc.fishery.order=psc.fishery.order,
                            psc.stock.name=psc.stock.name,
                            psc.stock.order=psc.stock.order))
  psc.fishery.mortality <- aggregate(data.frame(fishery.mortality=psc.full.fishery$total.mortality), aggr.factors, sum)

  psc.fishery.mortality <- psc.fishery.mortality[order(psc.fishery.mortality$psc.stock.order, psc.fishery.mortality$psc.fishery.order),]
  return(psc.fishery.mortality)
}



#' Sum up the escapement from a fram run to PSC Stocks
#'
#'
#' @param escapement Escapement values from a FRAM run
#' @param psc.stock The list of PSC stocks
#' @param psc.stock.map Relates FRAM stocks to PSC stocks
#'
#' @return A data frame with PSC escapement
#'
#'
#' @export
#'
#'
GetPscEscapement <- function(escapement, psc.stock, psc.stock.map) {

  psc.stock.tbl <- merge(psc.stock, psc.stock.map, by=c("psc.stock.id"))

  psc.escapement <- merge(escapement, psc.stock.tbl, by=c("fram.stock.id"), all.y=TRUE)


  unmatched <- merge(escapement, psc.stock.tbl, by=c("fram.stock.id"), all.x=TRUE)
  psc.escapement$escapement[is.na(psc.escapement$escapement)] <- 0

  aggr.factors <- with(psc.escapement,
                       list(run.id=fram.run.id,
                            run.year=run.year,
                            psc.stock.id=psc.stock.id,
                            psc.stock.name=psc.stock.name,
                            psc.stock.order=psc.stock.order))
  psc.escapement <- aggregate(data.frame(escapement=psc.escapement$escapement), aggr.factors, sum)

  psc.escapement <- psc.escapement[order(psc.escapement$psc.stock.order),]
  return(psc.escapement)
}

#' Make a table of fishery mortality
#'
#'
#' @param fishery.mortality Escapement values from a FRAM run
#' @param column.name The list of PSC stocks
#'
#' @importFrom dplyr pull
#'
#' @return dataframe of fishery mortalities
#'
#'
#' @export
#'
#'

BuildFisheryTable <- function(fishery.mortality, column.name) {
  stock.names <- unique(fishery.mortality[, c("psc.stock.order", "psc.stock.name")])
  stock.names <- stock.names[order(stock.names$psc.stock.order), "psc.stock.name"]

  if(is.vector(stock.names) == FALSE){
    stock.names <- pull(stock.names)
    }

  fisheries <- unique(fishery.mortality[,c("psc.fishery.order", "group.code", "psc.fishery.name")])

  for(col.idx in 1:length(stock.names)) {
    stock.frame <- data.frame(
      psc.fishery.name=fishery.mortality$psc.fishery.name[fishery.mortality$psc.stock.name == stock.names[col.idx]],
      value=fishery.mortality[fishery.mortality$psc.stock.name == stock.names[col.idx], column.name])
    names(stock.frame) <- c("psc.fishery.name", as.character(stock.names[col.idx]))
    fisheries <- merge(fisheries, stock.frame, by=c("psc.fishery.name"), all.x=TRUE)
    fisheries[is.na(fisheries[,ncol(fisheries)]),ncol(fisheries)] <- 0
  }

  fisheries <- fisheries[order(fisheries$psc.fishery.order),]
  fisheries <- fisheries[,names(fisheries) %notin% c("psc.fishery.order")]

  return(fisheries)
}


#' Make a table of fishery mortality
#'
#'
#' @param psc.fishery.table Table of fisheries
#'
#' @return Summarize regional totals
#'
#'
#' @export
#'
#'


SummaryRegionTotals <- function(psc.fishery.table) {

  groups <- unique(psc.fishery.table$group.code)

  fisheries <- psc.fishery.table[0,]
  for(col.idx in 1:length(groups)) {
    curr.group.code <- groups[col.idx]
    group.frame <- psc.fishery.table[psc.fishery.table$group.code == curr.group.code,]
    if (nchar(groups[col.idx]) > 0) {
      sum.values <- colSums(group.frame[,names(group.frame) %notin% c("psc.fishery.name", "group.code")], na.rm=TRUE)
      sum.values <- c(curr.group.code, curr.group.code, sum.values)
      fisheries <- rbind(fisheries, group.frame, sum.values)
    } else {
      fisheries <- rbind(fisheries, group.frame)
    }
  }

  #remove the group code column from the summary (e.g. remove the BC/US code)
  fisheries <- fisheries[,names(fisheries) %notin% c("group.code")]

  return (fisheries[,names(fisheries) %notin% c("group.code")])
}

#' writes .csv of PSC tables
#'
#'
#' @param fram.db.conn connection to fram db
#' @param psc.fishery table
#' @param psc.fishery.map table
#' @param psc.stock table
#' @param psc.stock.map table
#'
#' @return Summarize regional totals
#'
#'
#' @export
#'
#'

WritePSCFramTables <- function(fram.db.conn, psc.fishery, psc.fishery.map, psc.stock, psc.stock.map, report.dir) {
  fram.fisheries <- getFramFisheries(fram.db.conn)

  psc.full.fishery <- merge(psc.fishery, psc.fishery.map, by=c("psc.fishery.id"))
  psc.fram.fishery <- merge(psc.full.fishery, fram.fisheries, by=c("fram.fishery.id"))

  WriteCsv(file.path(report.dir,"PSCtoFramFisheries.csv"), psc.fram.fishery)

  fram.stocks <- getFramStocks(fram.db.conn)
  psc.full.stock <- merge(psc.stock, psc.stock.map, by=c("psc.stock.id"))
  psc.fram.stock <- merge(psc.full.stock, fram.stocks, all=TRUE, by=c("fram.stock.id"))

  WriteCsv(file.path(report.dir,"PSCtoFramStocks.csv"), psc.fram.stock)
}


#'  Compile a FRAM model run into PSC fishery and stock groupings
#'
#'
#' @param fram.db.conn RODBC connection to FRAM access database
#' @param run.name FRAM run name that is to be compiled into PSC fisheries/stocks
#' @param psc.data.list Data list of FRAM to PSC stocks and fisheries
#' @param tamm.data.list Data list related to TAMM Excel Workbook interpretation
#'
#' @return A data frame with PSC fishery mortalities by PSC Stock
#'
#'
#' @export

CompilePscData <- function(fram.db.conn, run.name, run.year, psc.data.list, tamm.data.list, report.dir, combine.GS) {

  run.info <- getFramRunInfo(fram.db.conn, run.name)
  ValidateRunInfo(run.info, run.year)

  psc.fishery <- psc.data.list$psc.fishery
  psc.fishery.map <- psc.data.list$psc.fishery.map
  psc.stock <- psc.data.list$psc.stock
  psc.stock.map <- psc.data.list$psc.stock.map

  WritePSCFramTables(fram.db.conn, psc.fishery, psc.fishery.map, psc.stock, psc.stock.map, report.dir)


  fishery.mortality <- getFramTotalFisheryMortality(fram.db.conn, run.name, run.year)
  if (is.null(tamm.data.list) == FALSE) {
    tamm.fishery <- tamm.data.list$tamm.fishery.mortalities
    fishery.mortality <- left_join(fishery.mortality,
                                   tamm.fishery,
                                   by=c("fram.fishery.id", "fram.stock.id"))
    tamm.value.row <- !is.na(fishery.mortality$tamm.value)
    fishery.mortality$total.mortality[tamm.value.row] <- fishery.mortality$tamm.value[tamm.value.row]
    fishery.mortality <- select(fishery.mortality, -one_of("tamm.value"))
  }

  psc.fishery.mortality <- GetPscMortality(fishery.mortality, psc.fishery, psc.fishery.map, psc.stock, psc.stock.map)

  escapement <- getFramTotalEscapement(fram.db.conn, run.name, run.year)
  if (is.null(tamm.data.list) == FALSE) {
    tamm.esc <- tamm.data.list$tamm.escapement
    escapement <- left_join(escapement,
                            tamm.esc,
                            by=c("fram.stock.id"))
    tamm.value.row <- !is.na(escapement$tamm.value)
    escapement$escapement[tamm.value.row] <- escapement$tamm.value[tamm.value.row]
    escapement <- select(escapement, -one_of("tamm.value"))
  }
  psc.escapement <- GetPscEscapement(escapement, psc.stock, psc.stock.map)

  aggr.factors <- with(psc.fishery.mortality,
                       list(psc.stock.name=psc.stock.name))

  total.fishery.mortality <- aggregate(data.frame(fishery.mortality=psc.fishery.mortality$fishery.mortality), aggr.factors, sum)

  psc.stock.summary <- merge(psc.escapement, total.fishery.mortality, by=c("psc.stock.name"), all=TRUE)
  psc.stock.summary$fishery.mortality[is.na(psc.stock.summary$fishery.mortality)] <- 0
  psc.stock.summary$escapement[is.na(psc.stock.summary$escapement)] <- 0
  psc.stock.summary$cohort <- (psc.stock.summary$fishery.mortality + psc.stock.summary$escapement)
  psc.stock.summary$er <- psc.stock.summary$fishery.mortality / psc.stock.summary$cohort

  psc.stock.summary <- merge(psc.stock.summary, psc.stock, by=c("psc.stock.id", "psc.stock.order", "psc.stock.name"))

  psc.cohort.summary <- psc.stock.summary[,c("psc.stock.name", "cohort")]

  psc.fishery.mortality <- merge(psc.fishery.mortality, psc.cohort.summary, all=TRUE, by=c("psc.stock.name"))
  psc.fishery.mortality$er <- psc.fishery.mortality$fishery.mortality / psc.fishery.mortality$cohort

  if(combine.GS){
    psc.stock.summary <- combine_GS_stock_summary(stock.summary = psc.stock.summary)
    psc.fishery.mortality <- combine_GS_fishery_mortality(fishery.mortality = psc.fishery.mortality)
  }

  result <- list(fishery.mortality=psc.fishery.mortality, stock.summary=psc.stock.summary, run.info=run.info)

  return (result)
}


#' Create Table 3 within the Annual report based on PSC Fisheries & Stocks
#'
#'
#' @param post.season.data Post Season data in PSC fishery/stock format
#'
#' @importFrom dplyr mutate_at vars
#'
#' @returns The formatted Table 3 for the annual report
#'
#'
#' @export

CreateTable3 <- function(post.season.data) {

  fishery.mortality <- post.season.data$fishery.mortality
  psc.fishery.table <- BuildFisheryTable(fishery.mortality, "er")

  fishery.mort.row <- GetColumnSums(psc.fishery.table[,3:ncol(psc.fishery.table)], "psc.fishery.name", kTotalFisheryName)

  fmt.fishery.table <- SummaryRegionTotals(psc.fishery.table)
  fmt.fishery.table <- rbind(fmt.fishery.table, fishery.mort.row)

  stock.summary <- post.season.data$stock.summary

  stock.rows <- as.data.frame(t(stock.summary[order(stock.summary$psc.stock.order),c("escapement", "cohort")]))

  summary.rows <- cbind(row.names(stock.rows), stock.rows)
  names(summary.rows) <- names(fmt.fishery.table)
  fmt.fishery.table <- rbind(fmt.fishery.table, summary.rows) %>%
    mutate_at(vars(-psc.fishery.name), as.double)



  return (fmt.fishery.table)
}



#' Calculate Country Specific PSC Stock ER Cap based on stock status and the PST
#'
#'
#' @param  stock.status: Post Season data in PSC fishery/stock format
#'
#' @return The stock.status data frame with the US and Canadian ER Caps
#'
#' @export
#'

GetPstStockCountryCap <- function(stock.status) {

  stock.status$canada.cap <- kNANumber
  stock.status$us.cap <- kNANumber

  stock.status$cap <- as.numeric(stock.status$cap)

  imu.stocks <- stock.status$cap.method == kCapMethodImu
  if (any(imu.stocks)) {
    low.status <- imu.stocks & stock.status$cap <= kPstLowStatusER
    low.status.count <- sum(low.status)

    moderate.status <- imu.stocks & stock.status$status == kStatusModerateCode
    moderate.status.count <- sum(moderate.status)

    abundant.status <- imu.stocks & stock.status$status == kStatusAbundantCode & stock.status$cap <= 0.6
    over.abundant.status <- imu.stocks & stock.status$status == kStatusAbundantCode & stock.status$cap > 0.6

    if (low.status.count > 1) {
      #Normal Low US Inside MU Condition (Canadian ER Cap = 0.11)
      stock.status$canada.cap[low.status] <- 0.11
    } else if (low.status.count == 1) {
      #Composite Low US Inside MU Condition (Canadian ER Cap = 0.13)
      stock.status$canada.cap[low.status] <- 0.13
    }

    if (moderate.status.count > 1) {
      #Normal Moderate US Inside MU Condition (Canadian ER Cap = 0.124 + 0.13 * ER)
      stock.status$canada.cap[moderate.status] <- 0.124 + 0.13 * stock.status$cap[moderate.status]
    } else if (moderate.status.count == 1) {
      #Composite Moderate US Inside MU Condition (Canadian ER Cap = 0.134 + 0.13 * ER)
      stock.status$canada.cap[moderate.status] <- 0.134 + 0.13 * stock.status$cap[moderate.status]
    }

    stock.status$canada.cap[abundant.status] <- 0.084 + 0.28 * stock.status$cap[abundant.status]
    stock.status$canada.cap[over.abundant.status] <- 0.024 + 0.38 * stock.status$cap[over.abundant.status]

    stock.status$us.cap[imu.stocks] <- stock.status$cap[imu.stocks] - stock.status$canada.cap[imu.stocks]
  }

  omu.stocks <- stock.status$cap.method == kCapMethodOmu
  if (any(omu.stocks)) {
    low.status <- omu.stocks & stock.status$status == kStatusLowCode
    low.status.count <- sum(low.status)

    moderate.status <- omu.stocks & stock.status$status == kStatusModerateCode
    moderate.status.count <- sum(moderate.status)

    abundant.status <- omu.stocks & stock.status$status == kStatusAbundantCode

    if (low.status.count > 1) {
      #Normal Low US Inside MU Condition (Canadian ER Cap = 0.11)
      stock.status$canada.cap[low.status] <- 0.10
    } else if (low.status.count == 1) {
      #Composite Low US Inside MU Condition (Canadian ER Cap = 0.13)
      stock.status$canada.cap[low.status] <- 0.12
    }

    if (moderate.status.count > 1) {
      #Normal Moderate US Inside MU Condition (Canadian ER Cap = 0.124 + 0.13 * ER)
      stock.status$canada.cap[moderate.status] <- 0.024 + 0.38 * stock.status$cap[moderate.status]
    } else if (moderate.status.count == 1) {
      #Composite Moderate US Inside MU Condition (Canadian ER Cap = 0.134 + 0.13 * ER)
      stock.status$canada.cap[moderate.status] <- 0.054 + 0.33 * stock.status$cap[moderate.status]
    }

    stock.status$canada.cap[abundant.status] <- 0.024 + 0.38 * stock.status$cap[abundant.status]

    stock.status$us.cap[omu.stocks] <- stock.status$cap[omu.stocks] - stock.status$canada.cap[omu.stocks]
  }

  fixed.stocks <- stock.status$cap.method == kCapMethodFixed
  if (any(fixed.stocks)) {
    low.status <- fixed.stocks & stock.status$cap <= kPstLowStatusER
    stock.status$us.cap[low.status] <- 0.1

    moderate.status <- fixed.stocks & stock.status$cap > kPstLowStatusER & stock.status$cap <= kPstModerateStatusER
    stock.status$us.cap[moderate.status] <- 0.12

    abundant.status <- fixed.stocks & stock.status$cap > kPstModerateStatusER
    stock.status$us.cap[abundant.status] <- 0.15

    stock.status$canada.cap[fixed.stocks] <- stock.status$cap[fixed.stocks] - stock.status$us.cap[fixed.stocks]
  }

  return(stock.status)
}


#' Identifies the stock status of PSC Stocks based on the Pacific Salmon Treaty
#'
#'
#' @param stock.summary Summary of stock information for PSC Stocks
#' @param run.year The run year (used for a kludge to handle 2014 IFR Coho status)
#'
#' @return The stock.status data frame with the US and Canadian ER Caps
#'
#'
#' @export


GetPstStockStatusCap <- function(stock.summary, run.year) {

  full.data <- stock.summary[order(stock.summary$psc.stock.order),]
  full.data$status <- ""
  full.data$cap <- ""

  full.data$cap.method[is.na(full.data$cap.method)] <- ""

  imu.stocks <- full.data$cap.method == kCapMethodImu
  if (any(imu.stocks)) {
    full.data$status[imu.stocks & full.data$cohort > full.data$upper.abundance.cohort] <- kStatusAbundantCode
    full.data$status[imu.stocks & full.data$cohort <= full.data$upper.abundance.cohort] <- kStatusModerateCode
    full.data$status[imu.stocks & full.data$cohort < full.data$lower.abundance.cohort] <- kStatusLowCode

    full.data$cap[imu.stocks & full.data$status == kStatusAbundantCode] <- full.data$abundant.abundance.cap[imu.stocks & full.data$status == kStatusAbundantCode]
    full.data$cap[imu.stocks & full.data$status == kStatusModerateCode] <- full.data$moderate.abundance.cap[imu.stocks & full.data$status == kStatusModerateCode]
    full.data$cap[imu.stocks & full.data$status == kStatusLowCode] <- full.data$low.abundance.cap[imu.stocks & full.data$status == kStatusLowCode]
  }

  omu.stocks <- full.data$cap.method == kCapMethodOmu
  if (any(omu.stocks)) {
    abundant.stocks <- omu.stocks & full.data$cohort > full.data$upper.abundance.cohort
    moderate.stocks <- omu.stocks & full.data$cohort <= full.data$upper.abundance.cohort
    low.stocks <- omu.stocks & full.data$cohort < full.data$lower.abundance.cohort

    full.data$status[abundant.stocks] <- kStatusAbundantCode
    full.data$status[moderate.stocks] <- kStatusModerateCode
    full.data$status[low.stocks] <- kStatusLowCode

    #Create a vector with a value for each Outside Management Unit cap method stock

    omu.cap.er <- rep(NA, nrow(full.data))
    omu.cap.er[abundant.stocks] <- pmax(full.data$low.abundance.cap[abundant.stocks],
                                       full.data$moderate.abundance.cap[abundant.stocks],
                                       full.data$abundant.abundance.cap[abundant.stocks],
                                       na.rm=TRUE)

    omu.cap.er[moderate.stocks] <- pmax(full.data$low.abundance.cap[moderate.stocks],
                                       full.data$moderate.abundance.cap[moderate.stocks],
                                       na.rm=TRUE)

    omu.cap.er[low.stocks] <- full.data$low.abundance.cap[low.stocks]


    full.data$cap[omu.stocks] <- pmax(
      (full.data$cohort[omu.stocks] - full.data$lower.escapement.goal[omu.stocks])/full.data$cohort[omu.stocks],
      omu.cap.er[omu.stocks])
  }

  fixed.stocks <- full.data$cap.method == kCapMethodFixed
  if (any(fixed.stocks)) {
    fixed.cap <- pmax(full.data$low.abundance.cap,
                      full.data$moderate.abundance.cap,
                      full.data$abundant.abundance.cap,
                      na.rm=TRUE)


    full.data$cap[fixed.stocks] <- fixed.cap[fixed.stocks]
    full.data$status[fixed.stocks & fixed.cap == full.data$abundant.abundance.cap] <- kStatusAbundantCode
    full.data$status[fixed.stocks & fixed.cap == full.data$moderate.abundance.cap] <- kStatusModerateCode
    full.data$status[fixed.stocks & fixed.cap == full.data$low.abundance.cap] <- kStatusLowCode

    #this is a hack to get the 2014 specific identification of Moderate stock status for Interior Fraser River MU
    #  in pre-season only, this r
    if (run.year == 2014 && full.data$escapement[full.data$psc.stock.id == 11] > 40000){
      full.data$cap[full.data$psc.stock.id == 11] <- 0.4
      full.data$status[full.data$psc.stock.id == 11] <- kStatusModerateCode
    }
  }

  return(full.data[,names(full.data) %in% c("psc.stock.name", "escapement", "er", "status", "cap", "cohort", "cap.method")])

}



#' Generate the Table 2 of the PSC Annual report for the specified run year
#'
#'
#' @param pre.season.data PSC summarized data by fishery and stock from Pre-Season model run
#' @param post.season.data PSC summarized data by fishery and stock from Post-Season model run
#' @param run.year The run year that table is generated for
#'
#' @return Table 2 of the annual report as a data frame
#'
#' @export


CreateTable2 <- function(pre.season.data, post.season.data, run.year) {

  pre.status <- GetPstStockStatusCap(pre.season.data$stock.summary, run.year)

  post.status <- GetPstStockStatusCap(post.season.data$stock.summary, run.year)

  fmt.tbl <- cbind(management.unit=as.character(pre.status$psc.stock.name), pre.status=pre.status$status, pre.cap=pre.status$cap, pre.model=pre.status$er)

  fmt.tbl <- cbind(fmt.tbl, post.status=post.status$status, post.cap=post.status$cap, post.estd=post.status$er)

  fmt.tbl <- cbind(fmt.tbl, pre.escapement=pre.status$escapement, post.escapement=post.status$escapement)
  fmt.tbl <- cbind(fmt.tbl, pre.ocean.age=pre.status$cohort, post.ocean.age=post.status$cohort)

  fmt.tbl <- as.data.frame(fmt.tbl)

  no.status <- (fmt.tbl$pre.status=="")

  #Remove ecapement and ocean age 3 totals for stocks that don't have a status
  fmt.tbl[no.status,c("pre.escapement","pre.ocean.age", "post.escapement", "post.ocean.age")] <- NA

  return (fmt.tbl)
}

#' Generate the Table 1 of the PSC Annual report for the specified run year
#'
#' @param pre.season.data PSC summarized data by fishery and stock from Pre-Season model run
#' @param post.season.data PSC summarized data by fishery and stock from Post-Season model run
#' @param run.year The run year that table is generated for
#'
#' @return Table 1 of the annual report as a data frame
#'
#' @export
#'


CreateTable1 <- function(pre.season.data, post.season.data, run.year) {

  pre.fishery <- pre.season.data$fishery.mortality
  pre.stock.summary <- pre.season.data$stock.summary


  post.fishery <- post.season.data$fishery.mortality
  post.stock.summary <- post.season.data$stock.summary

  aggr.factors <- with(pre.fishery,
                         list(group.code=group.code,
                              psc.stock.name=psc.stock.name,
                              psc.stock.order=psc.stock.order))
  pre.country.er <- aggregate(data.frame(er=pre.fishery$er), aggr.factors, sum)
  pre.country.er <- pre.country.er[order(pre.country.er$psc.stock.order),]


  aggr.factors <- with(post.fishery,
                       list(group.code=group.code,
                            psc.stock.name=psc.stock.name,
                            psc.stock.order=psc.stock.order))
  post.country.er <- aggregate(data.frame(er=post.fishery$er), aggr.factors, sum)
  post.country.er <- post.country.er[order(post.country.er$psc.stock.order),]

  canada.pre.er <- pre.country.er[pre.country.er$group.code == kCanadaGroupCode, ]
  us.pre.er <- pre.country.er[pre.country.er$group.code == kUSGroupCode, ]

  canada.post.er <- post.country.er[post.country.er$group.code == kCanadaGroupCode, ]
  us.post.er <- post.country.er[post.country.er$group.code == kUSGroupCode, ]

  pre.stock.status <- GetPstStockStatusCap(pre.stock.summary, run.year)
  post.stock.status <- GetPstStockStatusCap(post.stock.summary, run.year)

  post.stock.cap <- GetPstStockCountryCap(post.stock.status)
  pre.stock.cap <- GetPstStockCountryCap(pre.stock.status)

  fmt.tbl <- cbind(management.unit=as.character(us.pre.er$psc.stock.name),
                   cap.method=pre.stock.cap$cap.method,
                   us.pre.cap=pre.stock.cap$us.cap,
                   us.pre.model=us.pre.er$er,
                   us.pre.unused=pre.stock.cap$us.cap - us.pre.er$er)

  fmt.tbl <- cbind(fmt.tbl,
                   us.post.cap=post.stock.cap$us.cap,
                   us.post.estd=us.post.er$er,
                   us.post.unused=post.stock.cap$us.cap - us.post.er$er)

  fmt.tbl <- cbind(fmt.tbl,
                   cdn.pre.cap=pre.stock.cap$canada.cap,
                   cdn.pre.model=canada.pre.er$er,
                   cdn.pre.unused=pre.stock.cap$canada.cap - canada.pre.er$er)

  fmt.tbl <- cbind(fmt.tbl,
                   cdn.post.cap=post.stock.cap$canada.cap,
                   cdn.post.estd=canada.post.er$er,
                   cdn.post.unused=post.stock.cap$canada.cap - canada.post.er$er)

  fmt.tbl <- as.data.frame(fmt.tbl,stringsAsFactors=FALSE)

  fmt.tbl$us.pre.cap <- as.numeric(fmt.tbl$us.pre.cap)
  fmt.tbl$us.post.cap <- as.numeric(fmt.tbl$us.post.cap)
  fmt.tbl$us.pre.model <- as.numeric(fmt.tbl$us.pre.model)
  fmt.tbl$us.post.estd <- as.numeric(fmt.tbl$us.post.estd)
  fmt.tbl$us.pre.unused <- as.numeric(fmt.tbl$us.pre.unused)
  fmt.tbl$us.post.unused <- as.numeric(fmt.tbl$us.post.unused)


  fmt.tbl$cdn.pre.cap <- as.numeric(fmt.tbl$cdn.pre.cap)
  fmt.tbl$cdn.post.cap <- as.numeric(fmt.tbl$cdn.post.cap)
  fmt.tbl$cdn.pre.model <- as.numeric(fmt.tbl$cdn.pre.model)
  fmt.tbl$cdn.post.estd <- as.numeric(fmt.tbl$cdn.post.estd)
  fmt.tbl$cdn.pre.unused <- as.numeric(fmt.tbl$cdn.pre.unused)
  fmt.tbl$cdn.post.unused <- as.numeric(fmt.tbl$cdn.post.unused)

  cdn.pre.used <- fmt.tbl$cap.method %in% kUSCapMethods & fmt.tbl$cdn.pre.unused > 0
  fmt.tbl$us.pre.cap[cdn.pre.used] <- fmt.tbl$us.pre.cap[cdn.pre.used] + fmt.tbl$cdn.pre.unused[cdn.pre.used]
  fmt.tbl$us.pre.unused[cdn.pre.used] <- fmt.tbl$us.pre.cap[cdn.pre.used] - fmt.tbl$us.pre.model[cdn.pre.used]

  cdn.post.used <- fmt.tbl$cap.method %in% kUSCapMethods & fmt.tbl$cdn.post.unused > 0
  fmt.tbl$us.post.cap[cdn.pre.used] <- fmt.tbl$us.post.cap[cdn.post.used] + fmt.tbl$cdn.post.unused[cdn.post.used]
  fmt.tbl$us.post.unused[cdn.pre.used] <- fmt.tbl$us.post.cap[cdn.post.used] - fmt.tbl$us.post.estd[cdn.post.used]

  us.pre.used <- fmt.tbl$cap.method %in% kCdnCapMethods & fmt.tbl$us.pre.unused > 0
  fmt.tbl$cdn.pre.cap[us.pre.used] <- fmt.tbl$cdn.pre.cap[us.pre.used] + fmt.tbl$us.pre.unused[us.pre.used]
  fmt.tbl$cdn.pre.unused[us.pre.used] <- fmt.tbl$cdn.pre.cap[us.pre.used] - fmt.tbl$cdn.pre.model[us.pre.used]

  us.post.used <- fmt.tbl$cap.method %in% kCdnCapMethods & fmt.tbl$us.post.unused > 0
  fmt.tbl$cdn.post.cap[us.post.used] <- fmt.tbl$cdn.post.cap[us.post.used] + fmt.tbl$us.post.unused[us.post.used]
  fmt.tbl$cdn.post.unused[us.post.used] <- fmt.tbl$cdn.post.cap[us.post.used] - fmt.tbl$cdn.post.estd[us.post.used]

  return(fmt.tbl)
}



#' Checks run data retrieved from the FRAM database.
#'
#' @param run.info List of model run data retreived from FRAM database
#' @param run.year The run year that the run.info should represent
#'
#' @export



ValidateRunInfo <- function (run.info, run.year) {

  total.runs <- nrow(run.info)

  if (total.runs != 1) {
    if (total.runs > 1) {
      stop(sprintf("Too many runs (%d) found with the run name: %s", total.runs, run.info$run.name))
    } else if (total.runs == 0) {
      stop(sprintf("The run name %s is not in the database.", run.info$run.name))
    }
  }

  if (is.na(run.info$run.year)) {
    cat(sprintf("WARNING - Run name %s does not have run year set so can not validate against configured run year %d\n", run.info$run.name, run.year))
  } else if (run.info$run.year != run.year) {
    stop(sprintf("The run year in FRAM (%d) does not match the configured year (%d) for run name %s", run.info$run.year, run.year, run.info$run.name))
  }

  cat(sprintf("Run name was found: %s\n", run.info$run.name))
}


#' Combines the GS MUs for the stock.summary DF (pre or post season)
#'
#' @param stock.summary stock.summary from the pre or post season list
#' @param run.year inlcude.old.GS.MUs logical indicating if the ML and VI GS MUs should still be in the table
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter bind_rows
#'
#'
#' @export


combine_GS_stock_summary <- function(stock.summary){

  gs <- filter(stock.summary, str_detect(psc.stock.name, "^Georgia"))

  gs.combined <- tibble(
    psc.stock.id = first(gs$psc.stock.id),
    psc.stock.order = first(gs$psc.stock.order),
    psc.stock.name = "Georgia Strait",
    run.id = first(gs$run.id),
    run.year = first(gs$run.year),
    escapement = sum(gs$escapement),
    fishery.mortality = sum(gs$fishery.mortality),
    cohort = sum(gs$cohort),
    er = fishery.mortality/cohort
  )


    stock.summary.combined <- filter(stock.summary, !str_detect(psc.stock.name, "^Georgia")) %>%
      bind_rows(gs.combined)

    return(stock.summary.combined)


}


#' Combines the GS MUs for the fishery.mortality DF (pre or post season)
#'
#' @param stock.summary stock.summary from the pre or post season list
#' @param run.year inlcude.old.GS.MUs logical indicating if the ML and VI GS MUs should still be in the table
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter bind_rows
#'
#'
#' @export


combine_GS_fishery_mortality <- function(fishery.mortality){

  gfm <- fishery.mortality %>%
    mutate(psc.stock.name =
             if_else(psc.stock.name %in% c("Georgia Strait ML", "Georgia Strait VI"), "Georgia Strait", psc.stock.name)) %>%
    group_by(psc.stock.name, run.id, run.year, psc.fishery.name) %>%
    summarise(group.code =  first(group.code),
              psc.fishery.order = first(psc.fishery.order),
              psc.stock.order = first(psc.stock.order),
              fishery.mortality = sum(fishery.mortality),
              cohort = sum(cohort)
    ) %>%
    ungroup()

  cohort <- gfm %>%
    group_by(psc.stock.name) %>%
    summarise(
      cohort = max(cohort))

  fishery.mortality.combined <- gfm %>%
    select(-cohort) %>%
    left_join(cohort, by = "psc.stock.name") %>%
    mutate(er = fishery.mortality/cohort)

  return(fishery.mortality.combined)

}


