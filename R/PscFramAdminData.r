################
#
# Common methods and constants for dealing with a FRAM Database
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# April 26, 2017
#
################

#' Retrieve a data frame with the relationships between CoTC members and FRAM fisheries
#'
#' @param select_person_name Provide a person name to filter results or NA if all person fisheries
#' @param data_dir The directory that the file with the person fishery definition file is in
#'
#' @return A data frame identifying who is resposible for each FRAM fishery catch reporting
#'
#' @export
#'
getPersonFramFisheries <- function(select_person_name = NA, data_dir = data.dir) {
  person_fishery <- ReadCsv("PersonFramFisheries.csv", data_dir, unique.col.names=c("fram.fishery.id"))

  if (!is.na(select_person_name)) {
    person_fishery <- filter(person_fishery, person.name == select_person_name)
  }

  return (person_fishery)
}

#' Retrieve a data frame with the relationships between CoTC members and FRAM stocks
#'
#' @param select_person_name Provide a person name to filter results or NA if all person stocks
#'
#' @return A data frame identifying who is resposible for each FRAM fishery catch reporting
#' @export
#'
getPersonFramStocks <- function(select_person_name = NA, data_dir = data.dir) {
  person_stock <- ReadCsv("PersonFramStocks.csv", data_dir, unique.col.names=c("fram.stock.id"))

  if (!is.na(select_person_name)) {
    person_stock <- filter(person_stock, person.name == select_person_name)
  }

  return (person_stock)
}
