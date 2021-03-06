% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PscFramAdminData.r
\name{getPersonFramStocks}
\alias{getPersonFramStocks}
\title{Retrieve a data frame with the relationships between CoTC members and FRAM stocks}
\usage{
getPersonFramStocks(select_person_name = NA, data_dir = data.dir)
}
\arguments{
\item{select_person_name}{Provide a person name to filter results or NA if all person stocks}
}
\value{
A data frame identifying who is resposible for each FRAM fishery catch reporting
}
\description{
Retrieve a data frame with the relationships between CoTC members and FRAM stocks
}
