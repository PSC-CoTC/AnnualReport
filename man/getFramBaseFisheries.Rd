% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FramDB.R
\name{getFramBaseFisheries}
\alias{getFramBaseFisheries}
\title{Get the dataframe of valid fisheries and time steps from the base period of a specific model run}
\usage{
getFramBaseFisheries(fram.db.conn, fram.run.name)
}
\arguments{
\item{fram.db.conn}{An odbc connection to the FRAM database}

\item{fram.run.name}{The name of the model run you would like to retrieve fisheries and timesteps from}
}
\value{
A dataframe with the fishery scalars for a specific model run name
}
\description{
Get the dataframe of valid fisheries and time steps from the base period of a specific model run
}
