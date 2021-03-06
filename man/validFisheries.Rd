% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importPostSeasonTemplates.R
\name{validFisheries}
\alias{validFisheries}
\title{Validates the fishery definitions for parametrization of the FRAM model.
This function checks that all the fisheries are valid, relative to the base period
and that all the fisheries identified are the responsibility of the identified person.}
\usage{
validFisheries(
  person.name,
  fram.db.conn,
  fram.run.name,
  fishery.scalars,
  data.dir
)
}
\arguments{
\item{person.name}{the name of the person for the import file}

\item{fram.db.conn}{FRAM ODBC database connection}

\item{fram.run.name}{FRAM run name from the import file}

\item{fishery.scalars}{fishery catch data provided from the import file}
}
\value{
A boolean, TRUE for valid or FALSE for when there is issues with the catch
}
\description{
Validates the fishery definitions for parametrization of the FRAM model.
This function checks that all the fisheries are valid, relative to the base period
and that all the fisheries identified are the responsibility of the identified person.
}
