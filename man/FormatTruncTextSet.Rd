% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Util.r
\name{FormatTruncTextSet}
\alias{FormatTruncTextSet}
\title{Formats a vector of strings to a single text block.
If more than "show_total" lines, the text block is trucated with text
identifying that there is more not shown.}
\usage{
FormatTruncTextSet(text_lines, show_total = 5, sep = "\\n")
}
\arguments{
\item{text_lines}{A vector of text lines to format}

\item{show_total}{How many lines of text_lines to show in the output}

\item{sep}{Character used to seperate the lines}
}
\value{
A single text block.
}
\description{
Formats a vector of strings to a single text block.
If more than "show_total" lines, the text block is trucated with text
identifying that there is more not shown.
}
