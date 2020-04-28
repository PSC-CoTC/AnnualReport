
#' Get the path to a file within an R package
#'
#' @param file_name File name to load from the R package
#' @param params List of Parameter names use to replace variables in the file using str_interp (optional)
#' @param dir The schema name where the data is stored (e.g MRP_OPS)
#' @param clean_sql Should the format of SQL text be cleaned up (e.g. replace newline characters with a space)
#' @param package_name Package name that the file is located in.
#'
#' @return Text from the file name provided
#'
#' @importFrom utils packageName
#' @importFrom fs path
#' @importFrom stringr str_replace_all str_interp
#'
#'
#' @export
#'
formatPkgTextFile <- function(file_name, params = NULL, dir, clean_sql = TRUE, package_name = packageName(parent.frame())) {
  text_file_path <- system.file(dir, file_name, package = package_name)
  if (file.exists(text_file_path) == FALSE) {
    cat("ERROR - The file '{path(dir, file_name)}' is missing from the {package_name} package.")
    stop()
  }
  text <- readChar(text_file_path, file.info(text_file_path)$size)

  if (clean_sql) {
    text <- str_replace_all(text, "\r\n", " ")
  }

  if (!is.null(params)) {
    text <- str_interp(text, params)
  }
  return(text)
}

#' Retreives Sql Statement text from a file, binds parameters to the text, and  do basic format cleanup
#'
#' @param sql_file_name The name of of the file with sql to load
#' @param params List of named parameters to bind to the sql text
#' @param sql_dir The directory that stores all the sql statements (defaults to "sql")
#' @param clean_sql A boolean that identifies if the format of the final sql should be cleaned (e.g. strip newlines)
#' @param package_name The package name to load sql file from, defaults to the current package name
#'
#' @return An Sql text statement
#'
#' @importFrom stringr str_interp str_replace_all str_glue
#'
#' @export
#'
formatSql <- function(sql_file_name, params = NULL, sql_dir = "sql", clean_sql = TRUE, package_name = packageName(parent.frame())) {
  sql <- formatPkgTextFile(sql_file_name, params, sql_dir, clean_sql, package_name = package_name)
  return(sql)
}


#' Retreives HTML text, binds parameters to the text
#'
#' @param html_file_name The name of of the file with HTML to load
#' @param params List of named parameters to bind to the HTML text
#' @param html_dir The directory that stores all the HTML tempalte documents (defaults to "html")
#' @param package_name The package name to load sql file from, defaults to the current package name
#'
#' @return An html text statement
#'
#' @importFrom stringr str_interp str_replace_all str_glue
#'
#' @export
#'
formatHtml <- function(html_file_name, params = NULL, html_dir = "html_templates", package_name = packageName(parent.frame())) {
  html <- formatPkgTextFile(html_file_name, params, html_dir, FALSE, package_name = package_name)
  return(html)
}
