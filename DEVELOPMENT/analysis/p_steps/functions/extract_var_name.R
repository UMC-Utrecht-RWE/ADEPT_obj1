#' Extract Variable Name from Filename
#'
#' Extracts the variable name from an RDS filename that starts with a known population prefix.
#' The filename is expected to follow the pattern: [pop_prefix]_[varname].rds
#'
#' @param filepath Character. Full or relative path to the RDS file.
#' @return Character string containing the extracted variable name.

extract_varname <- function(filepath) {
  fname <- basename(filepath)
  sub(paste0("^", pop_prefix, "_(.*)\\.rds$"), "\\1", fname)
}
