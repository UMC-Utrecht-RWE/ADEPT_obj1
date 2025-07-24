#' Find Matching Row by Variable Name
#'
#' @param varname Character. The variable name to search for.
#' @param dt data.table. Table with a `Varname` column.
#' @param code_col Optional character. Column name containing comma-separated codes (e.g., ATC, ICD).
#' @return A single-row data.table or NULL if not uniquely matched.
find_matching_row <- function(varname, dt, code_col = NULL) {
  exact_row <- dt[Varname == varname]
  if (nrow(exact_row) == 1) return(exact_row)
  
  if (!is.null(code_col) && code_col %in% names(dt)) {
    dt[, code_list := strsplit(get(code_col), ",\\s*")]
    row_idx <- dt[sapply(code_list, function(codes) varname %in% codes), which = TRUE]
    dt[, code_list := NULL]  # clean up
    if (length(row_idx) == 1) return(dt[row_idx])
  }
  
  return(NULL)
}
