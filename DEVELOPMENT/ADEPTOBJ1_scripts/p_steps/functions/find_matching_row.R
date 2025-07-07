#' Find Matching Row by Variable Name in ATC Concept Sets
#'
#' Given a variable name and a data.table of ATC concept sets, this function searches for
#' a matching row either by exact match on the `Varname` column or by presence in the
#' comma-separated `ATC codes` column.
#'
#' @param varname Character. The variable name to search for.
#' @param dt data.table. The data.table containing ATC concept set rows, expected to have columns:
#'   - `Varname`: variable names
#'   - `ATC codes`: comma-separated ATC code strings
#'
#' @return A single-row data.table matching the `varname` either by exact match or by
#'   presence in the `ATC codes` list. Returns `NULL` if no unique match is found.

find_matching_row <- function(varname, dt) {
  # Exact match on Varname
  exact_row <- dt[Varname == varname]
  
  if (nrow(exact_row) == 1) return(exact_row)
  
  # If no exact match, try to find variable name inside comma-separated ATC codes column (partial matching with trimming spaces)
  # Split each row's ATC codes by comma, trim spaces, then check if variable name is in any
  dt[, ATC_codes_list := strsplit(`ATC codes`, ",\\s*")]
  
  row_idx <- dt[sapply(ATC_codes_list, function(codes) varname %in% codes), which = TRUE]
  
  if (length(row_idx) == 1) return(dt[row_idx])
  
  # No or multiple matches:
  return(NULL)
}


