# Defines functions for starting and stopping logging 

# ======================
#     Start Logging 
# ======================
start_logging <- function(log_file) {
  
  # Open log file for appending 
  log_con <- file(log_file, open = "a")
  
  # returns current time stamp
  ts <- function() format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  
  # Save original base functions and log connection
  assign(".orig_cat",     base::cat,     envir = .GlobalEnv)
  assign(".orig_print",   base::print,   envir = .GlobalEnv)
  assign(".orig_message", base::message, envir = .GlobalEnv)
  assign(".orig_warning", base::warning, envir = .GlobalEnv)
  assign(".log_con",      log_con,       envir = .GlobalEnv)
  
  # Turns output to be shown in console into one line of text
  # Adds a time stamp to it
  # Writes that line into text file
  # Also shows text in console so you can still see it
  
  # Override cat
  cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
    text <- paste(..., sep = sep)
    .orig_cat(paste(ts(), text, sep = " "), "\n", file = log_con, append = TRUE)
    .orig_cat(text, "\n")
  }
  
  # Override print
  print <- function(x, ...) {
    out <- capture.output(.orig_print(x, ...))
    for (line in out) {
      .orig_cat(ts(), line, "\n", file = log_con, append = TRUE)
      .orig_cat(line, "\n")
    }
    invisible(x)
  }
  
  # Override message
  message <- function(...) {
    text <- paste(...)
    .orig_cat(ts(), "MESSAGE:", text, "\n", file = log_con, append = TRUE)
    .orig_message(text)
  }
  
  # Override warning
  warning <- function(...) {
    text <- paste(...)
    .orig_cat(ts(), "WARNING:", text, "\n", file = log_con, append = TRUE)
    .orig_warning(text, call. = FALSE)
  }
  
  # Activate overrides
  assign("cat", cat, envir = .GlobalEnv)
  assign("print", print, envir = .GlobalEnv)
  assign("message", message, envir = .GlobalEnv)
  assign("warning", warning, envir = .GlobalEnv)
}

# ======================
#     Stop Logging 
# ======================

stop_logging <- function() {
  
  # If log file connection exists in global environment, close it to release the file.
  if (exists(".log_con", envir = .GlobalEnv)) {
    close(get(".log_con", envir = .GlobalEnv))
  }
  
  # Restore originals
  for (fn in c("cat", "print", "message", "warning")) {
    orig <- paste0(".orig_", fn)
    if (exists(orig, envir = .GlobalEnv)) {
      assign(fn, get(orig, envir = .GlobalEnv), envir = .GlobalEnv)
      rm(list = orig, envir = .GlobalEnv)
    }
  }
  
  if (exists(".log_con", envir = .GlobalEnv)) {
    rm(".log_con", envir = .GlobalEnv)
  }
}
