# Functions for running and timing scripts, error handling

timeDiffString <- function(time_diff) {
  time_diff_seconds <- as.numeric(time_diff, units = "secs")
  hours <- floor(time_diff_seconds / 3600)
  minutes <- floor((time_diff_seconds %% 3600) / 60)
  seconds <- time_diff_seconds %% 60
  return(paste0(
    hours,
    " tuntia, ",
    minutes, "
    minuuttia ja ",
    seconds,
    " sekuntia"
  ))
}

# Funktio suorittaa annetun scriptin
runScript <- function(script, env = TRUE) {
  cat("\n\n\t\t", paste(rep(".", nchar(script) + 21), collapse = ""))
  cat("\n\t\t ::: SUORITETAAN", script, " ::: ")
  cat("\n\t\t", paste(rep("'", nchar(script) + 21), collapse = ""))
  script_start <- Sys.time()
  
  tryCatch({
    suppressWarnings(source(script, local = env))
    message(paste0("\n\t\tAika ", timeDiffString(Sys.time() - script_start)))
  }, warning = function(w) {
    message("Warning message:", w)
  }, error = function(e) {
    stop(e)
  })
}

printTotalTime <- function(start_time) {
  message(paste0(
    "\n\t\tSuoritusaika ",
    timeDiffString(Sys.time() - start_time)
  ))
}