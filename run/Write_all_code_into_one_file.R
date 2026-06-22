# Main file path
main_file <- "run/main.R"

# Read main.R
main_lines <- readLines(main_file)

# Extract lines with source()
source_lines <- grep('source\\(', main_lines, value = TRUE)

# Extract file paths inside source("...")
extract_path <- function(x) {
  sub('.*source\\(["\']([^"\']+)["\']\\).*', '\\1', x)
}

source_files <- sapply(source_lines, extract_path)

# Include main.R at the beginning
all_files <- c(main_file, source_files)

# Output file
output_file <- "ALLCODE.R"

# Initialize output file
cat("", file = output_file)

# Loop through all files
for (f in all_files) {
  if (file.exists(f)) {
    cat(paste0("\n----- ", f, " -----\n"), file = output_file, append = TRUE)
    
    lines <- readLines(f)
    cat(paste(lines, collapse = "\n"), file = output_file, append = TRUE)
    
    cat("\n", file = output_file, append = TRUE)
  } else {
    cat(paste0("\n----- ", f, " (NOT FOUND) -----\n"),
        file = output_file, append = TRUE)
  }
}

cat("Done! Combined file saved as:", output_file, "\n")