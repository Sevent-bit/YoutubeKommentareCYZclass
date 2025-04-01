# Text Processing Script with Error Handling
# Loads text files, cleans, tokenizes, removes stopwords, and saves results

# 1. Install and load required packages with verification
required_packages <- c("tm", "tokenizers", "dplyr", "stringi", "textstem")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Explicitly load packages with verification
if (!require("tm", quietly = TRUE)) stop("tm package not installed")
if (!require("tokenizers", quietly = TRUE)) stop("tokenizers package not installed")
if (!require("dplyr", quietly = TRUE)) stop("dplyr package not installed")
if (!require("stringi", quietly = TRUE)) stop("stringi package not installed")
if (!require("textstem", quietly = TRUE)) stop("textstem package not installed")

# 2. Set your paths (using forward slashes for compatibility)
input_path <- "C:/Users/Felix/Desktop/YTC Downloads/CNN2"
output_path <- "C:/Users/Felix/Desktop/YTC cleantxt/CNN2"

# 3. UTF-8 cleaning function
clean_utf8 <- function(text) {
  # Force UTF-8 encoding
  text <- enc2utf8(text)

  # Remove invalid UTF-8 sequences
  text <- iconv(text, "UTF-8", "UTF-8", sub = " ")

  # Remove control characters
  text <- gsub("[[:cntrl:]]", " ", text)

  # Remove high-ascii characters
  text <- gsub("[^\x20-\x7E]", " ", text)

  # Normalize whitespace
  text <- gsub("\\s+", " ", trimws(text))

  return(text)
}

# 4. File processing function with explicit stopwords loading
process_text_file <- function(file_path) {
  tryCatch({
    # Read with UTF-8 encoding
    text_content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    text_content <- paste(text_content, collapse = " ")
    text_content <- clean_utf8(text_content)

    # Tokenize
    tokens <- tryCatch({
      tokenizers::tokenize_words(text_content,
                                 lowercase = TRUE,
                                 strip_punct = TRUE,
                                 strip_numeric = TRUE)
    }, error = function(e) {
      message("Tokenization error in file: ", file_path)
      return(character(0))
    })

    tokens <- unlist(tokens)

    tokens <- textstem::lemmatize_words(tokens)

    # Load stopwords explicitly
    stopwords_list <- tm::stopwords("en")
    tokens <- tokens[!tokens %in% stopwords_list]

    paste(tokens, collapse = " ")
  }, error = function(e) {
    message("Error processing file: ", file_path, " - ", e$message)
    return("")
  })
}

# 5. Save function
save_processed_text <- function(text, output_path, filename) {
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_path, paste0("processed_", filename, ".txt"))
  writeLines(text, output_file, useBytes = TRUE)
  return(output_file)
}

# 6. Main processing function
process_text_files <- function(input_path, output_path) {
  file_list <- list.files(path = input_path, pattern = "\\.txt$",
                          full.names = TRUE, ignore.case = TRUE)

  if (length(file_list) == 0) {
    stop("No .txt files found in: ", input_path)
  }

  results <- data.frame(
    original_file = character(),
    processed_file = character(),
    word_count = integer(),
    stringsAsFactors = FALSE
  )

  for (file_path in file_list) {
    file_name <- tools::file_path_sans_ext(basename(file_path))
    processed_text <- process_text_file(file_path)

    if (nchar(processed_text) > 0) {
      saved_path <- save_processed_text(processed_text, output_path, file_name)
      word_count <- length(unlist(strsplit(processed_text, " ")))

      results <- rbind(results, data.frame(
        original_file = file_path,
        processed_file = saved_path,
        word_count = word_count,
        stringsAsFactors = FALSE
      ))

      message(sprintf("Successfully processed: %s (%d words)",
                      file_name, word_count))
    }
  }

  return(results)
}

# 7. Execute the processing
tryCatch({
  cat("Starting processing...\n")
  processing_results <- process_text_files(input_path, output_path)

  # Print summary
  cat("\nProcessing complete!\n")
  cat("Files successfully processed:", nrow(processing_results), "\n")
  cat("Total words processed:", sum(processing_results$word_count), "\n")

  # Save log
  log_file <- file.path(output_path, "processing_log.csv")
  write.csv(processing_results, log_file, row.names = FALSE)
  cat("Log saved to:", log_file, "\n")
}, error = function(e) {
  cat("\nFATAL ERROR:", e$message, "\n")
})

# 8. Verify stopwords are loaded
cat("\nStopwords verification:\n")
cat("First 10 English stopwords:", head(tm::stopwords("en"), 10), "\n")
