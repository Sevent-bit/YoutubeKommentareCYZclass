# vader sentiment analyse der .txt files
install.packages(c("vader", "dplyr", "readr", "purrr", "stringr"))



library(vader)
library(tidyverse)
library(readr)


directory_path <- "C:/Users/Felix/Desktop/Finale Daten/YTC cleantxt/BBC"


txt_files <- list.files(path = directory_path,
                        pattern = "\\.txt$",
                        full.names = TRUE)


all_results <- data.frame()

# Process files one by one with progress feedback
for (i in seq_along(txt_files)) {
  file_path <- txt_files[i]
  file_name <- basename(file_path)

  cat("\nProcessing file", i, "of", length(txt_files), "-", file_name, "\n")

  tryCatch({

    text_content <- read_file(file_path)


    sentiment_scores <- get_vader(text_content)

    result <- data.frame(
      file_name = file_name,
      text_length = nchar(text_content),
      compound_score = sentiment_scores["compound"],
      positive_score = sentiment_scores["pos"],
      negative_score = sentiment_scores["neg"],
      neutral_score = sentiment_scores["neu"],
      stringsAsFactors = FALSE
    )

    # Append to results
    all_results <- bind_rows(all_results, result)


    cat("Successfully processed. Compound score:", result$compound_score, "\n")

  }, error = function(e) {

    cat("Error processing file:", conditionMessage(e), "\n")


    error_result <- data.frame(
      file_name = file_name,
      text_length = NA,
      compound_score = NA,
      positive_score = NA,
      negative_score = NA,
      neutral_score = NA,
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )

    # Append to results
    all_results <<- bind_rows(all_results, error_result)
  })
}


print(head(all_results))


output_file <- "C:/Users/Felix/Desktop/YTC Sentiment/sentiment_analysis_results.csv"
write_csv(all_results, output_file)

