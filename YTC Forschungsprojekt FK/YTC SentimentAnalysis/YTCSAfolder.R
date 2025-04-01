install.packages(c("vader", "dplyr", "readr", "purrr", "stringr"))


library(vader)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# Specify the directory containing the CSV files
directory_path <- "C:/Users/Felix/Desktop/YT Comments"

# Specify the output directory
output_directory <- "C:/Users/Felix/Desktop/YTC Sentiment"

# Create the output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Get a list of all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Function to clean text
clean_text <- function(text) {
  if (is.na(text)) {
    return("")  # Return empty string for NA values
  }
  text <- iconv(text, to = "UTF-8", sub = "byte")  # Convert to UTF-8
  text <- str_replace_all(text, "<.*?>", "")  # Remove HTML tags
  text <- str_replace_all(text, "[^[:alnum:][:space:]]", "")  # Remove special characters
  text <- str_squish(text)  # Remove extra spaces
  return(text)
}

# Function to process each CSV file
process_csv <- function(file_path, output_dir) {
  tryCatch({
    # Debug: Print the file being processed
    cat("Processing file:", file_path, "\n")

    # Read the CSV file
    data <- read_csv(file_path, locale = locale(encoding = "UTF-8"))

    # Debug: Print the first few rows of the data
    cat("First few rows of the data:\n")
    print(head(data))

    # Check if the file is empty
    if (nrow(data) == 0) {
      cat("Skipping file:", file_path, "- File is empty.\n")
      return()
    }

    # Check if the required column exists
    if (!"CommentContent" %in% colnames(data)) {
      cat("Skipping file:", file_path, "- 'CommentContent' column not found.\n")
      return()
    }

    # Clean the "CommentContent" column
    data$CommentContent <- sapply(data$CommentContent, clean_text)

    # Debug: Print a few cleaned comments
    cat("First few cleaned comments:\n")
    print(head(data$CommentContent))

    # Apply VADER sentiment analysis to each comment
    vader_scores <- map(data$CommentContent, ~ {
      tryCatch({
        scores <- get_vader(.x)
        # Debug: Print the comment and its sentiment scores
        cat("Comment:", .x, "\n")
        cat("Sentiment Scores:", scores, "\n")
        return(scores)
      }, error = function(e) {
        cat("Error processing comment in file:", file_path, "-", .x, "\n")
        cat("Error message:", e$message, "\n")
        return(list(compound = NA, pos = NA, neu = NA, neg = NA))  # Return NA scores for problematic comments
      })
    })

    # Convert the list of scores to a data frame
    vader_scores_df <- do.call(rbind, vader_scores)

    # Combine the original data with the VADER scores
    combined_data <- bind_cols(data, vader_scores_df)

    # Generate the output file path
    file_name <- basename(file_path)  # Extract the file name
    output_file_path <- file.path(output_dir, sub(".csv", "_vader_cleaned.csv", file_name))

    # Write the combined data to the new CSV file
    write_csv(combined_data, output_file_path)

    cat("Processed:", file_path, "\n")
    cat("Saved to:", output_file_path, "\n")

    # Explicitly clear large objects from memory
    rm(data, vader_scores, vader_scores_df, combined_data)
    gc(full = TRUE)  # Force garbage collection to free up memory
  }, error = function(e) {
    cat("Error processing file:", file_path, "\n")
    cat("Error message:", e$message, "\n")
  })
}

# Process files in smaller batches to avoid memory issues
for (i in seq(1, length(csv_files), by = 2)) {  # Process 2 files at a time
  batch <- csv_files[i:min(i + 1, length(csv_files))]
  for (file in batch) {
    process_csv(file, output_directory)
  }
  gc(full = TRUE)  # Force garbage collection after each batch
}

cat("All files processed and saved with VADER scores.\n")
