folder_path <- "C:/Users/Felix/Desktop/YTC cleantxt/CNN2"

# Get list of text files in the folder
file_list <- list.files(path = folder_path,
                        pattern = "\\.txt$",
                        full.names = TRUE,
                        ignore.case = TRUE)

# Check if any files were found
if (length(file_list) == 0) {
  stop("No .txt files found in the specified directory.")
}

# Initialize an empty data frame
text_data_CNN <- data.frame(
  file_name = character(),
  file_content = character(),
  stringsAsFactors = FALSE
)

# Function to read file content safely
read_file_safely <- function(file_path) {
  tryCatch({
    content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    paste(content, collapse = " ")
  }, error = function(e) {
    warning(paste("Error reading file:", file_path, "-", e$message))
    return(NA)
  })
}

# Process each file
for (file_path in file_list) {
  # Get just the file name without path
  file_name <- basename(file_path)

  # Read file content
  file_content <- read_file_safely(file_path)

  # Add to data frame (using the same data frame name)
  text_data_CNN <- rbind(text_data_CNN,
                         data.frame(file_name = file_name,
                                    file_content = file_content,
                                    stringsAsFactors = FALSE))
}

# Remove any rows with NA content (files that couldn't be read)
text_data_CNN <- na.omit(text_data_CNN)

# View the first few rows of the data frame
head(text_data_CNN)

# Check the number of rows
nrow(text_data_CNN)
