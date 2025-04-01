#scraping der YT Kommentare in .txt files
install.packages("httr")
install.packages("dplyr")
install.packages("readr")

library(httr)
library(dplyr)
library(readr)

#Nach Bedarf anpassen
playlist_id <- "kurierte Playlist"

api_key <- "API Key"

output_directory <- "C:/Users/Felix/Desktop/YTC Downloads/CNN2"
#============================================================================================================
#API stuff

fetch_video_info <- function(video_id, api_key) {

  url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=snippet&id=", video_id, "&key=", api_key)


  response <- GET(url)

  content <- content(response, "parsed")

  if (length(content$items) > 0) {
    title <- content$items[[1]]$snippet$title
    channel_title <- content$items[[1]]$snippet$channelTitle
    return(list(title = title, channelTitle = channel_title))
  } else {
    return(list(title = NA, channelTitle = NA))
  }
}




if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
  cat("Created output directory:", output_directory, "\n")
} else {
  cat("Using existing output directory:", output_directory, "\n")
}



video_ids <- fetch_playlist_video_ids(playlist_id, api_key)


for (video_id in video_ids) {

  video_info <- fetch_video_info(video_id, api_key)
  video_title <- video_info$title
  channel_name <- video_info$channelTitle


  youtube_comments <- scrape_youtube_comments(video_id, api_key)


  cat("Structure of youtube_comments for video", video_id, ":\n")
  str(youtube_comments)


  safe_video_title <- gsub("[^a-zA-Z0-9_]", "_", video_title)  # Replace special characters with underscores
  safe_channel_name <- gsub("[^a-zA-Z0-9_]", "_", channel_name)  # Replace special characters with underscores
  output_file_name <- paste0("yc_", safe_channel_name, "_", safe_video_title, ".txt")  # Include channel name and video title


  output_file_path <- file.path(output_directory, output_file_name)

  if (nrow(youtube_comments) > 0) {
    #Extracts the comment content (use the correct column name: CommentContent)
    comment_content <- youtube_comments$CommentContent

    cat("First few comments for video", video_id, ":\n")
    print(head(comment_content))

    #Saves the comments as a .txt file, separated by tabs
    write.table(comment_content, file = output_file_path, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    cat("Comments for video", video_id, "saved to:", output_file_path, "\n")
  } else {
    cat("No comments found for video", video_id, "\n")
  }

  Sys.sleep(1)  # delay to avoid hitting API quota limits
}

cat("All videos processed and comments saved.\n")
