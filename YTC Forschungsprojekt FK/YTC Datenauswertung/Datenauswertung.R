#whole script can be run, certain objects might need to be manually changed

if (!require("readtext")) install.packages("readtext")
if (!require("quanteda")) install.packages("quanteda")
if (!require("stringr")) install.packages("stringr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("quanteda.textplots")) install.packages("quanteda.textplots")
if (!require("quanteda.textstats")) install.packages("quanteda.textstats")
if (!require("quanteda.textmodels")) install.packages("quanteda.textmodels")
if (!require("BTM")) install.packages("BTM")
if (!require("tidytext")) install.packages("tidytext")
if (!require("tibble")) install.packages("tibble")
if(!require("igraph")) install.packages("igraph")
if (!require("ggraph")) install.packages("ggraph")
if (!require("vader")) install.packages("vader")

library(readtext)
library(quanteda)
library(quanteda.textplots)
library(stringr)
library(ggplot2)
library(quanteda.textstats)
library(tidyr)
library(dplyr)
library(quanteda.textmodels)
library(BTM)
library(tidytext)
library(tibble)
library(igraph)
library(ggraph)
library(vader)


#load and clean the scraped txt data

text_folder <- "C:/Users/Felix/Desktop/Finale Daten/YTC cleantxt/ALL"

text_data <- readtext(
  list.files(path = text_folder,
             pattern = "\\.txt$",
             full.names = TRUE),
  encoding = "UTF-8"
)


if (nrow(text_data) == 0) {
  stop("No text files loaded. Please check:\n",
       "1. Path exists: ", file.exists(text_folder), "\n",
       "2. Files found: ", length(list.files(path = text_folder, pattern = "\\.txt$")))
}


corpus_data <- corpus(text_data)

ALL_tokens_data <- tokens(
  corpus_data,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(
    pattern = c("<.*?>", "&[a-z]+;"),
    valuetype = "regex",
    case_insensitive = TRUE
  ) %>%
  tokens_remove("^[[:alpha:]]{1}$", valuetype = "regex")

# create Objects for different data sets

ALL_dfm <- dfm(ALL_tokens_data)

ALL_fcm <- fcm(ALL_dfm)

ALL_top_terms <- names(topfeatures(ALL_dfm, 100))

ALL_top_terms_fcm <- fcm_select(ALL_fcm, pattern = ALL_top_terms)

ALL_top_tokens <- tokens(ALL_top_terms)

ALL_top_dfm <- dfm(ALL_top_tokens)




war_dfm <- dfm_subset(ALL_dfm, grepl("_war_", docnames(ALL_dfm), ignore.case = TRUE))

corona_dfm <- dfm_subset(ALL_dfm, grepl("_corona_", docnames(ALL_dfm), ignore.case = TRUE))

election_dfm <- dfm_subset(ALL_dfm, grepl("_election_", docnames(ALL_dfm), ignore.case = TRUE))


BBC_dfm <- dfm_subset(ALL_dfm, grepl("^BBC_", docnames(ALL_dfm)))

CNN_dfm <- dfm_subset(ALL_dfm, grepl("^CNN_", docnames(ALL_dfm)))

FOX_dfm <- dfm_subset(ALL_dfm, grepl("^FOX_", docnames(ALL_dfm)))



corona_data <- ALL_tokens_data[grep("Corona", names(ALL_tokens_data))]
election_data <- ALL_tokens_data[grep("Election", names(ALL_tokens_data))]
war_data <- ALL_tokens_data[grep("War", names(ALL_tokens_data))]






# Word clouds

ALL_wc <- textplot_wordcloud(ALL_dfm)

CNN_WC <- textplot_wordcloud(CNN_dfm)

BBC_WC <- textplot_wordcloud(BBC_dfm)

FOX_WC <- textplot_wordcloud(FOX_dfm)

FOX_bigrams <- tokens_ngrams(FOX_tokens_data)

FOX_bigrams_wc <- textplot_wordcloud(FOX_bigrams)

war_wc <- textplot_wordcloud(war_dfm)

election_wc <- textplot_wordcloud(election_dfm)

corona_wc <- textplot_wordcloud(corona_dfm)


# create wordfish textplots to find political leaning


war_wordfish <- textmodel_wordfish(war_dfm)
summary(war_wordfish)
textplot_scale1d(war_wordfish)

election_wordfish <- textmodel_wordfish(election_dfm)
summary(election_wordfish)
textplot_scale1d(election_wordfish)

corona_wordfish <- textmodel_wordfish(corona_dfm)
summary(corona_wordfish)
textplot_scale1d(corona_wordfish)


CNN_wordfish <- textmodel_wordfish(CNN_dfm)
summary(CNN_wordfish)
textplot_scale1d(CNN_wordfish)

BBC_wordfish <- textmodel_wordfish(BBC_dfm)
summary(BBC_wordfish)
textplot_scale1d(BBC_wordfish)

FOX_wordfish <- textmodel_wordfish(FOX_dfm)
summary(FOX_wordfish)
textplot_scale1d(FOX_wordfish)

ALL_kwic <- kwic(
  corona_data,
  pattern = phrase("people"),
  window = 10,
  case_insensitive = TRUE
)
textplot_xray(ALL_kwic)


#FOX_collocations <- textstat_collocations(FOX_tokens_data, size = 2)

#BB_collocations <- textstat_collocations(BBC_tokens_data, size = 2)

#CNN_collocations <- textstat_collocations(CNN_tokens_data, size = 2)

#ALL_collocations <- textstat_collocations(ALL_tokens_data, size = 2)




# btm model to find topics

#---------------------------------------------------------------------------------------------------------------
#a second filtering process


filter_document_tokens <- function(input_tibble,
                                   token_col = "token",
                                   doc_col = "doc_id",
                                   auto_detect = TRUE,
                                   custom_noise = NULL,
                                   freq_threshold = 0.25) {

  # Validate input
  if (!inherits(input_tibble, "data.frame")) {
    stop("Input must be a data frame or tibble")
  }

  if (!all(c(token_col, doc_col) %in% names(input_tibble))) {
    stop(paste("Columns", token_col, "and/or", doc_col, "not found in input data"))
  }

  # 1. Automatically detect noise words
  auto_noise <- if (auto_detect) {
    term_stats <- input_tibble %>%
      group_by(!!sym(token_col)) %>%
      summarize(
        doc_freq = n_distinct(!!sym(doc_col)),
        term_freq = n()
      ) %>%
      ungroup()

    total_docs <- n_distinct(input_tibble[[doc_col]])

    term_stats %>%
      filter(doc_freq > (total_docs * freq_threshold)) %>%
      pull(!!sym(token_col))
  } else {
    character(0)
  }

  # 2. Combine noise words
  noise_words <- unique(c(auto_noise, custom_noise))

  # 3. Filter tokens
  filtered_tibble <- input_tibble %>%
    filter(!(!!sym(token_col)) %in% noise_words)

  # Calculate statistics
  original_terms <- n_distinct(input_tibble[[token_col]])
  remaining_terms <- n_distinct(filtered_tibble[[token_col]])
  removed_terms <- length(noise_words)

  # Return results
  list(
    filtered_data = filtered_tibble,
    noise_words = noise_words,
    stats = list(
      original_terms = original_terms,
      remaining_terms = remaining_terms,
      removed_terms = removed_terms,
      original_documents = n_distinct(input_tibble[[doc_col]]),
      remaining_documents = n_distinct(filtered_tibble[[doc_col]])
    )
  )
}
filtered_war_data <- filter_document_tokens(war_data_df)
-----------------------------------------------------------


war_data_df <- tibble(
  doc_id = docnames(filtered_war_data),
  token = as.list(war_data)
) %>%
  unnest(token)



set.seed(2025)
k <- 10
btm_model_war <- BTM(
 filtered_war_data_df,
  alpha = 1,
  beta = 0.01,
  iter = 1000,
  window = 15,
  background = FALSE,
  trace = 100

)

topics__war <- terms(btm_model_war, top = 10)
print(topics_war)

#===========================================================================

corona_data_df <- tibble(
  doc_id = docnames(corona_data),
  token = as.list(corona_data)
) %>%
  unnest(token)



set.seed(2025)
k <- 10
btm_model_corona <- BTM(
  corona_data_df,
  alpha = 1,
  beta = 0.01,
  iter = 1000,
  window = 15,
  background = FALSE,
  trace = 100

)

topics_corona <- terms(btm_model_corona, top = 10)
print(topics_corona)


#=======================================================================

election_data_df <- tibble(
  doc_id = docnames(election_data),
  token = as.list(election_data)
) %>%
  unnest(token)



set.seed(2025)
k <- 10
btm_model_election <- BTM(
  election_data_df,
  alpha = 1,
  beta = 0.01,
  iter = 1000,
  window = 15,
  background = FALSE,
  trace = 100

)

topics_election <- terms(btm_model_election, top = 10)
print(topics_election)


#===========================================================================

topics_election_df <- as.data.frame(topics_election)

topics_war_df <- as.data.frame(topics_war)

topics_corona_df <- as.data.frame(topics_corona)




#Visualization



topics_war_df <- do.call(rbind, lapply(seq_along(topics_war), function(i) {
  data.frame(
    topic = paste("Topic", i),
    terms = topics_war[[i]]$token,
    probability = topics_war[[i]]$probability,
    stringsAsFactors = FALSE
  )
}))

# Create faceted bar plot (using topics_war_df to match the variable name)
ggplot(topics_war_df, aes(x = reorder(terms, probability), y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Term", y = "Probability") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))



#==============================================================================


topics_election_df <- do.call(rbind, lapply(seq_along(topics_election), function(i) {
  data.frame(
    topic = paste("Topic", i),
    terms = topics_election[[i]]$token,
    probability = topics_election[[i]]$probability,
    stringsAsFactors = FALSE
  )
}))

# Create faceted bar plot (using topics_war_df to match the variable name)
ggplot(topics_election_df, aes(x = reorder(terms, probability), y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Term", y = "Probability") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))

#=========================================================================

topics_corona_df <- do.call(rbind, lapply(seq_along(topics_corona), function(i) {
  data.frame(
    topic = paste("Topic", i),
    terms = topics_corona[[i]]$token,
    probability = topics_corona[[i]]$probability,
    stringsAsFactors = FALSE
  )
}))


ggplot(topics_corona_df, aes(x = reorder(terms, probability), y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Term", y = "Probability") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))





# create Network grafic for co-occurance, specify data set for variable w5


w5 <- fcm(corona_data, context = "window", window = 5)




# 1. Create filtered co-occurrence matrix
feature_sums <- colSums(w5)
top_terms <- names(sort(feature_sums, decreasing = TRUE)[1:25])  # Top 100 terms
w5_filtered <- fcm_select(w5, pattern = top_terms, selection = "keep")

# 2. Convert to igraph object (with symmetric matrix)
adj_matrix <- as.matrix(w5_filtered)
adj_matrix <- pmax(adj_matrix, t(adj_matrix))  # Force symmetry for undirected graph

g <- graph_from_adjacency_matrix(
  adj_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# 3. Apply edge weight threshold
edge_threshold <- 5
g <- delete_edges(g, E(g)[weight < edge_threshold])

# 4. Remove isolated nodes
g <- delete_vertices(g, degree(g) == 0)

# 5. Calculate centrality measures
V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g, weights = NA)  # Unweighted betweenness

# 6. Create the network plot
set.seed(123)  # For reproducible layout

network_plot <- ggraph(g, layout = "fr") +  # Force-directed layout
  # Edges
  geom_edge_link(
    aes(width = weight, alpha = weight),
    color = "grey50",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.5, 3)) +

  # Nodes
  geom_node_point(
    aes(size = degree, fill = betweenness),
    shape = 21,
    color = "white",
    alpha = 0.8
  ) +
  scale_size(range = c(3, 10)) +
  scale_fill_gradient(low = "lightblue", high = "red") +

  # Labels
  geom_node_text(
    aes(label = name, size = degree/2),
    repel = TRUE,
    max.overlaps = 20,
    family = "sans",
    fontface = "bold"
  ) +

  # Cosmetic improvements
  labs(
    title = "Term Co-occurrence Network",
    subtitle = paste("Top 25 terms | Edge threshold:", edge_threshold),
    fill = "Betweenness\nCentrality",
    size = "Degree"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Display the plot
print(network_plot)

