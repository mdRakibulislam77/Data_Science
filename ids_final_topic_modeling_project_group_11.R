install.packages(c("rvest", "dplyr", "httr", "lubridate", "readr", "stringr", "textclean", "hunspell", "tidytext", "tidyr", "textstem", "tm", "SnowballC", "topicmodels", "tibble"))

library(rvest)
library(dplyr)
library(httr)
library(lubridate)
library(readr)
library(stringr)
library(textclean)
library(hunspell)
library(tidytext)
library(tidyr)
library(textstem)


safe_GET <- function(url, retries = 3, delay_sec = 2) {
  headers <- add_headers(`User-Agent` = "Mozilla/5.0")
  for (i in seq_len(retries)) {
    res <- try(GET(url, headers, timeout(10)), silent = TRUE)
    if (!inherits(res, "try-error") && status_code(res) == 200) {
      return(res)
    } else {
      message(paste("Request failed:", url, "- Retry", i))
      Sys.sleep(delay_sec)
    }
  }
  return(NULL)
}


get_full_article_text <- function(article_url) {
  res <- safe_GET(article_url)
  if (is.null(res)) {
    message("Failed to fetch full article:", article_url)
    return(NA_character_)
  }
  page <- read_html(res)
  selectors <- c(
    "div.article-body p",
    "div#storytext p",
    "div[data-testid='story-text'] p",
    "div.selectorgadget_suggested p"
  )
  for (sel in selectors) {
    paragraphs <- page %>% html_nodes(sel) %>% html_text(trim = TRUE)
    if (length(paragraphs) > 0 && any(nzchar(paragraphs))) {
      return(paste(paragraphs, collapse = "\n\n"))
    }
  }
  return(NA_character_)
}


get_npr_articles <- function(category, max_articles = 100) {
  base_url <- paste0("https://www.npr.org/sections/", category, "/")
  articles <- list()
  page <- 1
  while (length(articles) < max_articles) {
    url <- paste0(base_url, "?page=", page)
    cat("📄 Scraping", category, "- Page", page, "\n")
    res <- safe_GET(url)
    if (is.null(res)) break
    webpage <- read_html(res)
    article_nodes <- html_nodes(
      webpage,
      ".item-info, .bucketwrap, .internallink, .insettwocolumn, .inset2col, .bucketwrap.image.large"
    )
    if (length(article_nodes) == 0) break
    for (node in article_nodes) {
      title <- node %>% html_node("h2.title, h3.title") %>% html_text(trim = TRUE)
      link <- node %>% html_node("a") %>% html_attr("href")
      date_str <- node %>%
        html_node("time") %>%
        { if (!is.null(.)) html_attr(., "datetime") %||% html_text(., trim = TRUE) else NA_character_ }
      parsed_date <- parse_date_time(date_str, orders = c("Ymd HMS", "Ymd", "mdY", "B d, Y"), quiet = TRUE)
      if (any(is.na(c(title, link, parsed_date))) || grepl("/podcasts/", link)) {
        next
      }
      full_text <- tryCatch({
        get_full_article_text(link)
      }, error = function(e) NA_character_)
      if (is.na(full_text) || str_trim(full_text) == "") next
      articles[[length(articles) + 1]] <- tibble(
        title = title,
        description = full_text, 
        date = parsed_date,
        category = category,
        link = link
      )
      if (length(articles) >= max_articles) break
      Sys.sleep(1)
    }
    page <- page + 1
    Sys.sleep(2)
  }
  bind_rows(articles)
}


categories <- c("business", "health", "politics", "technology", "world")
all_articles <- bind_rows(lapply(categories, get_npr_articles, max_articles = 100))
write_csv(all_articles, "C:/DS_project/ids_final_project_group_11_news_raw.csv")
message("Scraping complete. File saved: ids_final_project_group_11_news_raw.csv")

df <- all_articles 
df <- df %>%
  mutate(description_contracted = replace_contraction(description))

df <- df %>%
  mutate(
    description_emojis_handled = replace_emoji(description_contracted),
    description_emojis_handled = replace_emoticon(description_emojis_handled),
    description_emojis_handled = gsub("<e2><80><94>", " ", description_emojis_handled, fixed = TRUE),
    description_emojis_handled = gsub("<c2><a0>", " ", description_emojis_handled, fixed = TRUE)
  )

correct_spelling <- function(text) {
  words <- unlist(strsplit(text, " "))
  misspelled <- hunspell(words)[[1]]
  if (length(misspelled) == 0) return(text)
  for (word in misspelled) {
    suggestion <- hunspell_suggest(word)[[1]]
    if (length(suggestion) > 0) {
      words[words == word] <- suggestion[1]
    }
  }
  return(paste(words, collapse = " "))
}
df <- df %>%
  mutate(description_spellchecked = sapply(description_emojis_handled, correct_spelling))

clean_text <- function(text) {
  text <- gsub("’s|\'s", "s", text)
  text <- tolower(text)
  text <- gsub("<.*?>", " ", text)
  text <- gsub("[^a-z\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- str_trim(text)
  return(text)
}
df <- df %>%
  mutate(cleaned_description = sapply(description_spellchecked, clean_text))

data("stop_words")
df$id <- 1:nrow(df)

tokens_filtered <- df %>%
  select(id, cleaned_description) %>%
  unnest_tokens(word, cleaned_description) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(id) %>%
  summarise(tokens_no_stop = list(word), .groups = "drop")

df <- df %>%
  left_join(tokens_filtered, by = "id")


df <- df %>%
  mutate(tokens_lemmatized = lapply(tokens_no_stop, lemmatize_words)) %>%
  mutate(clean_description = sapply(tokens_lemmatized, function(x) paste(x, collapse = " ")))


df <- df %>%
  select(title, date, category, link, description, clean_description)

write.csv(df, "C:/DS_project/ids_final_project_group_11_news_clean.csv", row.names = FALSE)
message("Preprocessing complete. File saved:ids_final_project_group_11_news_clean.csv")

#Topic Modelling

library(tm)
library(SnowballC)
library(topicmodels)
library(tibble)

df <- read.csv("C:/Users/user/Pictures/New folder (2)/clean_text_npr1.csv")

corpus <- VCorpus(VectorSource(df$clean_description))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
inspect(dtm[1:10, 1:10])

num_topics <- 5
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))#Number of latent topics to extract. Ensures reproducibility



terms(lda_model, 10)#Returns the top 10 words for each topic
print(topics(lda_model)[1:10])#Prints the most probable topic assignment for the first 10 documents.

top_n <- 10
topic_word_prob <- posterior(lda_model)$terms
words <- colnames(topic_word_prob)

for (i in 1:num_topics) {
  cat("\nTopic", i, "- Top", top_n, "words:\n")
  idx <- order(topic_word_prob[i, ], decreasing = TRUE)[1:top_n]
  print(data.frame(word = words[idx], prob = round(topic_word_prob[i, idx], 4)))
}

topic_distribution <- posterior(lda_model)$topics
df$dominant_topic <- apply(topic_distribution, 1, which.max)


topic_counts <- table(df$dominant_topic)
cat("\nNumber of documents per dominant topic:\n")
print(topic_counts)

doc_topic_df <- as.data.frame(topic_distribution)
colnames(doc_topic_df) <- paste0("Topic_", 1:num_topics)
cat("\nDocument-wise Topic Probability Overview (Top 10 Documents):\n")
print(head(doc_topic_df, 10))

category_topic_table <- table(df$category, df$dominant_topic)
cat("\nCategory distribution across topics:\n")
print(category_topic_table)

topic_categories <- character(num_topics)
cat("\nMost representative category for each topic:\n")
for (i in 1:num_topics) {
  topic_col <- category_topic_table[, as.character(i)]
  top_category <- names(which.max(topic_col))
  count <- max(topic_col)
  topic_categories[i] <- top_category
  cat(paste0("Topic ", i, " is most associated with category: ", top_category,
             " (", count, " documents)\n"))
}

cat("\nFinal Topic-Category Interpretation Summary:\n")
for (i in 1:num_topics) {
  idx <- order(topic_word_prob[i, ], decreasing = TRUE)[1:10]
  top_words <- paste(words[idx], collapse = ", ")
  top_category <- topic_categories[i]
  cat(paste0("\nTopic ", i, "\n",
             "Top Words: ", top_words, "\n",
             "Most Associated Category: ", top_category, "\n"))
}
