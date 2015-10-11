library(tm)
library(dplyr)

word_fq <- function(text) {
  corpus <- paste(text, collapse=" ") %>% VectorSource() %>% Corpus()

  # text cleanup
  corpus <- tm_map(corpus, content_transformer(tolower)) %>%
              tm_map(removePunctuation) %>%
                tm_map(stripWhitespace)

  # document matrix
  dtm <- DocumentTermMatrix(corpus) %>% as.matrix()

  # calculate frequencies
  frequency <- colSums(dtm) %>% sort(decreasing=TRUE)

  # remove stopwords
  frequency <- frequency[!(names(frequency) %in% stopwords("french"))]
  frequency <- frequency[!(names(frequency) %in% stopwords("english"))]

  df <- data.frame(word=names(frequency), n=frequency)
  rownames(df) <- NULL

  return(df)
}

orders <- read.csv('../data/src/orders/orders.csv', encoding = 'UTF-8', stringsAsFactors = FALSE)
precis_fq_df <- word_fq(orders$precis)
write.csv(precis_fq_df, '../data/output/precis_word_frequency.csv', row.names = FALSE, quote = FALSE)

attachments <- read.csv('../data/src/orders/attachments.csv', encoding = 'UTF-8', stringsAsFactors = FALSE)
attachments_fq_df <- word_fq(attachments$attachment)
write.csv(attachments_fq_df, '../data/output/attachments_word_frequency.csv', row.names = FALSE, quote = FALSE)