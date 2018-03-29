#5. Converting to and from non-tidy formats

#5.1 Tidying a document-term matrix
#document-term matrix (DTM):
#1. each row represents one document(such as a book or article),
#2. each colmn represents one term, and
#3. each value(typically) contains the number of appearances of that term in that document.
#usually it is implemented as sparse matrices as it stores that data in a more efficient format.

#DTM objects cannot be used directly with tidy tools, two alternatives:
#1. tidy(): turns a DTM into a tidy data frame.
#2. cast(): turns a tidy one-term-per-row data frame into a matrix. tidytext provides three variations:
    #1. cast_sparse()
    #2. cast_dtm()
    #3. cast_dfm()

#5.1.1 Tidying DocumentTermMatrix objects
library(tm)
data("AssociatedPress", package = 'topicmodels')
AssociatedPress

#access the terms in the documents with the Terms()
term <- Terms(AssociatedPress)
tail(term)

#To analyse this data with tidy tools, we would first need to turn it into a data frame with one-token-per-document-per-row.
library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td

#sentiment analysis
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments('bing'), by = c(term = 'word'))
ap_sentiments

#visualisation
library(ggplot2)
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) + 
  geom_bar(stat = 'identity') + 
  ylab("contribution to sentiment") +
  coord_flip()

#5.1.2 tidying dfm objects
#dfm: document-feature-matrix
library(quanteda)
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = F)
inaug_dfm
inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
inaug_tf_idf

library(tidyr)
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = T) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count/year_total)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(~term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) + 
  ylab("% frequency of word in inaugural address")

#5.2 Casting tidy text data into a matrix
#cast_() is used for converting from a tidy form to other formats
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)
m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)
dim(m)

library(janeaustenr)
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
austen_dtm

#5.3 Tidyingg corpus objects with metadata
#some data structures are designed to store document collections before tokenization, often called a corpus.
data('acq')
acq
acq[[1]] #first document
#on this data we can't use tidy tools
acq_td <- tidy(acq)
acq_td

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")
acq_tokens %>%
  count(word, sort = T)

acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))
