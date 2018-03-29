#3. Analysing word and document frequency:tf-idf

#term frequency (tf), how frequently a word occurs in a document
#inverse document frequency (idf), which decreases the weight for commonly used words and increases 
#the weight for words that are not used very much in a collection of documents
#The statistic tf-idf is intended to measure how important a word is to a document in a collection 
#(or corpus) of documents, for example, to one novel in a collection of novels or to one website in a 
#collection of websites.

#3.1 Term frequency in Jane Austen's novels
#the number of times a word appears in a novel divided by the total number of terms (words) in that 
#novel. This is exactly what term frequency is.
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup() #why are we using ungroup()?
book_words

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words

book_words <- left_join(book_words, total_words)
book_words

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#3.2 Zipf's Law
#Zipf's law states that the frequency that a word appears is inversely proportional to its rank.

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

#power law
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
#to see the relationship between 'term frequency' and rank.

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

#3.3 The bind_tf_idf function

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels= rev(unique(word))))
?factor #

plot_austen %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

#look at the novel individually
plot_austen %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

#3.4 A corpus of physics texts
library(gutenbergr)
physics=gutenberg_download(c(37729, 14725, 13476, 5001), 
                           meta_fields = "author")

#to find out how many times each wrod was used in each text.
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()
physics_words

physics_words <- physics_words %>%
  bind_tf_idf(word, author, n) 

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics

plot_physics %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

#let's look at each text individually
plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

#One thing we see here is "eq" in the Einstein text?
library(stringr)

physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)

#removing stop words
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
