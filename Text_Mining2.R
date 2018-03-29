#2. Sentiment Analysis with Tidy data.

#2.1 The sentiments dataset.
library(tidytext)
sentiments

#The three general purpose lexicons are:
#1. afinn
#2. bing
#3. nrc
get_sentiments('afinn')

#2.2 Sentiment analysis with inner join.
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books=austen_books() %>%
  group_by(book) %>%
  mutate(linenumber= row_number(),
         chapter=cumsum(str_detect(text,regex('^chapter [\\divxlc]',
                                   ignore_case=T)))) %>%
  ungroup() %>%
  unnest_tokens(word,text)
tidy_books

nrcjoy=get_sentiments('nrc') %>%
  filter(sentiment=='joy')
tidy_books %>%
  filter(book == 'Emma') %>%
  inner_join(nrcjoy) %>%
  count(word, sort= T)

library(tidyr)
janeaustensentiment= tidy_books %>%
  inner_join(get_sentiments('bing')) %>%
  count(book, index=linenumber %/% 80, sentiment)%>% 
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment=positive - negative)
janeaustensentiment
# %/% operator does integer division so the index keeps track of which 80-line section of text we are
#counting up negative and positive sentiment in.

library(ggplot2)
ggplot(janeaustensentiment, aes(index, sentiment, fill= book))+
  geom_col(show.legend = F)+
  facet_wrap(~book, ncol=2, scales = 'free_x')
?facet_wrap #Wrap a 1d ribbon of panels into 2d
?geom_col #There are two types of bar charts: geom_bar makes the height of the bar proportional to the
#number of cases in each group. If you want the height of the bars to represent values in the data,
#use geom_col instead. geom_bar must not be used with a y aesthetic.

#2.3 Comparing the three sentiment dictionaries.
pride_prejudice=tidy_books %>%
  filter(book == 'Pride & Prejudice')
pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
afinn

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
bing_and_nrc

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend = F)+
  facet_wrap(~method, ncol=1, scales = 'free_y')

#2.4 Most common positive and negative words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

custom_stop_words=bind_rows(data_frame(word=c("miss"),lexicon=c("custom")),
                            stop_words)
custom_stop_words

#Wordclouds
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% #acast is in reshape2
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()