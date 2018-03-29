text1 <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text1

#Converting the above text into data frame format to do analysis.
library(dplyr)
text_df=data_frame(line=1:4,text=text1)
text_df

#Extra Info
#a Tibble is a modern class of data frame within R,
#that has a convenient print method,
# Tibbles are great for use with tidy tools.

#We need to convert the above data into one-token-per-document-per-row,
#format since we can't filter out words which occur most frequently.
#TOKEN is a meaningful unit of text, that we use for analysis.

library(tidytext)
text_df %>% unnest_tokens(words,text)
?unnest_tokens
#We've split each row so that there is one token in each row of the new data frame.
#By default it converts the tokens to lowercase
#Punctuation has been stripped.

library(janeaustenr)
library(dplyr)
library(stringr)

original_books=austen_books() %>%
  group_by(book) %>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                              ignore_case=T)))) %>%
  ungroup()
original_books
#why did transmute() only drop text column and not both text and book columns?
?austen_books
?group_by
?mutate #adds new variables and preseves existing. transmute() drops existing variables.
?cumsum #cumulative sums
?str_detect #detect the presence or absence of a pattern in a string
?regex #regular expressions, ignore_case: case insensitive; 
      #^ (here, as an anchor) to match the start of the string.

#restructuring it in the one-token-per-row format
tidy_books=original_books %>%
  unnest_tokens(word, text)
tidy_books

#we will want to remove stop words: stop words are words
#that are not usefull for an analysis like the, of, to etc.

data(stop_words)
tidy_books=tidy_books%>% anti_join(stop_words)
#the dataset is stop word free.
?anti_join
#return all rows from x(tidy_books) where there are not matching values in y, 
#keeping just columns from x.

tidy_books %>% count(word,sort=T)

library(ggplot2)
tidy_books %>% count(word,sort=T) %>%
  filter(n>600) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

?filter #use it to find rows/cases where conditions are true; rows where the condition
#evaluates to NA are dropped.
?reorder
#method treats its first argument as a categorical variable, and reorders its levels 
#based on the values of a second variable, usually numeric.

library(gutenbergr)
hgwells=gutenberg_download(c(35,36,5230,159))

tidy_hgwells=hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words)
tidy_hgwells %>% count(word,sort=T)

gutenberg_works(str_detect(title, "Villette")) #to get Project Gutenberg ID numbers for each novel.

bronte=gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte= bronte %>% unnest_tokens(word,text) %>% anti_join(stop_words)

library(tidyr)
frequency=bind_rows( mutate(tidy_bronte, author='Bronte Sisters'),
                     mutate(tidy_hgwells, author='H.G. Wells'),
                     mutate(tidy_books, author='Jane Austen')) %>%
  mutate(word=str_extract(word,'[a-z]+')) %>%
  count(author,word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(author,proportion) %>%
  gather(author,proportion, 'Bronte Sisters':'H.G. Wells')

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
