library(tidytext)
library(tidyverse)
library(topicmodels)

get_sentiments("afinn") -> afinn
bing <- get+sentiments("bing")
get_sentiments("bing") -> bing
get_sentiments()
readLines("step_up.txt") -> step_up

tidy_text <- tibble(line = 1:length(step_up), text = step_up) %>% 
unnest_tokens(word, text)

clean_text <- tidy_text %>% 
  anti_join(stop_words)
clean_text %>%  
  count(word, sort = TRUE)

clean_text %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  summarise(n=sum(n))


dtm <- clean_text %>% 
  count(document = line, word) %>% 
  cast_dtm(document, word, n)
dtm

lda_model <-LDA(dtm, k =2, control = list(seed = 1234))
lda_model

topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)


document_topics <- tidy(lda_model, matrix = "gamma")

top_document_topics <- document_topics %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

print(top_document_topics)

ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Terms", y = "Beta") +
  theme_minimal()
