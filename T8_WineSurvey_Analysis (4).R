#############################################
###### Team 8 Wine Survey Analysis ##########
#############################################

#libraries needed for data analysis
library(rvest)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(widyr)
library(scales)
library(ggplot2)
library(textdata)
library(stringr)
library(wordcloud)
library(reshape2)
library(forcats)
library(tm)
library(pdftools)
library(reshape2)
library(ggraph)
library(igraph)
library(textreadr)
library(plotly)
library(quanteda)
library(RColorBrewer)
library(quanteda.textmodels)
### Loading Data to R ###
setwd("/Users/stavinvas/Desktop/Surveys")
survey <- list.files(path = "/Users/stavinvas/Desktop/Surveys")

#creating a loop to read add all the files in by line
survey_df <- data_frame()

for (i in 1:length(survey)) {
  text_place <- read_document(survey[i])
  df_two <- data_frame(survey = i, question = 1:length(text_place), text = text_place)
  survey_df <- rbind(survey_df, df_two)
}

#load stop_words to analyze dfs
data(stop_words)

#tidy data, with stop words
survey_tidy <- survey_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#counting most frequent words without stopwords
survey_freq <- survey_tidy %>%
  count(word, sort=T)

# bar chart of the most frequent tokens
freq_hist <- survey_tidy %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#####################################################
### Correlation Analysis Between Survey Questions ###
#####################################################

#Pairwise analysis between freq words in the questions
survey_cors <- survey_tidy %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, question, sort=TRUE) #check correlation on how many times a word is in the answer

#finding correlations in relation to a specific word
survey_cors %>%
  filter(item1 == "cuisine")

survey_cors <- data.frame(survey_cors)


### GRAPH NEEDS WORK ###

#correlation histogram
survey_cors %>%
  filter(item1 %in% c("food", "wine", "cuisine", "dinner", "creative")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_bar(stat = TRUE )+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

### Correlation Network ###
#expand plot area to see full network
#item1 grouped
survey_cors %>%
  group_by(item2) %>%
  filter(correlation >0.2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+ #shows links between
  geom_node_point(color = "purple", size=6)+ #similar to ngram semantic method/template
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

#item2 grouped
survey_cors %>%
  group_by(item2) %>%
  filter(correlation >0.2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+ #shows links between
  geom_node_point(color = "purple", size=6)+ #similar to ngram semantic method/template
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

#############################################
########### Bi-grams and TF-IDF #############
#############################################

#creating bigrams for the survey 
survey_bigrams <- survey_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2) %>% #2 refers to bigram
  filter(!is.na(bigram))

#frequent bigrams
bigram_freq <- survey_bigrams %>%
  count(bigram, sort = TRUE) 

#separating stop words from bigrams
bigrams_sep <- survey_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#filtering out stop words
bigrams_filt<- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#count most common bigram
bigram_counts <- bigrams_filt %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

### Bi-grams and TF-IDF ###

bigram_unite <- bigrams_filt %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

#applying tf-idf to bigrams for questions
bigram_tf_idf <- bigram_unite %>%
  count(question, bigram) %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf))

view(bigram_tf_idf)


#################3 sentiment anlysis 
##### afinn
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")


sentiments <- bind_rows(mutate(afinn, lexicon ="afinn"),
                        mutate(nrc, lexicon = "nrc"),
                        mutate(bing, lexicon = "bing"))
afinn <- sentiments%>%
  filter(lexicon =="afinn")
# mean value 
survey_afinn <- survey_tidy %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

# negative sentiment----- not running  
survey_tidy %>%
  inner_join(afinn)%>%
  mutate(tokensentiment = n* value)#%>%
  #arrange(desc(-tokensentiment))
# positive sentiment----- not running  
survey_token %>%
  inner_join(afinn)%>%
  mutate(tokensentiment = n* value)%>%
  arrange(desc(tokensentiment))


####################bing 
survey_senti <- survey_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

survey_bing<- survey_senti %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
ggplotly(survey_bing)


####### nrc 
survey_senti_nrc <- survey_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

survey_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25)
        ###########################################
        ### ziffs law and tf idf
################3
original_survey <- survey_df %>%
  unnest_tokens(word, text) %>%
  count(question, word, sort=TRUE) %>%
  ungroup()

total_words <- original_survey %>%
  group_by(question) %>%
  summarize(total=sum(n))

survey_words <- left_join(original_survey, total_words)

survey_words

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- survey_words %>%
  group_by(question) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=question))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=question))+
  #let's add a tangent line , the first derivative, and see what the slope is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)

###################################################
################# TF_IDF ##########################
###################################################

survey_words <- survey_words %>%
  bind_tf_idf(word, question, n)

survey_words 

survey_words %>%
  arrange(desc(tf_idf))%>%
  filter(n<2)

# looking at the graphical apprach:
survey_tf <-survey_words %>%
  anti_join(stop_words)%>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels =rev(unique(word)))) %>%
  group_by(question) %>%
  filter(n<5)%>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()
survey_tf






frequency <- bind_rows(mutate(survey_tidy, author = "survey_df")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  spread(author, proportion) %>%
  gather(author, proportion, `survey_df`)

corr_plot <- ggplot(frequency, aes(x=proportion, y=survey_tidy,
                           color = abs(survey_df- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(aes(text=paste("word: ", word)), alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), colour="gray20", alpha=1) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "survey_df", x=NULL)

  corr_plot




survey_words %>%
  anti_join(stop_words)%>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels =rev(unique(word)))) %>%
  group_by(question) 

