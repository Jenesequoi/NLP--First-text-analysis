#Install and loading packages 
install.packages("pdftools")
install.packages("RColorBrewer")
install.packages("tidytext")
library(pdftools)
library(dplyr)
library(purrr)
library(tm)
library(wordcloud)
library(RColorBrewer)
pdf<- "C:/Users/LENOVO/Documents/NLP/Women Who Run with the Wolves ( PDFDrive ).pdf"
pdf_text<- map(pdf, pdf_text)
text_df<- data_frame(document=pdf, text= pdf_text)

#Create words df, line number
library(magrittr)
library(tidytext)
library(tidyr)
book_words<-text_df %>%
  unnest %>%
  group_by(document)%>%
  mutate(linenumber=row_number())%>%
  ungroup()%>%
  unnest_tokens(word,text)%>%
  filter(word != "1")

#Create word count summary
summary_words<- book_words%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarise(count=n())%>%
  arrange(desc(count))

#Line count
book_words%>%
  summarise(total_lines=n())

#Unique word count
book_words%>%
  mutate(unique=n_distinct(word))%>%
  count(unique)

#term frequency, inverse document analysis
strategic_tfidf<-book_words %>%
  count(document, word,sort=TRUE)%>%
  bind_tf_idf(word,document,n)%>%
  arrange(desc(tf_idf))

#visualize tf, idf
library(ggplot2)
library(colorspace)
summary_words%>%
  top_n(15) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(x=word)) +
  geom_col(aes(y= count), fill= 'light blue')+
  ylab("term frequency")+
  coord_flip()

#wordcloud
# library(wordcloud)
# corpus = VCorpus(VectorSource(summary_words))
# wordcloud(corpus,min.freq=20, max.words=50,colors=brewer.pal(8, "Set1"))

#co occuring words
book_bigrams<- text_df%>%
  unnest%>%
  unnest_tokens(bigram,text,token="ngrams",n=2)

book_bigrams%>%
  count(bigram,sort=TRUE)

#remove stopwords
clean_bigrams<-book_bigrams%>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered<-clean_bigrams%>%
  filter(!word1%in%stop_words$word)%>%
  filter(!word2%in%stop_words$word)

#sort bigrams
bigrams_count<-bigrams_filtered%>%
  count(word1,word2, sort = TRUE)


unite_bigrams<-bigrams_filtered%>%
  unite(bigram,word1,word2,sep = " ")%>%
  count(bigram)%>%
  arrange(desc(n))


# bigram_tf_idf<-unite_bigrams%>%
#   count(document, bigram)%>%
#   bind_tf_idf(bigram,document,n)%>%
#   arrange(desc(tf_idf))
remove(bigram_tf_idf)

#visualize bigrams
unite_bigrams%>%
  top_n(15)%>%
  mutate(bigram = reorder(bigram, n))%>%
  ggplot(aes(bigram,n,fill = "purple")) +
  geom_col(show.legend = FALSE) +
  ylab("frequency") +
  coord_flip()

#visualize using arrows
library(influential)
library(ggraph)
visualize_bigrams<- function(bigrams) {
  set.seed(2016)
  a<-grid::arrow(type="closed",length=unit(.15,"inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout="fr") +
    geom_edge_link(aes(edge_alpha=n),show.legend=FALSE, arrow=a, end_cap=circle(.05,'inches')) +
    geom_node_point(color="lightblue",size = 4) +
    geom_node_text(aes(label=name),vjust=1,hjust=1) +
    theme_void()
}
count_bigrams<- function(dataset) { dataset %>%
    unnest_tokens(bigram, text, token="ngrams", n=2) %>%
    separate(bigram, c("word1","word2"),sep=" ") %>%
    filter(!word1 %in% stop_words$word)%>%
    filter(!word2 %in% stop_words$word)%>% count(word1,word2,sort=TRUE)
}

bigrams<- text_df%>%
  unnest%>%
  count_bigrams()

library(stringr)

bigrams %>% filter(n>20,
                   !str_detect(word1, "\\d"),
                   !str_detect(word2, "\\d")) %>% visualize_bigrams()

#sentiment analysis
strategic_sentiments<-unite_bigrams%>%
  inner_join(get_sentiments("bing"),by=c(word="word1"))

strategic_sentiments %>%
  count(sentiment,word,wt=count)%>%
  ungroup()%>%
  filter(n>=10)%>%
  mutate(n=ifelse(sentiment=="negative", -n, n))%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()