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

bigrams_count<-bigrams_filtered%>%
  count(word1,word2,sort = TRUE)
bigrams_count
