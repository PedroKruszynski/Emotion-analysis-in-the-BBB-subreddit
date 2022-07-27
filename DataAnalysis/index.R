install.packages("devtools")
install.packages("slam")
install.packages("NLP")
install.packages("tm", dependencies = TRUE)
install.packages("wordcloud")

install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")

library(dplyr)
library(tm)
library(wordcloud)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)

reddit <- read.csv(file = "~/Documents/TCC_project/DataMining/dados.csv", sep = ",", head = TRUE, encoding = "UTF-8")
reddit_dataset <- reddit[!(is.na(reddit$comment) | reddit$comment==""), ]
reddit_dataset$comment <- tolower(reddit$comment)

stopwords <- read.delim2(file = "~/Documents/TCC_project/DataAnalysis/stopwords.txt", sep = ",", header = FALSE, encoding = "UTF-8")
labelsForBar <- c(
  "Raiva",
  "Expectativa",
  "Desgosto",
  "Medo",
  "Alegria",
  "Tristeza",
  "Surpresa",
  "Confiança",
  "Negativo",
  "Positivo"
)

#corpus_comment <- Corpus(VectorSource(tolower(reddit_dataset$comment)))

jadeComments = reddit_dataset %>% filter(grepl('jade|picon', comment))
arthurComments = reddit_dataset %>% filter(grepl('arthur|aguiar', comment))
tiagoComments = reddit_dataset %>% filter(grepl('tiago|abravanel', comment))
naiaraComments = reddit_dataset %>% filter(grepl('naiara|azevedo', comment))
linnComments = reddit_dataset %>% filter(grepl('linn', comment))
mariaComments = reddit_dataset %>% filter(grepl('maria', comment))
douglasComments = reddit_dataset %>% filter(grepl('douglas|silva', comment))
pauloComments = reddit_dataset %>% filter(grepl('paulo|andre', comment))
brunnaComments = reddit_dataset %>% filter(grepl('brunna|gonçalves|goncalves', comment))
pedroComments = reddit_dataset %>% filter(grepl('pedro|scooby', comment))
laisComments = reddit_dataset %>% filter(grepl('lais|laís', comment))
lucianoComments = reddit_dataset %>% filter(grepl('luciano', comment))
jessilaneComments = reddit_dataset %>% filter(grepl('jessilane|jessi', comment))
eliezerComments = reddit_dataset %>% filter(grepl('eliezer|eli', comment))
esloveniaComments = reddit_dataset %>% filter(grepl('eslovênia|eslovenia|eslo|eslô', comment))
lucasComments = reddit_dataset %>% filter(grepl('lucas', comment))
barbaraComments = reddit_dataset %>% filter(grepl('bárbara|barbara', comment))
rodrigoComments = reddit_dataset %>% filter(grepl('rodrigo|mussi', comment))
viniciusComments = reddit_dataset %>% filter(grepl('vinícius|vinicius', comment))
nataliaComments = reddit_dataset %>% filter(grepl('natália|natalia|naty', comment))
gustavoComments = reddit_dataset %>% filter(grepl('gustavo|marsengo', comment))

analyzeDouglas <- get_nrc_sentiment(douglasComments$comment, lang = "portuguese")
barplot(
  colSums(analyzeDouglas),
  las = 2,
  col = rainbow(10),
  ylab = 'amount',
  main = 'Sentiment Analysis: Douglas',
  names.arg = labelsForBar,
)
