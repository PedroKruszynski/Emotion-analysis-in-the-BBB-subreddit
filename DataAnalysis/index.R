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

pathByBrother <- '~/Documents/TCC_project/DataAnalysis/Plots/ByBrother/'
createBar <- function(dataset, name) {
  png(sprintf('%sSentiment Analysis: %s.png', pathByBrother, name))
  barplot(
    colSums(dataset),
    las = 2,
    col = rainbow(10),
    ylab = 'amount',
    main = sprintf('Sentiment Analysis: %s', name)
  )
  dev.off()
}

daysWithEliminations <- c(
  c('25/01', 'Luciano', 'Eliminated'),
  c('01/02', 'Rodrigo', 'Eliminated'),
  c('07/02', 'Maria', 'Out by aggression'),
  c('08/02', 'Naiara', 'Eliminated'),
  c('15/02', 'Bárbara', 'Eliminated'),
  c('22/02', 'Brunna', 'Eliminated'),
  c('27/02', 'Tiago', 'Give up'),
  c('01/03', 'Larissa', 'Eliminated'),
  c('08/03', 'Jade', 'Eliminated'),
  c('15/03', 'Vinicius', 'Eliminated'),
  c('22/03', 'Lais', 'Eliminated'),
  c('29/03', 'Lucas', 'Eliminated'),
  c('03/04', 'Eslovênia', 'Eliminated'),
  c('11/04', 'Linn', 'Eliminated'),
  c('12/04', 'Natalia', 'Eliminated'),
  c('18/04', 'Jessilane', 'Eliminated'),
  c('17/04', 'Eliezer', 'Eliminated'),
  c('19/04', 'Gustavo', 'Eliminated'),
  c('21/04', 'Pedro', 'Eliminated')
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
larissaComments = reddit_dataset %>% filter(grepl('larissa|tomásia|tomasia', comment))

analyzeJade <- get_nrc_sentiment(jadeComments$comment, lang = "portuguese")
analyzeArthur <- get_nrc_sentiment(arthurComments$comment, lang = "portuguese")
analyzeTiago <- get_nrc_sentiment(tiagoComments$comment, lang = "portuguese")
analyzeNaiara <- get_nrc_sentiment(naiaraComments$comment, lang = "portuguese")
analyzeLinn <- get_nrc_sentiment(linnComments$comment, lang = "portuguese")
analyzeMaria <- get_nrc_sentiment(mariaComments$comment, lang = "portuguese")
analyzeDouglas <- get_nrc_sentiment(douglasComments$comment, lang = "portuguese")
analyzePaulo <- get_nrc_sentiment(pauloComments$comment, lang = "portuguese")
analyzeBrunna <- get_nrc_sentiment(brunnaComments$comment, lang = "portuguese")
analyzePedro <- get_nrc_sentiment(pedroComments$comment, lang = "portuguese")
analyzeLais <- get_nrc_sentiment(laisComments$comment, lang = "portuguese")
analyzeLuciano <- get_nrc_sentiment(lucianoComments$comment, lang = "portuguese")
analyzeJessilane <- get_nrc_sentiment(jessilaneComments$comment, lang = "portuguese")
analyzeEliezer <- get_nrc_sentiment(eliezerComments$comment, lang = "portuguese")
analyzeEslovenia <- get_nrc_sentiment(esloveniaComments$comment, lang = "portuguese")
analyzeLucas <- get_nrc_sentiment(lucasComments$comment, lang = "portuguese")
analyzeBarbara <- get_nrc_sentiment(barbaraComments$comment, lang = "portuguese")
analyzeRodrigo <- get_nrc_sentiment(rodrigoComments$comment, lang = "portuguese")
analyzeVinicius <- get_nrc_sentiment(viniciusComments$comment, lang = "portuguese")
analyzeNatalia <- get_nrc_sentiment(nataliaComments$comment, lang = "portuguese")
analyzeGustavo <- get_nrc_sentiment(gustavoComments$comment, lang = "portuguese")
analyzeLarissa <- get_nrc_sentiment(larissaComments$comment, lang = "portuguese")

createBar(analyzeJade, 'Jade')
createBar(analyzeArthur, 'Arthur')
createBar(analyzeTiago, 'Tiago')
createBar(analyzeNaiara, 'Naiara')
createBar(analyzeLinn, 'Linn')
createBar(analyzeMaria, 'Maria')
createBar(analyzeDouglas, 'Douglas')
createBar(analyzePaulo, 'Paulo')
createBar(analyzeBrunna, 'Brunna')
createBar(analyzePedro, 'Pedro')
createBar(analyzeLais, 'Láis')
createBar(analyzeLuciano, 'Luciano')
createBar(analyzeJessilane, 'Jessilane')
createBar(analyzeEliezer, 'Eliezer')
createBar(analyzeEslovenia, 'Eslovênia')
createBar(analyzeLucas, 'Lucas')
createBar(analyzeBarbara, 'Bárbara')
createBar(analyzeRodrigo, 'Rodrigo')
createBar(analyzeVinicius, 'Vinicius')
createBar(analyzeNatalia, 'Natalia')
createBar(analyzeGustavo, 'Gustavo')
createBar(analyzeLarissa, 'Larissa')