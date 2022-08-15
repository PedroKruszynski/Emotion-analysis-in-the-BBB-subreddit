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
pathByEliminations <- '~/Documents/TCC_project/DataAnalysis/Plots/ByEliminations/'
createBar <- function(dataset, name, path) {
  png(sprintf('%sSentiment Analysis: %s.png', path, name))
  barplot(
    colSums(dataset),
    las = 2,
    col = rainbow(10),
    ylab = 'amount',
    main = sprintf('Sentiment Analysis: %s', name)
  )
  dev.off()
}

daysWithEliminations <- data.frame(
  dayOfElimination=c(
    '2022-01-25',
    '2022-02-01',
    '2022-02-07',
    '2022-02-08',
    '2022-02-15',
    '2022-02-22',
    '2022-02-27',
    '2022-03-01',
    '2022-03-08',
    '2022-03-15',
    '2022-03-22',
    '2022-03-29',
    '2022-04-03',
    '2022-04-11',
    '2022-04-12',
    '2022-04-18',
    '2022-04-17',
    '2022-04-19',
    '2022-04-21'
  ),
  eliminated=c(
    'Luciano',
    'Rodrigo',
    'Maria',
    'Naiara',
    'Bárbara',
    'Brunna',
    'Tiago',
    'Larissa',
    'Jade',
    'Vinicius',
    'Lais',
    'Lucas',
    'Eslovênia',
    'Linn',
    'Natalia',
    'Jessilane',
    'Eliezer',
    'Gustavo',
    'Pedro'
  ),
  reason=c(
    'Eliminated',
    'Eliminated',
    'Out by aggression',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Give up',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated',
    'Eliminated'
  )
)

redditEliminations <- reddit_dataset %>%
  filter(
    as.Date(as.POSIXct(created, origin='1970-01-01', tz='UTC')) %in% as.Date(daysWithEliminations$dayOfElimination)
  ) %>% 
  filter(
    title == 'DISCUSSÃO DIÁRIA - BBB22'
  ) %>% 
  group_split(created)

for (i in redditEliminations) {
  createBar(
    get_nrc_sentiment(i$comment, lang = "portuguese"),
    sprintf('%s - %s', i$title[1], as.Date(as.POSIXct(i$created[1], origin='1970-01-01', tz='UTC'))),
    pathByEliminations
  )
}

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

createBar(analyzeJade, 'Jade', pathByBrother)
createBar(analyzeArthur, 'Arthur', pathByBrother)
createBar(analyzeTiago, 'Tiago', pathByBrother)
createBar(analyzeNaiara, 'Naiara', pathByBrother)
createBar(analyzeLinn, 'Linn', pathByBrother)
createBar(analyzeMaria, 'Maria', pathByBrother)
createBar(analyzeDouglas, 'Douglas', pathByBrother)
createBar(analyzePaulo, 'Paulo', pathByBrother)
createBar(analyzeBrunna, 'Brunna', pathByBrother)
createBar(analyzePedro, 'Pedro', pathByBrother)
createBar(analyzeLais, 'Láis', pathByBrother)
createBar(analyzeLuciano, 'Luciano', pathByBrother)
createBar(analyzeJessilane, 'Jessilane', pathByBrother)
createBar(analyzeEliezer, 'Eliezer', pathByBrother)
createBar(analyzeEslovenia, 'Eslovênia', pathByBrother)
createBar(analyzeLucas, 'Lucas', pathByBrother)
createBar(analyzeBarbara, 'Bárbara', pathByBrother)
createBar(analyzeRodrigo, 'Rodrigo', pathByBrother)
createBar(analyzeVinicius, 'Vinicius', pathByBrother)
createBar(analyzeNatalia, 'Natalia', pathByBrother)
createBar(analyzeGustavo, 'Gustavo', pathByBrother)
createBar(analyzeLarissa, 'Larissa', pathByBrother)