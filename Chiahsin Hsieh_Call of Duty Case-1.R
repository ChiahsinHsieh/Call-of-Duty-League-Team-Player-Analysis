#' Title: Robust Sentiment Analysis
#' Purpose: Inner join sentiment lexicons to text
#' Author: Ted Kwartler
#' email: edward.kwartler@hult.edu
#' License: GPL>=3
#' Date: Dec 28 2020
#'
setwd("~/Desktop/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")

#Libraries
library(tm)
#library(qdap)
library(lexicon)
library(dplyr)
library(fst)
library(pbapply)
library(mgsub)
library(tidytext)
library(reshape2)
library(wordcloud)
library(viridisLite)
library(radarchart)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

#Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')


tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}


cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

#'Stops words
stops <- c(stopwords('SMART'), 'win', 'won','hate', 'like') 
#############################################################################
# Read in Data, clean & organize
text      <- read_fst('student_TeamTimelines.fst') 
txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Review a section
tweetFreq[50:55,]

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 500
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 500) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# Simple barplot; values greater than 1200
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 1200) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)


#############################################################################
setwd("~/Desktop/hult_NLP_student/cases/Call of Duty E-Sport/teamFollowerTimelines")

# Read in Data, clean & organize
optic     <- read_fst('student_2020-12-28_OpTicChI2_followers_timelines.fst') 
txtCorpus <- VCorpus(VectorSource(optic$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Review a section
tweetFreq[50:55,]

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 400
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 400) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# Simple barplot; values greater than 1100
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 1100) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

#############################################################################
setwd("~/Desktop/hult_NLP_student/cases/Call of Duty E-Sport/teamFollowerTimelines")

# Read in Data, clean & organize
LAGuerrillas      <- read_fst('student_2020-12-28_LAGuerrillas2_followers_timelines.fst') 
txtCorpus <- VCorpus(VectorSource(LAGuerrillas$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

# Frequency Data Frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

# Review a section
tweetFreq[50:55,]

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
tweetFreq[50:55,]

# Simple barplot; values greater than 400
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 400) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# Simple barplot; values greater than 1100
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 1100) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)
###############################################################################

OpTiciak <- read.fst("student_2020-12-28_OpTicCHI2_followers_timelines.fst", from = 1, to = 10000)

OpTiccorp <- OpTiciak  %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

OpTicing <- inner_join(OpTiccorp, bing) %>% 
  count(team = "OpTicCHI", index = linenumber %/% 50, sentiment, sort = TRUE) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(OpTicing, aes(index, sentiment, fill = team)) +
  geom_bar(stat = "identity", show.legend = TRUE, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  facet_wrap(~team, labeller = labeller(team = c("OpTicCHI" = "team: OpTiciak")),ncol = 2, scales = "free_x")+
  theme(text = element_text(size = 15))

###############################################################################

LAGuerrilliak <- read.fst("student_2020-12-28_LAGuerrillas2_followers_timelines.fst", from = 1, to = 10000)

LAGuerrillcorp <- LAGuerrilliak  %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

LAGuerrilling <- inner_join(LAGuerrillcorp, bing) %>% 
  count(team = "LAGuerrillas", index = linenumber %/% 50, sentiment, sort = TRUE) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(LAGuerrilling, aes(index, sentiment, fill = team)) +
  geom_bar(stat = "identity", show.legend = TRUE, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  facet_wrap(~team, labeller = labeller(team = c("LAGuerrillas" = "team: LAGuerrillas")),ncol = 2, scales = "free_x")+
  theme(text = element_text(size = 15))
