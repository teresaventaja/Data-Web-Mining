#######################################################################################################
########################################### Package ###################################################
#######################################################################################################

if (!require("twitteR")) install.packages("twitteR", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("tidytext")) install.packages("tidytext")
if (!require("textdata")) install.packages("textdata")
if (!require("rtweet")) install.packages("rtweet")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("multcomp")) install.packages("multcomp")
if (!require("party")) install.packages("party", dependencies = TRUE)
if (!require("openssl")) install.packages("openssl")
if (!require("httpuv")) install.packages("httpuv")
if (!require("base64enc")) install.packages("base64enc")
if (!require("httr")) install.packages("httr")
if (!require("tm")) install.packages("tm", dependencies = TRUE)
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("topicmodels")) install.packages("topicmodels")
if (!require("SnowballC")) install.packages("SnowballC", dependencies = TRUE)


library(rtweet)
library(twitteR)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(tidyverse)
library(multcomp)
library(party)
library(openssl)
library(httpuv)
library(base64enc)
library(httr)
library(tm)
library(wordcloud)
library(topicmodels)
library(SnowballC)
library(openssl)
library(httpuv)

#######################################################################################################
############################################ Data #####################################################
#######################################################################################################

## Scrape data from Twitter
# Set up access consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "tTqM3fy6MOj0AANRx3v9K796L"
consumer_secret <- "cPoTgwmc11n5xwRMLXSMZzruDobMq38raCfhysy6GrwqmfLUbp"
access_token <- "1290278060013629440-lYJ8SiuMRPGxSjYU2x3Pbsis2P6Kqz"
access_secret <- "ut9QHCLDNzJFJ4D1pX9PuMwJVqUALZyNoE6YQDYi8MKCc"

setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)

# Scrape data pertaining to tweets with #TikTok and in english
tweetsTT <- searchTwitter("#TikTok", n=1000, lang="en")

# Convert tweets to DF
tweets.df <- twListToDF(tweetsTT)

# Create copy of working data [Sentiment Analysis Requirement]
tweets <- tweets.df %>% dplyr::select(screenName, text)

#######################################################################################################
#################################### Data Exploration##################################################
#######################################################################################################

#look at first 5 rows
head(tweets.df,5)

#See frequent words
tweet_words <- tweets.df %>% dplyr::select(id, text) %>% unnest_tokens(word,text)

#visually check words
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

## create a list of stop words not worth including
myStopwords <- stop_words %>% dplyr::select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "out","the", "to", "on", "a", "this", "you", "of", "his", "and", "i", "be")))

tweet_words_interesting <- tweet_words %>% anti_join(myStopwords)

test_graph <- tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% ggplot(aes(x = reorder
                                                                                               (word,n,function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")
test_graph

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweet_words_interesting$word))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# count word frequence
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=20)
df <- data.frame(term = names(term.freq), freq = term.freq)

#Visualize

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#######################################################################################################
####################################### Rank Queries ##################################################
#######################################################################################################

##Identifying working directory
getwd()

##Selecting only 1st column (tweets text), and 1st 200 tweets

## tweets.df is the data we fetched from Twitter
## For this section we are going to use 200 tweets to build the corpus

tweetstext <- tweets.df[1:200, 1]
tweetstext <- as.data.frame(tweetstext)

## I am going to save it as a .csv document to explore the data

write.csv(tweetstext, "200tweets.csv", row.names = F)

##setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)

## Step 1 - Build our corpus of documents 

doc1 <- "Do you guys remember when #TikTok was called #Vine? 
They are the fucking same!!"
doc2 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc3 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc4 <- "A US WeChat ban could hurt many in America, not just China
#China
#UnitedStates
#DOJ
#Huawei 
#TikTok 
#WeChat. https://t.co/d4inc6eysQ"
doc5 <- "Check out SomePoliticalDude's video! #TikTok https://t.co/iZjb9Nc5fH"
doc6 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc7 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc8 <- "Check out Becks's video! #TikTok https://t.co/OUZS91luWT"
doc9 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc10 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc11 <- "RT @d_Yhanne25: Guys makiduet na kayo kay @Espanto2001! This is your chance! <U+0001F49A><U+0001F49A><U+0001F49A>
#DarrenEspanto 

Check out Darren Espanto's video! #TikTok."
doc12 <- "RT @1vZ08: Uniqlo Jeans Challenge by Yaya&amp;BoyPakorn

#Repost @cheoberu 
Jeed and Lom. But where's Fai? <U+0001F60A><U+0001F602>
Reposted from @yaai2020 #UniqloJe."
doc13 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc14 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc15 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc16 <- "RT @CRAVITYstarship: [TikTok]
The 100th-day celebration
for LUVITY is right over the door. 
Would you like to pass this hallway
to join our."
doc17 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc18 <- "pushmypurple_ #thickchallenge <U+0001F525><U+0001F929><U+0001F351> Ladies submit your challenge today! <U+0001F525> Search / use the sound Malki Means King -. https://t.co/gSqIPG4Bjg"
doc19 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc20 <- "RT @ATM19301420: Check out <U+CF54><U+C2A4><U+BAA8><U+D3F4><U+B9AC><U+D0C4>'s video! #TikTok

<U+2066>@OfficialMonstaX<U+2069> Shownu arrived to inflame TikTok! Please show the recognition and ap."
doc21 <- "@realDonaldTrump @cnn Pls help American teachers keep their jobs! Thousands of us at #GOGOkid will become unemploye. https://t.co/lNd311wcj0"
doc22 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc23 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc24 <- "Check out brenda's video! #TikTok <U+2066>@UN<U+2069>  https://t.co/mEMglVFViz"
doc25 <- "RT @LassyMay: Of course #DumpTrump would have nothing to do 
with #Triller financially would he ???
Whenever #CoronavirusTrump seems passio."
doc26 <- "RT @tw318_: Jackson teaching zhong han liang playing skateboard. Such a warm guys <U+2764><U+FE0F> they were talking cantonese for communicate. #JacksonW."
doc27 <- "RT @NoJudgementsOT7: <U+0001F9E8>Eating up all the haters<U+0001F9E8>
    Check out BTS' Chickens7's video! 
@BTS_twt #dynamitevideoteaser #dynamitemvteaser #BTSArmy."
doc28 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc29 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc30 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc31 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc32 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc33 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc34 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc35 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc36 <- "Look at these adorable kittens <U+0001F970>Check out my Youtube video on how we found them in the woods <U+0001F332><U+0001F62F>#WorldPhotoDay. https://t.co/p0aXm90pvJ"
doc37 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc38 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc39 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>
    
    <U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc40 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc41 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc42 <- "Check out AyhTee's video! #TikTok https://t.co/88J6TpTtsL"
doc43 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc44 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc45 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc46 <- "You gotta love @kimcattrall as #SamanthaJones on #SexAndTheCity!!! She was awesome! 
.
.
#DanyolJaye #OnTheJayeSpot. https://t.co/2GNIbjLuxZ"
doc47 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc48 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc49 <- "Check out lizzard 's video! #TikTok https://t.co/oTSDVtzVTd"
doc50 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc51 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc52 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc53 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc54 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc55 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc56 <- "RT @JL_Mico: Smartphones: https://t.co/VlYyxt9Tz8 
#Industry40 #AI #DataScience #MachineLearning #Analytics #Marketing #Business #TV #Video."
doc57 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc58 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc59 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc60 <- "RT @ckoever: We are publishing new inside information on #TikTok: Whistleblowers with insight into moderation say, TikTok failed to bar use."
doc61 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc62 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc63 <- "RT @JL_Mico: Smartphones: https://t.co/VlYyxt9Tz8 
#Industry40 #AI #DataScience #MachineLearning #Analytics #Marketing #Business #TV #Video."
doc64 <- "Smartphones: https://t.co/VlYyxt9Tz8 
#Industry40 #AI #DataScience #MachineLearning #Analytics #Marketing #Business. https://t.co/DmcM9cfTm6"
doc65 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc66 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc67 <- "<U+0794><U+07A6><U+0787><U+07AA><U+078E><U+07AA><U+0784><U+07AB>! 
Check out nasy's video! #TikTok https://t.co/LKVo7EkoWv"
doc68 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc69 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc70 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc71 <- "#girlslivesmatter #tiktok SHARE THIS  https://t.co/FgakrdpPXH"
doc72 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc73 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc74 <- "Check out isabellastricklandd's video! #TikTok https://t.co/MPyAaj4HG3"
doc75 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc76 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc77 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc78 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc79 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc80 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc81 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc82 <- "RT @Nicol00456165: Good morning dear friends. <U+0001F31E><U+0001F379><U+0001F352> Have an unforgetable day <U+0001F33A><U+0001F338><U+0001F33A><U+0001F33C><U+0001F490><U+0001F33C> &amp; remember that THE TRUE SECRET OF HAPPINESS LIES IN TAKI."
doc83 <- "RT @RT_com: 'The to-ing and fro-ing by the #UK government about whether or not to block #TikTok from building a headquarters here is as irr."
doc84 <- "Check out Michael Schiumo's video! #TikTok #BlackLivesMatter  https://t.co/MyuqRxPWkX"
doc85 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc86 <- "RT @hemrocktweets: Colour me With love! We all make a difference #WednesdayThoughts #WednesdayWisdom #WednesdayMotivation #wedomorewednesda."
doc87 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc88 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc89 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc90 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc91 <- "RT @LassyMay: Of course #DumpTrump would have nothing to do 
with #Triller financially would he ???
Whenever #CoronavirusTrump seems passio."
doc92 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc93 <- "RT @Nicol00456165: Good morning dear friends. <U+0001F31E><U+0001F379><U+0001F352> Have an unforgetable day <U+0001F33A><U+0001F338><U+0001F33A><U+0001F33C><U+0001F490><U+0001F33C> &amp; remember that THE TRUE SECRET OF HAPPINESS LIES IN TAKI."
doc94 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc95 <- "Colour me With love! We all make a difference #WednesdayThoughts #WednesdayWisdom #WednesdayMotivation. https://t.co/MaBkvVzBCQ"
doc96 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc97 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc98 <- "Check out Hannah Snyder's video! #TikTok https://t.co/WhUzmKcxEj"
doc99 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc100 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc101 <- "RT @AllesUndNix_: RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT. https://t.co/qVhvYtjDoI"
doc102 <- "RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT. https://t.co/YIoEPnSZHy"
doc103 <- "RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT. https://t.co/94YyRtMGeM"
doc104 <- "RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT @AllesUndNix_: RT. https://t.co/Y4mF7QLbgB"
doc105 <- "RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @AllesUndNix_: RT @ikeelliswill: RT. https://t.co/UxIhIOy0Hi"
doc106 <- "RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT @ikeelliswill: RT. https://t.co/hycww95N5L"
doc107 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc108 <- "Check out S.P.'s video! #TikTok https://t.co/PjMsRb56c0"
doc109 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc110 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc111 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc112 <- "Check out Jamie Nodoubt's video! #TikTok <U+0001F917> #jamienodoubt #poetry #fyp #foryou #foryoupage #4 #4u #4upage #foru. https://t.co/mn7XEqYTTe"
doc113 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc114 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc115 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc116 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc117 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc118 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc119 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc120 <- "#DonaldTrump Believes #Oracle Can Handle Chinese App #TikTok Deal.

https://t.co/Uo6GyhS6RN"
doc121 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc122 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc123 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc124 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc125 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc126 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc127 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc128 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc129 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc130 <- "I tried.. Btw I'm loyal Chin Stan (.<U+3160>~<U+3160>)

Check out DanityKingdom's video! #TikTok https://t.co/zvzKUUgvbv

==
[Fol. https://t.co/wDrPMATpNa"
doc131 <- "Check out JT Laybourne's video! #TikTok https://t.co/DqFPbTVnyl"
doc132 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc133 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc134 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc135 <- "RT @LassyMay: Of course #DumpTrump would have nothing to do 
with #Triller financially would he ???
Whenever #CoronavirusTrump seems passio."
doc136 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc137 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc138 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc139 <-"Check out Edward's video! #TikTok https://t.co/jqlFBlZDdI"
doc140 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc141 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc142 <- "RT @TSNXtra: Thanks to #TikTok, a 2008 #TaylorSwift single Love Story is 2020's song of the summer <U+0001F4C0><U+0001F399><U+0001F4C0> https://t.co/NaXZwsHpx3"
doc143 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc144 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc145 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc146 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc147 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc148 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc149 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc150 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc151 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc152 <- "Check out Jimmy Levy's video! #TikTok typical democratic liberals trying to tell this guy to be quiet. The Clinton'. https://t.co/Qh6ZOlt4Kf"
doc153 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc154 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc155 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc156 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc157 <- "Check out Carry Christ's video! #TikTok  https://t.co/meK84s3ukX"
doc158 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc159 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc160 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc161 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc162 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc163 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc164 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc165 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc166 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc167 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc168 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw" 
doc169 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc170 <- "Check out The Cole Twins's video! #TikTok https://t.co/vp1w6ko1Ob"
doc171 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc172 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc173 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc174 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc175 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc176 <- "Thanks to its easily consumable and frighteningly addictive video clips, #TikTok has proved a boon for. https://t.co/gufu1gGBLJ"
doc177 <- "<U+0001F525> Hot off the press: Apple Boosts #Streaming #Radio; Universal Inks Deal with Soundtrack Your Brand; #TikTok Worki. https://t.co/wti9qEZXOx"
doc178 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc179 <- "RT @Dmaylife1: Peep Merch
#PeepMerch
#Like
#Comment
#Share
#dmaylife 
#bopkingmeechie
#meechiethegreat
#tiktok 
#explore
#modellife 
#insta."
doc180 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc181 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc182 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc183 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc184 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc185 <- "RT @LassyMay: Of course #DumpTrump would have nothing to do 
with #Triller financially would he ???
Whenever #CoronavirusTrump seems passio."
doc186 <- "Whoa freedom lovin #Belarusprotests have Balls <U+0001F3C0> <U+0001F3C0> literally - Look <U+0001F440> 
#boston #resist #sports #london #moscow. https://t.co/ww7CqBIzFL"
doc187 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc188 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc189 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc190 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc191 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc192 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc193 <- "Check out Dan Napolitano's video! #TikTok https://t.co/DNQvr6hvxN"
doc194 <- "RT @joonsdisciple_: All 144p of me are vibin' #DynamiteVideoTeaser #BTS_Dynamite #tiktok https://t.co/JR3FntsPpw"
doc195 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc196 <- "RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc197 <-"Check out Cath.'s video! #TikTok https://t.co/0FJXk2v0H2"
doc198 <-"RT @DKB_BRAVE: We Ride<U+0001F699><U+0001F495>

<U+0001F517> https://t.co/a4wSrEbzMv

#<U+C6B4><U+C804><U+B9CC><U+D574>_Challenge
#WeRide_Challenge
#<U+BE0C><U+B808><U+C774><U+BE0C><U+AC78><U+C2A4> #BraveGirls
#<U+C6B4><U+C804><U+B9CC><U+D574> #WeRide
#<U+B2E4><U+D06C><U+BE44> #DKB
#<U+C774><U+CC2C> #E_C."
doc199 <- "Check out Shin Dong! #TikTok shin got tiktok <U+0001F602> https://t.co/YLk32rcz1l"
doc200 <-   "RT @tw318_: Jackson teaching zhong han liang playing skateboard. Such a warm guys <U+2764><U+FE0F> they were talking cantonese for communicate. #JacksonW."



##

doc.list <- list(
  doc1, doc2, doc3, doc4, doc5, doc6, doc7, doc8, doc9, doc10,
  doc11, doc12, doc13, doc14, doc15, doc16, doc17, doc18, doc19, doc20,
  doc21, doc22, doc23, doc24, doc25, doc26, doc27, doc28, doc29, doc30,
  doc31, doc32, doc33, doc34, doc35, doc36, doc37, doc38, doc39, doc40,
  doc41, doc42, doc43, doc44, doc45, doc46, doc47, doc48, doc49, doc50,
  doc51, doc52, doc53, doc54, doc55, doc56, doc57, doc58, doc59, doc60,
  doc61, doc62, doc63, doc64, doc65, doc66, doc67, doc68, doc69, doc70,
  doc71, doc72, doc73, doc74, doc75, doc76, doc77, doc78, doc79, doc80,
  doc81, doc82, doc83, doc84, doc85, doc86, doc87, doc88, doc89, doc90,
  doc91, doc92, doc93, doc94, doc95, doc96, doc97, doc98, doc99, doc100,
  doc101, doc102, doc103, doc104, doc105, doc106, doc107, doc108, doc109, doc110,
  doc111, doc112, doc113, doc114, doc115, doc116, doc117, doc118, doc119, doc120,
  doc121, doc122, doc123, doc124, doc125, doc126, doc127, doc128, doc129, doc130,
  doc131, doc132, doc133, doc134, doc135, doc136, doc137, doc138, doc139, doc140,
  doc141, doc142, doc143, doc144, doc145, doc146, doc147, doc148, doc149, doc150,
  doc151, doc152, doc153, doc154, doc155, doc156, doc157, doc158, doc159, doc160,
  doc161, doc162, doc163, doc164, doc165, doc166, doc167, doc168, doc169, doc170,
  doc171, doc172, doc173, doc174, doc175, doc176, doc177, doc178, doc179, doc180,
  doc181, doc182, doc183, doc184, doc185, doc186, doc187, doc188, doc189, doc190,
  doc191, doc192, doc193, doc194, doc195, doc196, doc197, doc198, doc199, doc200
)

## Getting the length of the list (200)

N.doc <- length(doc.list)

## Naming each element of the list, so instead of "[[1]]" (example) they are named "$doc1"

names(doc.list) <- paste0("doc", c(1:N.doc))


##
doc.list


### QUERY 1 


##  Step 2 - Selecting the query 
## In this case we are going to check on BlackLivesMatter and girlslivesmatter, since the topic is trendy

query1 <- "BlackLivesMatter girlslivesmatter"

## Step 3 - Including the query into the list of documents we created

my.docs <- VectorSource(c(doc.list, query1))

##Naming the list element we included "query1"

my.docs$Names <- c(names(doc.list), "query1")

##Creating the Corpus

my.corpus <- Corpus(my.docs)
my.corpus


## Step 4 - Standardize the documents
getTransformations()
#Lists what transformations we can use

#removePumctuation
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus$content

#stemDocument
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus$content

##Remove Numbers, convert to lowercase and remove space
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$content


## Step 5 - Covert the corpus into term document matrix 

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:10, ])

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

##Concatenate and print function - returns metadata about the size of our matrix

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.")


## Step 6 - Choice & Implementation

## Creating a function that compute weights for each term frequency

get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.doc/df)
  weight
}

## Testing the function

cat("A word appearing in 4 of 6 documents, occuring 1, 2, 3, and 6 times, respectively: \n", 
    get.tf.idf.weights(c(1, 2, 3, 0, 0, 6), 4))


## Step 7 - Assigning frequency % to each word 

## Creating a function that will assign weights to each word
## It will return the weight of each word in each document, i.e.
## the word and how frequently it appears

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.doc] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]

## Step 8 - Normalise the tfidf matrix based on the square root of the column sums

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))

tfidf.matrix[0:3, ]


## Step 9 - Separate the query from the rest of the data

query.vector <- tfidf.matrix[, (N.doc + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.doc]


## Step 10 - Comparing query words with our matrix (words ranked by frequency)

doc.scores <- t(query.vector) %*% tfidf.matrix


## Step 11 - Creating a data frame including the list of scores with their corresponding document

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))

## Organising the results in decreasing order
## That way the documents with the best similarity will appear on the top

results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

## Formatting the results

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

## Step 12 - Saving the result for query 1

resultsquery1 <- results.df
resultsquery1

# 2 observations with similarity score greater than 0 (1% of the dataset)

### QUERY 2
## In this case I am not going to comment step by step
## the process to avoid repetition
## They are the same steps taken on Query 1, but with a different query

doc.list

query2 <- "American CoronavirusTrump"

my.docs <- VectorSource(c(doc.list, query2))
my.docs$Names <- c(names(doc.list), "query2")

my.corpus <- Corpus(my.docs)
my.corpus

## Standardising data

my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus$content
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus$content

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$content


## Term document matrix

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:10, ])

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.")


## Applying Functions

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))

tfidf.matrix[0:3, ]


## Comparing the query with the term matrix

query.vector <- tfidf.matrix[, (N.doc + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.doc]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

## Saving the results

resultsquery2 <- results.df

# 5 results with similarity greater than 0 (2.5% of the dataset)


###QUERY 3 

## In this case I am not going to comment step by step
## the process to avoid repetition
## They are the same steps taken on Query 1, but with a different query

doc.list

query3 <- "DynamiteVideoTeaser WeRideChallenge"

my.docs <- VectorSource(c(doc.list, query3))
my.docs$Names <- c(names(doc.list), "query3")

my.corpus <- Corpus(my.docs)
my.corpus


## Standardising data

my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus$content
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus$content

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$content


## Term document matrix

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:10, ])

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.")


## Applying Functions

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))

tfidf.matrix[0:3, ]


## Comparing the query with the term matrix

query.vector <- tfidf.matrix[, (N.doc + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.doc]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

## Saving the results

resultsquery3 <- results.df

## Analysis of results

remove_0_results <- subset(resultsquery3, score>0.072,
                           select=doc:score:text)
#142 observations (71% of the dataset) are viral content

dynamite <- subset(remove_0_results, score>0.30 & score<0.072,
                   select=doc:text)
#52 observations for dynamite viral content (26% of the dataset)

we_ride <- subset(remove_0_results, score<0.30 & score>0.13,
                  select=doc:text)
#90 observations for "We Ride" viral content (45% of the dataset)

#######################################################################################################
####################################### Sentiment Analysis ############################################
#######################################################################################################

## Process & Clean text using transformers
# Remove http elements
tweets$stripped_text1 <- gsub("https\\S+","",tweets$text)

# Use unnest_tokens() function to convert to lowercase, remove punctuation and add tweet ID
tweets.stem <- tweets %>% dplyr::select(stripped_text1) %>% unnest_tokens(word, stripped_text1)

# Remove stop words from the list
cleaned_tweets <- tweets.stem %>% anti_join(stop_words)

myStopwords <- stop_words %>% dplyr::select(-lexicon) %>% 
  bind_rows(data.frame(word = c("t.co", "rt", "out","the", "to", "on", "a", "this", "you", "of", "his", 
                                "and", "i", "be")))
cleaned_tweets <- cleaned_tweets %>% anti_join(myStopwords)

## Check Bing Lexicon
# Bing Sentiment
# Positive
get_sentiments("bing") %>% filter(sentiment=="positive")
# Negative
get_sentiments("bing") %>% filter(sentiment=="negative")

## Bing Sentiment Analysis
# score and combine the cleaned tweets with the bing lexicon
bing_tweets <- cleaned_tweets %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

# Visualise Bing Sentiment
bing_tweets %>% group_by(sentiment) %>% top_n(15) %>% ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing #TikTok", y = "Contribution to Sentiment", x = NULL) + coord_flip() + theme_bw()

# Get sentiment score for each tweet
# Build sentiment calculator function for the bing lexicon
sentiment_bing = function(twt){
  #Perform basic text cleaning & strip https elements
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    unnest_tokens(word, stripped_text)%>%
    anti_join(stop_words) %>% 
    anti_join(myStopwords) %>%    
    inner_join(get_sentiments("bing")) %>% #remove stop words & merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% #next create a score column, -1 for negative, 1 for positive
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment =='positve'~ n*1)
    )
  #Calculate Total Score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there are no words, zero score
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise sum pos & neg score
  )
  #This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1",
    nrow(twt_tbl)>0~"Type 2"
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

# Apply bing sentiment calculator
tweet_sent <- lapply(tweets$text, function(x){sentiment_bing(x)})

#Results
 # Tibble to specify topic, score and type
score_breakdown <- tibble(topic = '#TikTok', score = unlist(map(tweet_sent, 'score')), type = unlist(map(tweet_sent, 'type')))

 # Visualise overall sentiment towards TikTok 
ggplot(score_breakdown, aes(x=score, fill = topic)) + geom_histogram(bins = 15, alpha = 0.6) + 
  facet_grid(~topic) + theme_bw()

#######################################################################################################
####################################### Topic Modelling ###############################################
#######################################################################################################

#We start from the corpus built in previous phases and we remove few other words
##remove the word tiktok from corpus
myCorpusLB=tm_map(myCorpus, removeWords, c("tiktok"))

#stem the document
myCorpusLB=tm_map(myCorpus, stemDocument)


#create document term matrix 
dtm1 <- TermDocumentMatrix(myCorpusLB, control = list(wordLengths = c(1, Inf)))
dtm1
inspect(dtm1)

#find frequency terms - duplication of part 1 code to check for updated corpus
freq.terms<-findFreqTerms(dtm1, lowfreq = 20)
termFreq<-rowSums(as.matrix(dtm1))
termFreq<-subset(termFreq, termFreq>=20)
df<-data.frame(term=names(termFreq), freq=termFreq)
df 

#find most frequent words
m<-as.matrix(dtm1)
word.freq<-sort(rowSums(m), decreasing=T)
word.freq

#create word cloud
pal1<-brewer.pal(9, "BuGn")[-(1:4)]
set.seed(1234)
wordcloud(words=names(word.freq), freq=word.freq,min.freq = 3, random.order = F, colors = pal1)

#include in the Document term matrix only documents with at least one non 0 entry
dtmLDA<-as.DocumentTermMatrix(dtm1)
doc.length = apply(dtmLDA, 1, sum)
dtmLDA = dtmLDA[doc.length > 0,]
dtmLDA


#create LDA
lda_2=LDA(dtmLDA, k = 2, method = 'Gibbs', 
          control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                         thin = 500, burnin = 4000, iter = 2000))
#LDA model with 5 topics selected

lda_5 = LDA(dtmLDA, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 3 topics selected
lda_3 = LDA(dtmLDA, k = 3, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 10 topics selected
lda_6 = LDA(dtmLDA, k = 6, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))
#LDA model with 4 topics selected
lda_4 = LDA(dtmLDA, k = 4, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))


#select top 10 most frequent words under each topic

top10terms_5 = as.matrix(terms(lda_5,10))
top10terms_3 = as.matrix(terms(lda_3,10))
top10terms_6 = as.matrix(terms(lda_6,10))
top10terms_4 = as.matrix(terms(lda_4,10))
top10terms_2 = as.matrix(terms(lda_2,10))


#display top 10 most frequent words by topic
top10terms_5
top10terms_3
top10terms_6
top10terms_4
top10terms_2

#search number of words belonging to each topic and display it
lda.topics_5 = as.matrix(topics(lda_5))
lda.topics_2 = as.matrix(topics(lda_2))
lda.topics_3=as.matrix(topics(lda_3))
#lda.topics_10 = as.matrix(topics(lda_10))
summary(as.factor(lda.topics_5[,1]))

summary(as.factor(lda.topics_3[,1]))

