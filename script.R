## COVID 19 Sentiment analysis
#packages

library(dplyr)
library(ggplot2)
library(tidyr)
library(sjmisc)
library(psych)
library(MASS)
library(reshape2)
library(reshape)
library(ggpubr)
library(topicmodels)
library(tm)
library(tidytext)
library(dplyr)
library(SnowballC)
library(lubridate)
library(wordcloud2)
library(wordcloud)
library(syuzhet)
library(scales)

SICSS_Montreal <- read.csv("rehydrated_COVID_TweetIDs_MarchAprilMay_1Perc.csv",stringsAsFactors=FALSE) 

#Clean dataset: untoken the text

SICSS_Mont <- SICSS_Montreal %>%
  filter(lang=="en", grepl('Canada', user_location))%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  filter(!(word=="https"| 
             word=="rt"|
             word=="t.co"|
             word=="amp" | 
             word== "3"|
             word== "19"|
             word=="2"|
             word=="1"|
             word== "coronavirus"|
             word=="covid"|
             word=="covid19"|
             word=="it’s"|
             word=="i'm"))

SICSS_Mont %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

```

## Topic Modelling
```{r warning=FALSE}
#Version 1
# Load data
Tweets_topic <- read.csv("rehydrated_COVID_TweetIDs_MarchAprilMay_1Perc.csv",stringsAsFactors=FALSE)
# Pull the tweet text
Tweets_sec <- Tweets_topic %>% filter(lang=="en", grepl('Canada', user_location)) %>%
  pull(text) 
# Create a corpus of tweets for a Document-Term Matrix
corpus <- Corpus(VectorSource(Tweets_sec))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("rt", "https", "t.co", stopwords("english")))
corpus <- tm_map(corpus, stemDocument)
# Create the Document Term Matrix
DTM <- DocumentTermMatrix(corpus)
# OPTIONAL: we can delete the less frequent terms, for this, change "DTM" below for "sparse_DTM"
#frequent_ge_20 <- findFreqTerms(DTM, lowfreq = 20)
#sparse_DTM <- removeSparseTerms(DTM, 0.995)
# Group the terms by topic
# OPTIONAL: we can change the "k" variable velow to set the number of topics. For now it's set at 6
tweet_lda <- LDA(DTM, k = 4, control = list(seed = 1234))
# Extract the "per topic per word" probabilities of a topic ("beta")
tweet_topics <- tidy(tweet_lda, matrix = "beta")
# Select the top terms per topic
tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# Plot the different topics and the top words
tweet_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#Version 2
tidy_dtm <- SICSS_Mont %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(word) %>%
  cast_dtm(word, word, n)

tweet_topic_model<-LDA(tidy_dtm , k=4, control = list(seed = 321))

topics <- tidy(tweet_topic_model, matrix = "beta")

top_terms1 <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Sentiment analysis (Matt’s version)

Tweets_topic <- read.csv("rehydrated_COVID_TweetIDs_MarchAprilMay_1Perc.csv",stringsAsFactors=FALSE)

################
## Filter the tweets
################
library(plyr)
## Select only the English Canadian tweets
Tweets_filtered<- Tweets_topic %>% 
  filter(lang=="en", grepl("canada",
                           ignore.case = TRUE,
                           user_location))

## Select only tweets that mention Canada
Tweets_canada<- Tweets_filtered %>% 
  filter(grepl("canad.*",
               ignore.case = TRUE,
               text))

## Select only tweets that mention Trudeau
Tweets_trudeau<- Tweets_filtered %>% 
  filter(grepl("trudeau",
               ignore.case = TRUE,
               text))

## Join the datasets
Tweets_about_canada <- full_join(Tweets_canada,
                                 Tweets_trudeau)

## Rename dataset back to filtered (to work with legacy code below)
Tweets_filtered <- Tweets_about_canada

## Select only tweets that mention America, US, Trump 

################
## Clean the filtered tweets
################

## Create a new dataset based on the filtered tweets
Tweets_tidy<-Tweets_filtered %>%
  dplyr::select(created_at,text)%>%
  unnest_tokens("word", text)

#removing stop words
data("stop_words")
Tweets_tidy_rmstop <-Tweets_tidy %>%
  anti_join(stop_words)

## Remove numbers
Tweets_tidy_rmstop<-Tweets_tidy_rmstop[-grep("\\b\\d+\\b", Tweets_tidy_rmstop$word),]

## Remove white spaces
Tweets_tidy_rmstop$word <- gsub("\\s+","",Tweets_tidy_rmstop$word)

#Stemming
Tweets_tidy_rmstop %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

#removal of twitter-specific language
detach(package:plyr)
top_words<-
  Tweets_tidy_rmstop %>%
  #anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(word) %>%
  arrange(desc(n))

# top_words %>%
#   ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
#   geom_bar(stat="identity")+
#   theme_minimal()+
#   theme(axis.text.x =
#           element_text(angle = 60, hjust = 1, size=13))+
#   theme(plot.title =
#           element_text(hjust = 0.5, size=18))+
#   ylab("Frequency")+
#   xlab("")+
#   ggtitle("Most Frequent Words in Tweets")+
#   guides(fill=FALSE)+
#   coord_flip()

#sentiment analysis using dictionary

Tweet_sentiment <- top_words %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

## Plot by date
## Wrangle the date format
# Tweets_tidy_rmstop$date<-as.Date(Tweets_tidy_rmstop$created_at,
#                                  format="%b %d %x %x %Y")

Tweets_tidy_rmstop$createdClean<-paste(substr(Tweets_tidy_rmstop$created_at,5,10),"2020")

## Create new column with standard date format
## https://www.r-bloggers.com/date-formats-in-r/

Tweets_tidy_rmstop$date<-as.Date(Tweets_tidy_rmstop$createdClean, 
                                 format="%b %d %Y")


## Plot negative sentiment by date

# Tweet_sentiment_plot <-
#   Tweets_tidy_rmstop %>%
#   inner_join(get_sentiments("bing")) %>% 
#   filter(sentiment=="negative") %>%
#   count(date, sentiment)

## Get a count of how many tweets per day

words_perDay <-
  Tweets_tidy_rmstop %>%
  dplyr::count(date)

words_perDay <-
  words_perDay %>%
  dplyr::rename("n_words" = n)

## Aggregate positive and negative sentiment by tweet
# Aggregate by negative sentiment
sentiment_negative <-
  Tweets_tidy_rmstop %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)
# Rename the new column
sentiment_negative <-
  sentiment_negative %>%
  dplyr::rename(n_negative = n)

## Aggregate by positive sentiment
## Aggregate positive and negative sentiment by tweet
sentiment_positive <-
  Tweets_tidy_rmstop %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="positive") %>%
  count(date, sentiment)
sentiment_positive <-
  sentiment_positive %>%
  dplyr::rename(n_positive = n)

## Join the two datasets to get both positive and negative sentiment, and words per day
sentiment_both <- full_join(sentiment_negative, sentiment_positive, by="date")
sentiment_both <- full_join(sentiment_both, words_perDay, by="date")

## Replace missing values with 0
sentiment_both$n_negative <- ifelse(is.na(sentiment_both$n_negative),0,sentiment_both$n_negative)
sentiment_both$n_positive <- ifelse(is.na(sentiment_both$n_positive),0,sentiment_both$n_positive)

## Create some derived variables
sentiment_both$ratioPosNeg = sentiment_both$n_positive/sentiment_both$n_negative
sentiment_both$ratioPosPerWord = sentiment_both$n_positive/sentiment_both$n_words
sentiment_both$ratioNegPerWord = sentiment_both$n_negative/sentiment_both$n_words

## Create a new variable that keeps track of the week
sentiment_both$week <- week(sentiment_both$date)

## Summarize the tweets by week
sentiment_byWeek <- sentiment_both %>%
  group_by(week) %>%
  summarize(n_positive = sum(n_positive),
            n_negative = sum(n_negative),
            n_words = sum(n_words))

## Create some derived variables
sentiment_byWeek$ratioPosNeg = sentiment_byWeek$n_positive/sentiment_byWeek$n_negative
sentiment_byWeek$ratioPosPerWord = sentiment_byWeek$n_positive/sentiment_byWeek$n_words
sentiment_byWeek$ratioNegPerWord = sentiment_byWeek$n_negative/sentiment_byWeek$n_words

##################
## Save the datasets
##################

# write.csv(sentiment_byWeek,
#           "~/Documents/GitHub/SICSS_2020/sentiment_aboutCanada_byWeek.csv")
```

```{r warning=FALSE}
##################
## Plot the results
##################

sentiment_byWeek <- readr::read_csv("sentiment_aboutCanada_byWeek.csv")

## Number of words
ggplot(sentiment_both, aes(x=date)) +
  geom_line(aes(y = n_negative), color = "red") +
  geom_line(aes(y = n_positive), color="blue") +
  # geom_line(aes(y = n_words), color="black") +
  
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

## Plot the results
## Ratio of positive / negative sentiment per word
ggplot(sentiment_both, aes(x=date)) +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "2 day") +
  #scale_x_date(date_minor_breaks = "2 day") +
  #geom_line(aes(y = ratioNegPerWord), color = "red") +
  #geom_line(aes(y = ratioPosPerWord), color="blue") +
  geom_line(aes(y = ratioPosNeg), color="black") +
  
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Ratio of Positive/Negative Sentiment per total Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

# ggplot(Tweet_sentiment_plot, aes(x=date, y=n))+
#   geom_line(color="red", size=.5)+
#   theme_minimal()+
#   theme(axis.text.x = 
#           element_text(angle = 60, hjust = 1, size=13))+
#   theme(plot.title = 
#           element_text(hjust = 0.5, size=18))+
#   ylab("Number of Negative Words")+
#   xlab("")+
#   ggtitle("Negative Sentiment in Tweets")+
#   theme(aspect.ratio=1/4)

survey_weekly <- readr::read_csv("SURVEY_LABOR_WEEKLY.csv")

## Convert the start date column to a date
survey_weekly$date <- as.Date(survey_weekly$Survey_Date_Start)

## Calculate the % satisfied with measures overall
survey_weekly$satMeas_overall <- survey_weekly$SATMEAS_SS + survey_weekly$SATMEAS_VS

## Calculate the % fearful overall
survey_weekly$fear_overall <- survey_weekly$FEAR_V + survey_weekly$FEAR_SW

## Note: $FEAR_AL = already contracted the virus

## Drop the first row
survey_weekly <- survey_weekly[-c(1), ]

# Add in the correct country data
survey_weekly$Country <- "Canada"

## Wrangle the dates into weeks
survey_weekly_tidy <- survey_weekly 
survey_weekly_tidy$week <- week(survey_weekly_tidy$date)

##################
## Correlations
##################

## Join the sentiment and survey data by week
sentiment_with_survey <- full_join(sentiment_byWeek, survey_weekly_tidy, by="week")


## See if there is a correlation between sentiment and % satisfied
cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$satMeas_overall, 
         method = "spearman"
)

## See if there is a correlation between positive sentiment and sat overall
cor.test(sentiment_with_survey$ratioPosPerWord, 
         sentiment_with_survey$satMeas_overall, 
         method = "spearman"
)

## See if there is a correlation between negative sentiment and sat overall
cor.test(sentiment_with_survey$ratioNegPerWord, 
         sentiment_with_survey$satMeas_overall, 
         method = "spearman"
)



## See if there is a correlation between positive sentiment and % fearful
cor.test(sentiment_with_survey$ratioPosPerWord, 
         sentiment_with_survey$fear_overall, 
         method = "spearman"
)

library(readxl)
SURVEY_LABOR_CAN <- read_excel("SURVEY_LABOR_CAN.xlsx", 
                               sheet = "Feuil1")
#View(SURVEY_LABOR_CAN)
sentiment_with_survey <- full_join(sentiment_byWeek, SURVEY_LABOR_CAN, by="week")

## See if there is a correlation tweets sentiment and Proportion of respondents that already exposed to covid

cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$FEAR_AL, 
         method = "spearman"
)

## See if there is a correlation tweets sentiment and Proportion that think we are in the worst of the pandemic now


cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$EVL_WN, 
         method = "spearman"
)

## See if there is a correlation tweets sentiment and Proportion that approve provincial measures


cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$SATMEAS_PR, 
         method = "spearman"
)

## See if there is a correlation tweets sentiment and Proportion that do not commit to measures


cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$NCOMM, 
         method = "spearman"
)

## See if there is a correlation tweets sentiment and Proportion that think the threat is blown out of proportion


cor.test(sentiment_with_survey$ratioPosNeg, 
         sentiment_with_survey$THR_OP, 
         method = "spearman"
)



## Plot the results
## By week
## Ratio of positive / negative sentiment per word
ggplot(sentiment_with_survey, aes(x=week)) +
  #scale_x_date(date_breaks = "1 week", date_minor_breaks = "2 day") +
  geom_line(aes(y = ratioPosNeg), color="black") +
  geom_line(aes(y = SATMEAS_S), color="blue") +
  geom_line(aes(y = FEAR_NS), color="red") +
  #geom_line(aes(y = FEAR_AL), color="green")
  #geom_line(aes(y = EVL_WN), color="green")
  #geom_line(aes(y = SATMEAS_PR), color="green")
  #geom_line(aes(y = NCOMM), color="green")
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Ratio of Positive/Negative Sentiment per total Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

#########
## Read the coronavirus data
########

library(readxl)
covid_canada <- read_excel("Public_COVID-19_Canada.xlsx", skip = 3)

######
## Optionally read the other datasets (if not already in memory)
######

#sentiment_both <- read_csv("~/Documents/GitHub/SICSS_2020/sentiment_aboutCanada_byDay.csv")
#sentiment_byWeek <- read_csv("~/Documents/GitHub/SICSS_2020/sentiment_aboutCanada_byWeek.csv")
#survey_weekly <- read_csv("~/Documents/GitHub/SICSS_2020/survey_weekly_tidy_canada")

##########
## Wrangle dates
#########

covid_canada$date <- as.Date(covid_canada$date_report)
covid_canada$week <- week(covid_canada$date)

## Create dataframe of cases per day
casesPerDay <-
  covid_canada %>%
  count(date)
casesPerDay <-
  casesPerDay %>%
  dplyr::rename(n_cases = n)

## Join the sentiment data per day with the casesPerDay dataframe
sentiment_both <- full_join(sentiment_both, casesPerDay, by="date")

## scale the cases per day such that it is between 0 and 1 (/max)
#sentiment_both$casesScaled <- sentiment_both$n_cases/max(sentiment_both$n_cases,na.rm = TRUE)

## Plot the result
## Number of words and cases
#ggplot(sentiment_both, aes(x=date)) +
# geom_line(aes(y = n_negative), color = "red") +
#geom_line(aes(y = n_positive), color="blue") +
# geom_line(aes(y = n_words), color="black")

# theme_minimal()+
#theme(axis.text.x = 
#       element_text(angle = 60, hjust = 1, size=13))+
#theme(plot.title = 
#       element_text(hjust = 0.5, size=18))+
#ylab("Number of Words")+
#xlab("")+
#ggtitle("Sentiment in Tweets")+
#theme(aspect.ratio=1/4)

## Ratio of positivity and scaled cases
ggplot(sentiment_both, aes(x=date)) +
  # geom_line(aes(y = n_negative), color = "red") +
  # geom_line(aes(y = n_positive), color="blue") +
  geom_line(aes(y = ratioPosNeg), color="black") +
  geom_line(aes(y = casesScaled), color = "green") +
  
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

## See if there is a correlation between sickness and positivity
cor.test(sentiment_both$ratioPosNeg,sentiment_both$casesScaled, method = "spearman")

############  
## Create dataframe of cases per week
############

casesPerWeek <-
  covid_canada %>%
  count(week)
casesPerWeek <-
  casesPerWeek %>%
  dplyr::rename(n_cases = n)

## Join the sentiment data per week with the casesPerWeek dataframe
sentiment_with_cases <- full_join(sentiment_byWeek, casesPerWeek, by="week")

## Join the survey data
#sentiment_with_cases_survey <- full_join(sentiment_with_cases, survey_weekly, by="week")

## Scale the cases between 0 and 1 (/max)
sentiment_with_cases_survey$casesScaled <- sentiment_with_cases_survey$n_cases /
  max(sentiment_with_cases_survey$n_cases,na.rm = TRUE)

##############
### Save the dataset
#############

#write.csv(sentiment_with_cases_survey,
#         "~/Documents/GitHub/SICSS_2020/sentiment_cases_survey_weekly_Canada.csv")

```

```{r warning=FALSE}
##############
## Final analyses (after all wrangling is done)
##############
## Load the dataset
library(readr)
sentiment_with_cases_survey <- read_csv("sentiment_cases_survey_weekly_Canada.csv")

###############
## Plot the results
###############
library(ggplot2)
## Ratio of positivity and scaled cases
ggplot(sentiment_with_cases_survey, aes(x=week)) +
  geom_line(aes(y = fear_overall), color = "red") +
  geom_line(aes(y = satMeas_overall), color="blue") +
  geom_line(aes(y = ratioPosNeg), color="black") +
  geom_line(aes(y = casesScaled), color = "green") +
  
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

## See if there is a correlation between positivity and survey approval
cor.test(sentiment_with_cases_survey$ratioPosNeg,
         sentiment_with_cases_survey$satMeas_overall, 
         method = "spearman",
         na.action = "na.exclude")

## See if there is a correlation between cases and positive sentiment
cor.test(sentiment_with_cases_survey$ratioPosNeg,
         sentiment_with_cases_survey$casesScaled, 
         method = "spearman",
         na.action = "na.exclude")

## See if there is a correlation between cases and fear
cor.test(sentiment_with_cases_survey$fear_overall,
         sentiment_with_cases_survey$casesScaled, 
         method = "spearman",
         na.action = "na.exclude")


```

```{r warning=FALSE}
##############
## Final analyses (after all wrangling is done)
##############
## Load the dataset
library(readr)
sentiment_with_cases_survey <- read_csv("sentiment_cases_survey_weekly_Canada.csv")

###############
## Plot the results
###############
library(ggplot2)
## Ratio of positivity and scaled cases
ggplot(sentiment_with_cases_survey, aes(x=week)) +
  geom_line(aes(y = fear_overall), color = "red") +
  geom_line(aes(y = satMeas_overall), color="blue") +
  geom_line(aes(y = ratioPosNeg), color="black") +
  geom_line(aes(y = casesScaled), color = "green") +
  
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("")+
  ggtitle("Sentiment in Tweets")+
  theme(aspect.ratio=1/4)

## See if there is a correlation between positivity and survey approval
cor.test(sentiment_with_cases_survey$ratioPosNeg,
         sentiment_with_cases_survey$satMeas_overall, 
         method = "spearman",
         na.action = "na.exclude")

## See if there is a correlation between cases and positive sentiment
cor.test(sentiment_with_cases_survey$ratioPosNeg,
         sentiment_with_cases_survey$casesScaled, 
         method = "spearman",
         na.action = "na.exclude")

## See if there is a correlation between cases and fear
cor.test(sentiment_with_cases_survey$fear_overall,
         sentiment_with_cases_survey$casesScaled, 
         method = "spearman",
         na.action = "na.exclude")