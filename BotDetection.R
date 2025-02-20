# Load required packages
library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)
library(dplyr)
library(caret)

# Set options
options(stringsAsFactors = F)
options("scipen" = 100, "digits" = 4)

# Load stopwords
english_stopwords <- readLines("stopwords_en.txt", encoding = "UTF-8")

# Load dataset
data <- read.csv("si_3.5.csv")
data <- data %>%
  select(author_id, label, decision, explanation) %>%
  mutate(outcome = case_when(
    label == "bot" & decision == "bot" ~ 1,
    label == "bot" & decision == "human" ~ 2,
    label == "human" & decision == "bot" ~ 3,
    label == "human" & decision == "human" ~ 4)) %>%
  relocate(outcome, .after = 3) %>%
  rename(doc_id = author_id, text = explanation) %>%
  mutate(across(c(label, decision), ~na_if(., ""))) %>%
  filter(!is.na(label) & !is.na(decision))

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(data$decision), as.factor(data$label))
print(conf_matrix)

# Data Preparation for LDA
data.1 <- data %>% filter(outcome == 1)
corpus <- Corpus(DataframeSource(data.1))
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# Document Term Matrix
minimumFrequency <- 1
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
data.1 <- data.1[sel_idx, ]

# Find Optimal Number of Topics
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Create Word Cloud
topicToViz <- 1
top20terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:20]
words <- names(top20terms)
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:20]
mycolors <- brewer.pal(8, "Dark2")

png(filename="si-1.png", width = 1200, height = 900, res = 300)
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
dev.off()
