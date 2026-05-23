#Background script to create embeddings for emfd news


#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.

#import sentence BERT model
st <- import("sentence_transformers")$SentenceTransformer
#all_mini <- st("all-MiniLM-L6-v2")  # Or "all-mpnet-base-v2" for more precision
sbert_multiling <- st("paraphrase-multilingual-mpnet-base-v2") # multilingual model
#util_st <- import("sentence_transformers.util")

#1. eMFD - Hopp et al., 2021 

#load data
emfd_news_sentences <- readRDS("data/emfd_news/emfd_news_sentences.rds")

#embeddings
emfd_news_embed <- sbert_multiling$encode(emfd_news_sentences$sentence)

#save output
saveRDS(emfd_news_embed, "data/emfd_news/emfd_news_embed.rds")
