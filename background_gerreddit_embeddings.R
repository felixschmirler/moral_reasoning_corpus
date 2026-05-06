#Background script to create embeddings for german reddit data


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

#1. Data ----

##1.6. GeRedE German Reddit Corpus - Blombach et al. (2020) ----

#load data
gerede_politics_sentences <- readRDS("data/german_reddit_corpus/gerede_politics_sentences.rds")

#embeddings
gerede_politics_embed <- sbert_multiling$encode(gerede_politics_sentences$sentence)

#save output
saveRDS(gerede_politics_embed, "data/mft_twitter_corpus/gerede_politics_embed.rds")
