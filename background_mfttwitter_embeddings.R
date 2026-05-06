#Background script to create embeddings for mft twitter data


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

##1.4. MFT Twitter Corpus - Hoover et al., 2020 ----

#load data
mft_twitter_sentences <- readRDS("data/mft_twitter_corpus/mft_twitter_sentences.rds")

#embeddings
mft_twitter_embed <- sbert_multiling$encode(mft_twitter_sentences$sentence)

#save output
saveRDS(mft_twitter_embed, "data/mft_twitter_corpus/mft_twitter_embed.rds")
