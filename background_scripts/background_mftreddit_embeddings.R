#Background script to create embeddings for mft reddit data


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

##1.4. MFT Reddit Corpus - Trager et al., 2022 ----

#load data
mft_reddit_sentences <- readRDS("data/mft_reddit_corpus/mft_reddit_sentences.rds")

#embeddings
mft_reddit_embed <- sbert_multiling$encode(mft_reddit_sentences$sentence)

#save output
saveRDS(mft_reddit_embed, "data/mft_reddit_corpus/mft_reddit_embed.rds")
