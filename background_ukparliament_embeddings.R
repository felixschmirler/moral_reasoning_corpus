#Background script to create embeddings for uk parliament data


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

#1. ParlSpeech V2 (UK) - Rauh & Schwalbach, 2020 

#load data
parlspeech_uk_sentences_s <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_s.rds")

#subset data due to processing time 
parlspeech_uk_sentences_s <- parlspeech_uk_sentences_s[7000001:7893641,] #7893641 max

#embeddings
parlspeech_uk_embed <- sbert_multiling$encode(parlspeech_uk_sentences_s$sentence)

#save output
saveRDS(parlspeech_uk_embed, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_8m.rds")
