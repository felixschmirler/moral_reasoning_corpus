#Background script to create embeddings for open discourse data


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

##1.1. Open Discourse Corpus - Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####

#load data
open_discourse_sentences_s <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s.rds")

#embeddings
open_discourse_embed <- sbert_multiling$encode(open_discourse_sentences_s$sentence)

#save output
saveRDS(open_discourse_embed, "data/german_bundestag/open_discourse_corpus/open_discourse_embed.rds")
