#Script to pre sample the larger text corpora


#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()

#import sentence BERT model
st <- import("sentence_transformers")$SentenceTransformer
#all_mini <- st("all-MiniLM-L6-v2")  # Or "all-mpnet-base-v2" for more precision
sbert_multiling <- st("paraphrase-multilingual-mpnet-base-v2") # multilingual model
#util_st <- import("sentence_transformers.util")

#load DDR vectors
sentences_deont_main_ddr <- readRDS("content/sentences_deont_main_ddr_rds")
sentences_conseq_main_ddr <- readRDS("content/sentences_conseq_main_ddr_rds")

#1. Data ----
##1.1. Open Discourse Corpus - Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####

#load data
open_discourse_sentences <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences.rds")

#embeddings
open_discourse_embed <- sbert_multiling$encode(open_discourse_sentences$sentence)
saveRDS(open_discourse_sentences, "content/open_discourse_embed.rds")
