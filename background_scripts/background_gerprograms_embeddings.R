#Background script to create embeddings for german election programs


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

#1. German Election Programs (2002-2025) - Voit et al. 2024 

#load data
ger_programs_sentences <- readRDS("data/german_election_programs/ger_programs_sentences.rds")

#embeddings
ger_programs_embed <- sbert_multiling$encode(ger_programs_sentences$sentence)

#save output
saveRDS(ger_programs_embed, "data/german_election_programs/ger_programs_embed.rds")
