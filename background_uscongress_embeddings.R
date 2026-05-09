#Background script to create embeddings for US Congress data


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

##1.2. US Congress Data ----

#load data
uscongress_combined_sentences_s <- readRDS("data/us_congress/uscongress_sentences_c_s.rds")

#subset data due to processing time 
uscongress_combined_sentences_s <- uscongress_combined_sentences_s[3000001:5000000,] #6952383

#embeddings
uscongress_combined_embed <- sbert_multiling$encode(uscongress_combined_sentences_s$sentence)

#save output
saveRDS(uscongress_combined_embed, "data/us_congress/uscongress_combined_embed_5m_c.rds")
