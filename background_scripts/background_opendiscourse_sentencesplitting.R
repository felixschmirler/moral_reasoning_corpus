#Script to split the Open Discourse Corpus into sentences ahead of pre-sampling and annotation

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()

#import spaCy 
spacy <- import("spacy")
ger_md <- spacy$load("de_core_news_md") #mostly to split german speeches into sentences
en_md <- spacy$load("en_core_web_md") #mostly to split english speeches into sentences

#1. Data ----
##1.1. Open Discourse Corpus - Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####
open_discourse <- readRDS("data/german_bundestag/open_discourse_corpus/1_open_discourse_raw.rds")

#split into sentences - takes many hours, run as a background jobs in badges (e.g. 20 min for 10k on a normal laptop)  

#test badge
open_discourse <- open_discourse[40001:134910,] # 134910 max, change manually depending on what badge you want to run

docs_gen <- ger_md$pipe(open_discourse$text)

docs <- iterate(docs_gen)

open_discourse_sentences <- map2_dfr(
  open_discourse$text_id,
  docs,
  \(id, doc) {
    sents <- iterate(doc$sents)
    tibble(
      text_id     = id,
      sentence_id = seq_along(sents),
      sentence    = map_chr(sents, \(s) s$text)
    )
  }
)

open_discourse_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
open_discourse_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
open_discourse_sentences %<>% 
  left_join(open_discourse) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(open_discourse)

#write to file 
saveRDS(open_discourse_sentences, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_135k_b.rds")

