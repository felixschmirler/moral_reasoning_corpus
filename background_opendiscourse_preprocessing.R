#Script to preprocess corpora ahead of pre-sampling and annotation

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

#contributions_extended <- readRDS("open_discourse_corpus/RDS/contributions_extended.RDS") #initially not relevant
#contributions_simplified <- readRDS("open_discourse_corpus/RDS/contributions_simplified.RDS") #initially not relevant
#electoral_terms <- readRDS("open_discourse_corpus/RDS/electoral_terms.RDS") #initially not relevant
factions <- readRDS("data/open_discourse_corpus/RDS/factions.RDS") 
politicians <- readRDS("data/open_discourse_corpus/RDS/politicians.RDS") 
speeches <- readRDS("data/open_discourse_corpus/RDS/speeches.RDS")

#join open discourse datasets
open_discourse <- speeches %>%
  left_join(politicians, join_by(politician_id == id)) %>%
  left_join(factions, join_by(faction_id == id))

#remove individual datasets
rm(factions)
rm(politicians)
rm(speeches)

#rename and reduce number of variables
open_discourse %<>% 
  mutate(
    text_id = paste0("opendiscourse_", id),
    author_id = paste0(politician_id, "_", first_name, "_", last_name),
    date = ymd(date),
    text_length = str_length(speech_content)
  ) %>%
  select(text_id, text = speech_content, author_id, date, text_length, party = abbreviation) #variables temporarily kept for filtering

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
open_discourse %<>% 
  filter(author_id != "-1_Not found_") %>%
  filter(party != "not found", party != "Fraktionslos") %>% #also removes ministers and chancellors due to missing party affiliation in the protocols
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

#remove reactions and line breaks in speeches
open_discourse %<>% 
  mutate(
    text = str_remove_all(text, "\\(\\{\\d+\\}\\)"),
    text = str_squish(text)
  ) 

#split into sentences - takes many hours, run as a background jobs in badges (e.g. 20 min for 10k on a normal laptop)  

#test badge
open_discourse <- open_discourse[90001:135357,] #change manually depending on what badge you want to run

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
saveRDS(open_discourse_sentences, "data/open_discourse_corpus/open_discourse_sentences_90k_135k.rds")

