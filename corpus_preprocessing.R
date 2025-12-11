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
open_discourse <- open_discourse[1:1000,] #change manually depending on what badge you want to run

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
saveRDS(open_discourse_sentences, "data/open_discourse_corpus/open_discourse_sentences_test.rds")

###load pre-processed file ----
open_discourse_sentences <- readRDS("data/open_discourse_corpus/open_discourse_sentences_test.rds")

###TO DO filter out very long speeches and short comments ----
speeches_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(speeches_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 25% and bottom 25%
#speeches_sentences %<>% filter(sentence_length > 48, sentence_length < 145)

##1.2. US Congress ----

##1.2.1. US Congress - Gentzkow et al ----
#speeches
us_speeches_106 <- read_delim("data/us_congress/speeches_106.txt", delim = "|")
us_speeches_107 <- read_delim("data/us_congress/speeches_107.txt", delim = "|")
us_speeches_108 <- read_delim("data/us_congress/speeches_108.txt", delim = "|")
us_speeches_109 <- read_delim("data/us_congress/speeches_109.txt", delim = "|")
us_speeches_110 <- read_delim("data/us_congress/speeches_110.txt", delim = "|")
us_speeches_111 <- read_delim("data/us_congress/speeches_111.txt", delim = "|")
us_speeches_112 <- read_delim("data/us_congress/speeches_112.txt", delim = "|")
us_speeches_113 <- read_delim("data/us_congress/speeches_113.txt", delim = "|")
us_speeches_114 <- read_delim("data/us_congress/speeches_114.txt", delim = "|")

#descr
us_descr_106 <- read_delim("data/us_congress/descr_106.txt", delim = "|")
us_descr_107 <- read_delim("data/us_congress/descr_107.txt", delim = "|")
us_descr_108 <- read_delim("data/us_congress/descr_108.txt", delim = "|")
us_descr_109 <- read_delim("data/us_congress/descr_109.txt", delim = "|")
us_descr_110 <- read_delim("data/us_congress/descr_110.txt", delim = "|")
us_descr_111 <- read_delim("data/us_congress/descr_111.txt", delim = "|")
us_descr_112 <- read_delim("data/us_congress/descr_112.txt", delim = "|")
us_descr_113 <- read_delim("data/us_congress/descr_113.txt", delim = "|")
us_descr_114 <- read_delim("data/us_congress/descr_114.txt", delim = "|")

#SpeakerMap
us_SpeakerMap_106 <- read_delim("data/us_congress/106_SpeakerMap.txt", delim = "|")
us_SpeakerMap_107 <- read_delim("data/us_congress/107_SpeakerMap.txt", delim = "|")
us_SpeakerMap_108 <- read_delim("data/us_congress/108_SpeakerMap.txt", delim = "|")
us_SpeakerMap_109 <- read_delim("data/us_congress/109_SpeakerMap.txt", delim = "|")
us_SpeakerMap_110 <- read_delim("data/us_congress/110_SpeakerMap.txt", delim = "|")
us_SpeakerMap_111 <- read_delim("data/us_congress/111_SpeakerMap.txt", delim = "|")
us_SpeakerMap_112 <- read_delim("data/us_congress/112_SpeakerMap.txt", delim = "|")
us_SpeakerMap_113 <- read_delim("data/us_congress/113_SpeakerMap.txt", delim = "|")
us_SpeakerMap_114 <- read_delim("data/us_congress/114_SpeakerMap.txt", delim = "|")

#join Gentzkow datasets
uscongress_gentzkow <- speeches %>%
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