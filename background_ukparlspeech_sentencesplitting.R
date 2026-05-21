#Script to split us congress data (Gentzkow - pre 2016) into sentences ahead of pre-sampling and annotation

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


##1.3. UK Parliament ----

###1.3.1. ParlSpeech V2 (UK) - Rauh & Schwalbach, 2020 ####
parlspeech_uk <- readRDS("data/uk_parliament/uk_parlspeechv2/Corp_HouseOfCommons_V2.rds")

#rename and reduce number of variables
parlspeech_uk %<>% 
  mutate(
    text_id = paste0("uk_parlspeech_", date, "_", speechnumber),
    author_id = paste0(speaker),
    date = ymd(date),
    text = str_replace_all(text, "hon.|Hon.", "honourable") %>% str_squish(),
    text_length = str_length(text)
  ) %>%
  select(text_id, text, author_id, date, text_length, party) 

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
parlspeech_uk %<>% 
  filter(author_id != "CHAIR") %>%
  filter(party == "Lab" | party == "Con") %>%  
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)


####To do: filter out very long and very short speeches ----

#lower limit examples
#11 tokens in Aroyehun et al. 2025 - fairly inclusive
#500 words Bachmann & Gleibs 2024 - more conservative

parlspeech_uk %>%
  filter(
    text_length >= 100,
    text_length <= quantile(text_length, 0.99)
  ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

quantile(parlspeech_uk$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#test badge
parlspeech_uk <- parlspeech_uk[600001:nrow(parlspeech_uk),] # 1055249 max change manually depending on what badge you want to run

docs_gen <- en_md$pipe(parlspeech_uk$text)

docs <- iterate(docs_gen)

parlspeech_uk_sentences <- map2_dfr(
  parlspeech_uk$text_id,
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

parlspeech_uk_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
parlspeech_uk_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
parlspeech_uk_sentences %<>% 
  left_join(parlspeech_uk) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(parlspeech_uk)

#write to file 
saveRDS(parlspeech_uk_sentences, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_1m_b.rds")


