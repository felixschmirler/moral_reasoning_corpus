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

###To do: filter out very long and very short speeches ----
open_discourse %>%
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 15000))

quantile(open_discourse$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out top 10% and bottom 10%
#open_discourse %<>% filter(text_length > 218, text_length <= 7195)

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
rm(open_discourse_sentences)

#write to file 
saveRDS(open_discourse_sentences, "data/open_discourse_corpus/open_discourse_sentences_test.rds")

###load pre-processed files ----
open_discourse_sentences_test <- readRDS("data/open_discourse_corpus/open_discourse_sentences_test.rds")

open_discourse_sentences_30 <- readRDS("data/open_discourse_corpus/open_discourse_sentences_1_30k.rds")
open_discourse_sentences_60 <- readRDS("data/open_discourse_corpus/open_discourse_sentences_30k_60k.rds")
open_discourse_sentences_90 <- readRDS("data/open_discourse_corpus/open_discourse_sentences_60k_90k.rds")
open_discourse_sentences_135 <- readRDS("data/open_discourse_corpus/open_discourse_sentences_90k_135k.rds")

open_discourse_sentences <- bind_rows(open_discourse_sentences_30, 
                                      open_discourse_sentences_60,
                                      open_discourse_sentences_90,
                                      open_discourse_sentences_135)

saveRDS(open_discourse_sentences, "data/open_discourse_corpus/open_discourse_sentences.rds")

rm(open_discourse_sentences_30)
rm(open_discourse_sentences_60)
rm(open_discourse_sentences_90)
rm(open_discourse_sentences_135)

open_discourse_sentences <- readRDS("data/open_discourse_corpus/open_discourse_sentences.rds")

###To do: filter out very long and very short sentences ----
open_discourse_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(open_discourse_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 10% and bottom 10%
#open_discourse_sentences %<>% filter(sentence_length > 28, sentence_length <= 196)

##1.2. US Congress ----

##1.2.1. US Congress pre 2016 - Gentzkow et al ----

#speeches
us_speeches_pre2016 <- bind_rows(
  read_delim("data/us_congress/gentzkow_pre2016/speeches_106.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_107.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_108.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_109.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_110.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_111.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_112.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_113.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/speeches_114.txt", delim = "|", locale = locale(encoding = "Latin1"))
)

#descr
us_descr_pre2016 <- bind_rows(
  read_delim("data/us_congress/gentzkow_pre2016/descr_106.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_107.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_108.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_109.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_110.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_111.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_112.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_113.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/descr_114.txt", delim = "|", locale = locale(encoding = "Latin1"))
)

#SpeakerMap
us_speakermap_pre2016 <- bind_rows(
  read_delim("data/us_congress/gentzkow_pre2016/106_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/107_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/108_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/109_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/110_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/111_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/112_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/113_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1")),
  read_delim("data/us_congress/gentzkow_pre2016/114_SpeakerMap.txt", delim = "|", locale = locale(encoding = "Latin1"))
)

#join Gentzkow datasets
uscongress_pre2016 <- us_speeches_pre2016 %>% 
  left_join(us_descr_pre2016) %>% 
  left_join(us_speakermap_pre2016, by = join_by(speech_id)) 

#remove individual datasets
rm(us_speeches_pre2016)
rm(us_descr_pre2016)
rm(us_speakermap_pre2016)

#House of Representatives
uscongress_pre2016 %<>%
  filter(chamber.x == "H")


#rename and reduce number of variables
uscongress_pre2016 %<>% 
  mutate(
    text_id = paste0("uscongress_pre2016_", speech_id),
    author_id = paste0(speakerid, "_", firstname, "_", last_name),
    date = ymd(date),
    text_length = str_length(speech)
  ) %>%
  select(text_id, text = speech, author_id, date, text_length, party)

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
uscongress_pre2016 %<>% 
  filter(author_id != "NA_NA_Unknown") %>%
  filter(party == "D" | party == "R") %>% 
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

###To do: filter out very long and very short speeches ----
uscongress_pre2016 %>%
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 15000))

quantile(uscongress_pre2016$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out top 10% and bottom 10%
#uscongress_pre2016 %<>% filter(text_length > 218, text_length <= 7195)
