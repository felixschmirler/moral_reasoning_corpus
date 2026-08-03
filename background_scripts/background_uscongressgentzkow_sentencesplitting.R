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


#1. Data ----
##1.2. US Congress ----

###1.2.1. US Congress pre 2016 - Gentzkow et al ----

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


uscongress_pre2016 %<>% 
  mutate(
    text_id = paste0("uscongress_pre2016_", speech_id),
    author_id = paste0(speakerid, "_", firstname, "_", last_name),
    date = ymd(date),
    text = str_replace_all(speech, "\\.\\s+([a-z])", " \\1") %>% str_squish(),
    text_length = str_length(text)
  ) %>%
  select(text_id, text, author_id, date, text_length, party)

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
uscongress_pre2016 %<>% 
  filter(author_id != "NA_NA_Unknown") %>%
  filter(party == "D" | party == "R") %>% 
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

####To do: filter out very long and very short speeches ----
uscongress_pre2016 %<>% 
  filter(text_id != "uscongress_pre2016_1140100957") #speech is so long it breaks the script

#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  

#test badge
uscongress_pre2016 <- uscongress_pre2016[100001:401746,] #401746 change manually depending on what badge you want to run

docs_gen <- en_md$pipe(uscongress_pre2016$text)

docs <- iterate(docs_gen)

uscongress_pre2016_sentences <- map2_dfr(
  uscongress_pre2016$text_id,
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

uscongress_pre2016_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
uscongress_pre2016_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
uscongress_pre2016_sentences %<>% 
  left_join(uscongress_pre2016) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(uscongress_pre2016)

#write to file 
saveRDS(uscongress_pre2016_sentences, "data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_400k_c.rds")


