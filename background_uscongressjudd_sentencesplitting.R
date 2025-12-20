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

###1.2.2. US Congress post 2016 - Judd et al ----

####load pre-processed file ----
uscongress_post2016_raw <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_raw.rds")

#House of Representatives and Democrats or Republicans only
uscongress_post2016_raw %<>%
  filter(chamber == "House") 

#rename and reduce number of variables
uscongress_post2016 <- uscongress_post2016_raw %>% 
  mutate(
    text_id = paste0("uscongress_post2016_", str_extract(file, "(?<=json/).*(?=.json)"), "_", turn),
    author_id = speaker_bioguide,
    date = ymd(paste(year, month, day)),
    text = str_remove(text, paste0(speaker, ".."))  %>% str_trim(),
    text_length = str_length(text)
  ) %>%
  select(text_id, text, author_id, date, text_length, party)


#filter out speeches without information about the politician, the party, pre 2000 and duplicates
uscongress_post2016 %<>% 
  filter(!is.na(author_id)) %>%
  filter(party == "D" | party == "R") %>% 
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

###To do: filter out very long and very short speeches ----
uscongress_post2016 %>%
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 15000))

quantile(uscongress_post2016$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out top 10% and bottom 10%
#uscongress_post2016 %<>% filter(text_length > 218, text_length <= 7195)

#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  
#test badge
uscongress_post2016 <- uscongress_post2016[100001:121402,] #change manually depending on what badge you want to run

docs_gen <- en_md$pipe(uscongress_post2016$text)

docs <- iterate(docs_gen)

uscongress_post2016_sentences <- map2_dfr(
  uscongress_post2016$text_id,
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

uscongress_post2016_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
uscongress_post2016_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
uscongress_post2016_sentences %<>% 
  left_join(uscongress_post2016) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(uscongress_post2016)

#write to file 
saveRDS(uscongress_post2016_sentences, "data/us_congress/judd_post2016/uscongress_post2016_sentences_120k.rds")


