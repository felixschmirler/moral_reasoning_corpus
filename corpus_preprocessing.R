#Script to preprocess corpora ahead of pre-sampling and annotation

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(jsonlite) #to read json files
library(legislatoR) #to retrieve information about the political affiliation of politicians, Rvoteview could be an alternative 
library(rvest) #for scraping UK parliament data 2019-2022


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
factions <- readRDS("data/german_bundestag/open_discourse_corpus/RDS/factions.RDS") 
politicians <- readRDS("data/german_bundestag/open_discourse_corpus/RDS/politicians.RDS") 
speeches <- readRDS("data/german_bundestag/open_discourse_corpus/RDS/speeches.RDS")

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

####To do: filter out very long and very short speeches ----

#lower limit examples
#11 tokens in Aroyehun et al. 2025 - fairly inclusive
#500 words Bachmann & Gleibs 2024 - more conservative

open_discourse %>%
  filter(
    text_length >= 200,
    text_length <= quantile(text_length, 0.99)
  ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

quantile(open_discourse$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out 1% of the longest speeches and speeches with less than 300 characters
#open_discourse %<>% filter(text_length > 218, text_length <= 7195)

#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  

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
saveRDS(open_discourse_sentences, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_test.rds")

####load pre-processed files ----
open_discourse_sentences_test <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_test.rds")

open_discourse_sentences_30 <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_1_30k.rds")
open_discourse_sentences_60 <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_30k_60k.rds")
open_discourse_sentences_90 <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_60k_90k.rds")
open_discourse_sentences_135 <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_90k_135k.rds")

open_discourse_sentences <- bind_rows(open_discourse_sentences_30, 
                                      open_discourse_sentences_60,
                                      open_discourse_sentences_90,
                                      open_discourse_sentences_135)

saveRDS(open_discourse_sentences, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences.rds")

rm(open_discourse_sentences_30)
rm(open_discourse_sentences_60)
rm(open_discourse_sentences_90)
rm(open_discourse_sentences_135)

open_discourse_sentences <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences.rds")

####To do: filter out very long and very short sentences ----

open_discourse_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(open_discourse_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 10% and bottom 10%
#open_discourse_sentences %<>% filter(sentence_length > 28, sentence_length <= 196)

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

####To do: filter out very long and very short speeches ----
uscongress_pre2016 %<>% 
  filter(text_id != "uscongress_pre2016_1140100957") #speech is so long it breaks the script

uscongress_pre2016 %>%
  filter(
    text_length >= 200,
    text_length <= quantile(text_length, 0.99)
  )  %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(uscongress_pre2016$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out top 10% and bottom 10%
#uscongress_pre2016 %<>% filter(text_length > 218, text_length <= 7195)

#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  

#test badge
uscongress_pre2016 <- uscongress_pre2016[1:1000,] #change manually depending on what badge you want to run

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
saveRDS(uscongress_pre2016_sentences, "data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_test.rds")

####load pre-processed files ----
uscongress_pre2016_sentences_test <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_test.rds")

uscongress_pre2016_sentences_50 <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_50k.rds")
uscongress_pre2016_sentences_150 <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_150k.rds")
uscongress_pre2016_sentences_250 <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_250k.rds")
uscongress_pre2016_sentences_350 <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_350k.rds")
uscongress_pre2016_sentences_400 <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_400k.rds")

uscongress_pre2016_sentences <- bind_rows(uscongress_pre2016_sentences_50, 
                                          uscongress_pre2016_sentences_150,
                                          uscongress_pre2016_sentences_250,
                                          uscongress_pre2016_sentences_350,
                                          uscongress_pre2016_sentences_400)

saveRDS(uscongress_pre2016_sentences, "data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences.rds")

rm(uscongress_pre2016_sentences_50)
rm(uscongress_pre2016_sentences_150)
rm(uscongress_pre2016_sentences_250)
rm(uscongress_pre2016_sentences_350)
rm(uscongress_pre2016_sentences_400)

uscongress_pre2016_sentences <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences.rds")

####To do: filter out very long and very short sentences ----
uscongress_pre2016_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(uscongress_pre2016_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

###1.2.2. US Congress post 2016 - Judd et al ----

#retrieve political affiliation from legislatoR
cld_content()
us_core <- get_core(legislature = "usa_house")
us_party <- get_political(legislature = "usa_house") 
us_ids <- get_ids(legislature = "usa_house") 

us_legislatoR_combined <- us_core %>%
  left_join(us_ids) %>%
  full_join(us_party, join_by(pageid)) %>%
  mutate(across(everything(), as.character)) %>%
  filter(!is.na(bioguide))

rm(us_core)
rm(us_party)
rm(us_ids)

#volume and congress session for matching party affiliation
congress_session <- tibble(
  vol = c(162, 163, 164, 165, 166, 167, 168, 169, 170, 171) %>% as.character(),
  session = c(114, 115, 115, 116, 116, 117, 117, 118, 118, 119) %>% as.character()
)


#test files
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1241-2.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-13/json/CREC-2016-09-12-pt1-PgE1241-3.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1241-3.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1241-4.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1241-5.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1241-6.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1242.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgE1242-2.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-12/json/CREC-2016-09-12-pt1-PgH5279.json")
test_json <- fromJSON("data/us_congress/judd_post2016/output/2016/CREC-2016-09-27/json/CREC-2016-09-27-pt1-PgH5961.json")

# header as tibble
header <- as_tibble(test_json$header)

# content as tibble
content <- as_tibble(test_json$content) %>% filter(kind == "speech")

# related_bills as tibble - only exists in some files
#related_bills <- as_tibble(test_json$related_bills)

# combine 
test_combined <- bind_cols(header, content) %>%
  left_join(congress_session) %>%
  left_join(us_legislatoR_combined, 
            by = c( "speaker_bioguide" = "bioguide", "session" = "session")) %>% 
  mutate(across(everything(), as.character))

rm(test_json)
rm(header)
rm(content)
rm(test_combined)
rm(test_json)

#function to convert individual json files into tibble format (requires additional data)
parse_congress_files <- function(path) {
  
  json_file <- fromJSON(path)
  
  header <- as_tibble(json_file$header)
  content <- as_tibble(json_file$content) %>% filter(kind == "speech")
  
  if (!"speaker_bioguide" %in% names(content)) {
    content$speaker_bioguide <- NA_character_
  }
  
  bind_cols(tibble(file = path),header, content) %>% 
    mutate(across(everything(), as.character)) %>%
    left_join(congress_session) %>%
    left_join(us_legislatoR_combined, 
              by = c( "speaker_bioguide" = "bioguide", "session" = "session")) 
}


#list all 2016 files
root <- "data/us_congress/judd_post2016/output/2016"   

filepaths_2016 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2016)
file.exists(filepaths_2016)

#test_files <- filepaths_2016[1:10]
congress_tbl_2016 <- map_dfr(filepaths_2016, parse_congress_files)

congress_tbl_2016 %>% distinct(text)

congress_tbl_2016 %>% count(file) %>% view() 


#list all 2017 files
root <- "data/us_congress/judd_post2016/output/2017"   

filepaths_2017 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2017)
file.exists(filepaths_2017) 

congress_tbl_2017 <- map_dfr(filepaths_2017, parse_congress_files)

#list all 2018 files
root <- "data/us_congress/judd_post2016/output/2018"   

filepaths_2018 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2018)
file.exists(filepaths_2018) 

congress_tbl_2018 <- map_dfr(filepaths_2018, parse_congress_files)

#list all 2019 files
root <- "data/us_congress/judd_post2016/output/2019"   

filepaths_2019 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2019)
file.exists(filepaths_2019) 

congress_tbl_2019 <- map_dfr(filepaths_2019, parse_congress_files)

#list all 2020 files
root <- "data/us_congress/judd_post2016/output/2020"   

filepaths_2020 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2020)
file.exists(filepaths_2020) 

congress_tbl_2020 <- map_dfr(filepaths_2020, parse_congress_files)

#list all 2021 files
root <- "data/us_congress/judd_post2016/output/2021"   

filepaths_2021 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2021)
file.exists(filepaths_2021) 

congress_tbl_2021 <- map_dfr(filepaths_2021, parse_congress_files)

#list all 2022 files
root <- "data/us_congress/judd_post2016/output/2022"   

filepaths_2022 <- list.files(
  root,
  pattern = "\\.json$",
  full.names = TRUE,
  recursive = TRUE
)

length(filepaths_2022)
file.exists(filepaths_2022) 

congress_tbl_2022 <- map_dfr(filepaths_2022, parse_congress_files)

#join files and write to file

uscongress_post2016_raw <- bind_rows(
  congress_tbl_2016,
  congress_tbl_2017,
  congress_tbl_2018,
  congress_tbl_2019,
  congress_tbl_2020,
  congress_tbl_2021,
  congress_tbl_2022
)

saveRDS(uscongress_post2016_raw, "data/us_congress/judd_post2016/uscongress_post2016_raw.rds")

#remove findividual objects
rm(header)
rm(content)

rm(root)
rm(filepaths_2016)
rm(filepaths_2017)
rm(filepaths_2018)
rm(filepaths_2019)
rm(filepaths_2020)
rm(filepaths_2021)
rm(filepaths_2022)

rm(congress_session)
rm(us_legislatoR_combined)

rm(congress_tbl_2016)
rm(congress_tbl_2017)
rm(congress_tbl_2018)
rm(congress_tbl_2019)
rm(congress_tbl_2020)
rm(congress_tbl_2021)
rm(congress_tbl_2022)

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
  filter(
    !text_length >= 300,
    text_length <= quantile(text_length, 0.99)
  ) %>% view()
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

quantile(uscongress_post2016$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#filter out top 10% and bottom 10%
#uscongress_post2016 %<>% filter(text_length > 218, text_length <= 7195)

#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  

#test badge
uscongress_post2016 <- uscongress_post2016[1:1000,] #change manually depending on what badge you want to run

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
rm(uscongress_post2016_raw)
rm(uscongress_post2016)

#write to file 
saveRDS(uscongress_post2016_sentences, "data/us_congress/judd_post2016/uscongress_post2016_sentences_test.rds")

###load pre-processed files ----
uscongress_post2016_sentences_test <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences_test.rds")

uscongress_post2016_sentences_100 <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences_100k.rds")
uscongress_post2016_sentences_120 <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences_120k.rds")

uscongress_post2016_sentences <- bind_rows(uscongress_post2016_sentences_100, 
                                      uscongress_post2016_sentences_120)

saveRDS(uscongress_post2016_sentences, "data/us_congress/judd_post2016/uscongress_post2016_sentences.rds")

rm(uscongress_post2016_sentences_100)
rm(uscongress_post2016_sentences_120)

uscongress_post2016_sentences <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences.rds")

###To do: filter out very long and very short sentences ----
uscongress_pre2016_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(uscongress_pre2016_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

###1.2.3. US Congress combined ----

uscongress_pre2016_sentences <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences.rds")
uscongress_post2016_sentences <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences.rds")

uscongress_combined_sentences <- bind_rows(uscongress_pre2016_sentences, 
                                           uscongress_post2016_sentences)

uscongress_combined_sentences %>% 
  mutate(
    year = year(date)
  ) %>% 
  count(year) %>% view()

rm(uscongress_pre2016_sentences)
rm(uscongress_post2016_sentences)

saveRDS(uscongress_combined_sentences, "data/us_congress/uscongress_sentences.rds")

####load pre-processed file ----
uscongress_combined_sentences <- readRDS("data/us_congress/uscongress_sentences.rds")

##1.3. UK Parliament ----

###1.3.1. ParlSpeech V2 (UK) - Rauh & Schwalbach, 2020 ####
parlspeech_uk <- readRDS("data/uk_parliament/uk_parlspeechv2/Corp_HouseOfCommons_V2.rds")

#rename and reduce number of variables
parlspeech_uk %<>% 
  mutate(
    text_id = paste0("uk_parlspeech_", date, "_", speechnumber),
    author_id = paste0(speaker),
    date = ymd(date),
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
parlspeech_uk <- parlspeech_uk[1:1000,] #change manually depending on what badge you want to run

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
saveRDS(parlspeech_uk_sentences, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_test.rds")

###load pre-processed files ----
parlspeech_uk_sentences_test <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_test.rds")

parlspeech_uk_sentences_50 <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_test.rds")

parlspeech_uk_sentence <- bind_rows(parlspeech_uk_sentences_50, 
                                    parlspeech_uk_sentences_150)

saveRDS(parlspeech_uk_sentences, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences.rds")

rm(parlspeech_uk_sentences_50)
rm(parlspeech_uk_sentences_150)

parlspeech_uk_sentences <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences.rds")

###1.3.2. Data from theyworkforyou.com ####

#get list of parliament records for missing time window
url <- "https://www.theyworkforyou.com/pwdata/scrapedxml/debates/"  

page <- read_html(url)

links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  slice(-(1:6))

links %<>% 
  mutate(
    date = str_extract(value, "(?<=debates).*(?=..xml)") %>% ymd()
  ) %>%
  filter(date >= as_date("2019-12-17")) %>% 
  filter(date <= as_date("2022-05-20"))

#test 

url <- paste0("https://www.theyworkforyou.com/pwdata/scrapedxml/debates/", links$value[1])

page <- read_html(url)

test <- page %>%
  html_elements("speech") %>% as.character %>% view()

test #frankly too much hassle for now


