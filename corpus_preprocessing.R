#Script to preprocess corpora ahead of pre-sampling and annotation

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(jsonlite) #to read json files
library(legislatoR) #to retrieve information about the political affiliation of politicians, Rvoteview could be an alternative 
#library(rvest) #for scraping UK parliament data 2019-2022 paused for now
library(data.table)

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()

#import spaCy 
spacy <- import("spacy")
ger_md <- spacy$load("de_core_news_md") #mostly to split german speeches into sentences
en_md <- spacy$load("en_core_web_md") #mostly to split english speeches into sentences

#import pandas 
pd <- import("pandas", convert = FALSE)

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
    text = str_remove_all(speech_content, "\\(\\{\\d+\\}\\)") %>% str_squish(), #remove reactions and line breaks in speeches
    text_length = str_length(speech_content)
  ) %>%
  select(text_id, text, author_id, date, text_length, party = abbreviation) #variables temporarily kept for filtering

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
open_discourse %<>% 
  filter(author_id != "-1_Not found_") %>%
  filter(party != "not found", party != "Fraktionslos") %>% #also removes ministers and chancellors due to missing party affiliation in the protocols
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

#write to file 
saveRDS(open_discourse, "data/german_bundestag/open_discourse_corpus/1_open_discourse_raw.rds")

# split into sentences - takes hours, run as a background jobs in badges 
# background_opendiscourse_sentencesplitting.R  script

#test badge
open_discourse_test <- open_discourse[1:1000,] #change manually depending on what badge you want to run

docs_gen <- ger_md$pipe(open_discourse_test$text)

docs <- iterate(docs_gen)

open_discourse_test_sentences <- map2_dfr(
  open_discourse_test$text_id,
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

open_discourse_test_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
open_discourse_test_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
open_discourse_test_sentences %<>% 
  left_join(open_discourse_test) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(open_discourse_test)

#write to file 
saveRDS(open_discourse_test_sentences, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_test.rds")

####load pre-processed files ----
#open_discourse_sentences_test <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_test.rds")

open_discourse_sentences <- bind_rows(readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_40k_b.rds"),
                                      readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_135k_b.rds")
)

saveRDS(open_discourse_sentences, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences.rds")

open_discourse_sentences <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences.rds")

#filter out very long and very short speeches 
open_discourse_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  #  filter(
  #    text_length > 100,
  #    text_length <= quantile(text_length, 0.99)
  #  ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 10000))

open_discourse_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  pull(text_length) %>%
  quantile(c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 
             0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 0.99, 1))

#filter out 1% longest speeches and speeches with less than 100 characters (~5%)
open_discourse_sentences_s <- open_discourse_sentences %>% filter(text_length > 100, text_length < 12050) 

#save shorter version
saveRDS(open_discourse_sentences_s, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s.rds")

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
    text = str_replace_all(speech, "\\.\\s+([a-z])", " \\1") %>% str_squish(),
    text_length = str_length(text)
  ) %>%
  select(text_id, text, author_id, date, text_length, party)

#uscongress_pre2016 %>% filter(text_id == "uscongress_pre2016_1070090022") %>% pull(text) %>% str_replace_all("\\.\\s+([a-z])", " \\1")

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
uscongress_pre2016 %<>% 
  filter(author_id != "NA_NA_Unknown") %>%
  filter(party == "D" | party == "R") %>% 
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

uscongress_pre2016 %>% 
  filter(text_id == "uscongress_pre2016_1080182805")
  
#split into sentences - takes hours, run as a background jobs in badges (e.g. 20 min for 10k on a regular institutioanal Laptop from 2024)  

#test badge
uscongress_pre2016 <- uscongress_pre2016 %>% 
  filter(text_id == "uscongress_pre2016_1080182805" | 
         text_id == "uscongress_pre2016_1090077611" |
         text_id == "uscongress_pre2016_1070007729" |
         text_id == "uscongress_pre2016_1090177178" |
         text_id == "uscongress_pre2016_1090041224") #[1:10,] #change manually depending on what badge you want to run

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
uscongress_pre2016_sentences_c <- bind_rows(
  readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_100k_c.rds"),
  readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_400k_c.rds")
                                          )

saveRDS(uscongress_pre2016_sentences_c, "data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_c.rds")

uscongress_pre2016_sentences_c <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_c.rds")

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
uscongress_post2016_b <- uscongress_post2016_raw %>% 
  mutate(
    text_id = paste0("uscongress_post2016_", str_extract(file, "(?<=json/).*(?=.json)"), "_", turn),
    author_id = speaker_bioguide,
    date = ymd(paste(year, month, day)),
    text = str_remove(text, paste0(speaker, ".."))  %>% str_squish(),
    text_length = str_length(text)
  ) %>%
  select(text_id, text, author_id, date, text_length, party)

#filter out speeches without information about the politician, the party, pre 2000 and duplicates
uscongress_post2016_b %<>% 
  filter(!is.na(author_id)) %>%
  filter(party == "D" | party == "R") %>% 
  filter(date >= as_date("2000-01-01")) %>% 
  distinct(text, .keep_all = TRUE)

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
uscongress_post2016_sentences_b <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences_120k_b.rds")

###1.2.3. US Congress combined ----

uscongress_pre2016_sentences_c <- readRDS("data/us_congress/gentzkow_pre2016/uscongress_pre2016_sentences_c.rds")
uscongress_post2016_sentences_b <- readRDS("data/us_congress/judd_post2016/uscongress_post2016_sentences_120k_b.rds")

uscongress_combined_sentences_c <- bind_rows(uscongress_pre2016_sentences_c, 
                                           uscongress_post2016_sentences_b)

uscongress_combined_sentences_c %>% 
  mutate(
    year = year(date)
  ) %>% 
  count(year) %>% view()

rm(uscongress_pre2016_sentences_c)
rm(uscongress_post2016_sentences_b)

saveRDS(uscongress_combined_sentences_c, "data/us_congress/uscongress_sentences_c.rds")

####load pre-processed file ----
uscongress_combined_sentences_c <- readRDS("data/us_congress/uscongress_sentences_c.rds")

#filter out very long and very short speeches 
uscongress_combined_sentences_c %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  #  filter(
  #    text_length > 100,
  #    text_length <= quantile(text_length, 0.99)
  #  ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 10000))

uscongress_combined_sentences_c %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  pull(text_length) %>%
  quantile(c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 
             0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 0.99, 1))

#filter out 1% longest speeches and speeches with less than 100 characters (~10%)
uscongress_combined_sentences_c_s <- uscongress_combined_sentences_c %>% filter(text_length > 100, text_length < 9601) 

saveRDS(uscongress_combined_sentences_c_s, "data/us_congress/uscongress_sentences_c_s.rds")

####load pre-processed file ----
uscongress_combined_sentences_c_s <- readRDS("data/us_congress/uscongress_sentences_c_s.rds")

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
#parlspeech_uk_sentences_test <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_test.rds")

parlspeech_uk_sentences <- bind_rows(readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_300k_b.rds"), 
                                     readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_600k_b.rds"),
                                     readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_1m_b.rds"))

saveRDS(parlspeech_uk_sentences, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences.rds")

parlspeech_uk_sentences <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences.rds")

#filter out very long and very short speeches 
parlspeech_uk_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  #  filter(
  #    text_length > 100,
  #    text_length <= quantile(text_length, 0.99)
  #  ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

parlspeech_uk_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  pull(text_length) %>%
  quantile(c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 
             0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 0.99, 1))

#filter out 1% longest speeches and speeches with less than 100 characters (~5%)
parlspeech_uk_sentences_s <- parlspeech_uk_sentences %>% filter(text_length > 100, text_length < 9585) 

#save shorter version
saveRDS(parlspeech_uk_sentences_s, "data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_s.rds")

###1.3.2. Data from theyworkforyou.com  (abondoned for now ----

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

rm(url)
rm(page)
rm(test)
rm(links)

##1.4. MFT Reddit Corpus - Trager et al., 2022 ####
mft_reddit <- read_csv("data/mft_reddit_corpus/final_mfrc_data.csv")

mft_reddit %>% count(subreddit, bucket) %>% view()

#rename and reduce number of variables
mft_reddit %<>% 
  mutate(
    text_id = paste0("mft_reddit_", subreddit, "_", bucket, "_", row_number()),
    author_id = NA_character_,
    date = NA_Date_,
    text_length = str_length(text),
    party = NA_character_
  ) %>%
  select(text_id, text, author_id, date, text_length, party) 


#filter out duplicates
mft_reddit %<>% 
  distinct(text, .keep_all = TRUE) 

#test badge
#mft_reddit <- mft_reddit[1:1000,] #change manually depending on what badge you want to run

docs_gen <- en_md$pipe(mft_reddit$text)

docs <- iterate(docs_gen)

mft_reddit_sentences <- map2_dfr(
  mft_reddit$text_id,
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

mft_reddit_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
mft_reddit_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
mft_reddit_sentences %<>% 
  left_join(mft_reddit) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(mft_reddit)

#write to file 
saveRDS(mft_reddit_sentences, "data/mft_reddit_corpus/mft_reddit_sentences.rds")

###load pre-processed files ----
mft_reddit_sentences <- readRDS("data/mft_reddit_corpus/mft_reddit_sentences.rds")

##1.5. MFT Twitter Corpus - Hoover et al., 2020 ####
mft_twitter <- fromJSON("data/mft_twitter_corpus/MFTC_V4_text.json")

mft_twitter %<>% unnest() %>% select(-annotations)

#rename and reduce number of variables
mft_twitter %<>% 
  mutate(
    text_id = paste0("mft_twitter_", Corpus, "_", tweet_id),
    author_id = NA_character_,
    date = NA_Date_,
    text_length = str_length(tweet_text),
    party = NA_character_
  ) %>%
  select(text_id, text = tweet_text, author_id, date, text_length, party) 


#filter out duplicates
mft_twitter %<>% 
  distinct(text, .keep_all = TRUE) 

#test badge
#mft_twitter <- mft_twitter[1:1000,] #change manually depending on what badge you want to run

docs_gen <- en_md$pipe(mft_twitter$text)

docs <- iterate(docs_gen)

mft_twitter_sentences <- map2_dfr(
  mft_twitter$text_id,
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

mft_twitter_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
mft_twitter_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
mft_twitter_sentences %<>% 
  left_join(mft_twitter) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(mft_twitter)

#write to file 
saveRDS(mft_twitter_sentences, "data/mft_twitter_corpus/mft_twitter_sentences.rds")

###load pre-processed files ----
mft_twitter_sentences <- readRDS("data/mft_twitter_corpus/mft_twitter_sentences.rds")

##1.6. GeRedE German Reddit Corpus - Blombach et al. (2020) ####

#huge file 64gb - creating 5% subsets of huge corpus for further processing
in_path  <- "data/german_reddit_corpus/gerede.ldjson"
out_path <- "data/german_reddit_corpus/gerede_100pct.ldjson"  #change name for every 5% subset

total_bytes <- file.info(in_path)$size
start_bytes <- floor(0.95 * total_bytes)  #change for every 5% subset
end_bytes   <- floor(1 * total_bytes)  #change for every 5% subset 

in_con  <- file(in_path,  open = "rb")     
out_con <- file(out_path, open = "w")

seek(in_con, where = start_bytes, origin = "start")  
readLines(in_con, n = 1, warn = FALSE)               

bytes_read  <- start_bytes                           
chunk_size  <- 10000

repeat {
  lines <- readLines(in_con, n = chunk_size, warn = FALSE)
  if (length(lines) == 0) break
  
  line_bytes <- sum(as.numeric(nchar(lines, type = "bytes"))) +
    as.numeric(length(lines))   # newline chars
  
  if (bytes_read + line_bytes <= end_bytes) {        # CHANGED
    writeLines(lines, out_con)
    bytes_read <- bytes_read + line_bytes
  } else {
    cum <- cumsum(as.numeric(nchar(lines, type = "bytes")) + 1)
    keep <- which(cum + bytes_read <= end_bytes)     # CHANGED
    if (length(keep)) writeLines(lines[keep], out_con)
    break
  }
}

close(in_con)   
close(out_con)   

#remove objects
rm(bytes_read) 
rm(chunk_size) 
rm(cum) 
rm(in_con) 
rm(in_path) 
rm(keep) 
rm(line_bytes) 
rm(lines) 
rm(out_con) 
rm(out_path) 
rm(start_bytes) 
rm(end_bytes) 
rm(total_bytes) 

#read file 
con <- file("data/german_reddit_corpus/gerede_70pct.ldjson") #change for every file
gerede_reddit <- stream_in(con, pagesize = 1000, simplifyDataFrame = FALSE)

#filter out posts tagged as politics

gerede_politics <- keep(
  gerede_reddit,
  ~ identical(.x[[1]][["link_flair_text"]], "Politik")
)

#write to file 
saveRDS(gerede_politics, "data/german_reddit_corpus/gerede_70pct_politics.rds")

rm(gerede_reddit)


###load pre-processed files 
gerede_politics_5 <- readRDS("data/german_reddit_corpus/gerede_5pct_politics.rds")
gerede_politics_10 <- readRDS("data/german_reddit_corpus/gerede_10pct_politics.rds")
gerede_politics_15 <- readRDS("data/german_reddit_corpus/gerede_15pct_politics.rds")
gerede_politics_20 <- readRDS("data/german_reddit_corpus/gerede_20pct_politics.rds")
gerede_politics_25 <- readRDS("data/german_reddit_corpus/gerede_25pct_politics.rds")
gerede_politics_30 <- readRDS("data/german_reddit_corpus/gerede_30pct_politics.rds")
gerede_politics_35 <- readRDS("data/german_reddit_corpus/gerede_35pct_politics.rds")
gerede_politics_40 <- readRDS("data/german_reddit_corpus/gerede_40pct_politics.rds")
gerede_politics_45 <- readRDS("data/german_reddit_corpus/gerede_45pct_politics.rds")
gerede_politics_50 <- readRDS("data/german_reddit_corpus/gerede_50pct_politics.rds")
gerede_politics_55 <- readRDS("data/german_reddit_corpus/gerede_55pct_politics.rds")
gerede_politics_60 <- readRDS("data/german_reddit_corpus/gerede_60pct_politics.rds")
gerede_politics_65 <- readRDS("data/german_reddit_corpus/gerede_65pct_politics.rds")
gerede_politics_70 <- readRDS("data/german_reddit_corpus/gerede_70pct_politics.rds")
gerede_politics_75 <- readRDS("data/german_reddit_corpus/gerede_75pct_politics.rds")
gerede_politics_80 <- readRDS("data/german_reddit_corpus/gerede_80pct_politics.rds")
gerede_politics_85 <- readRDS("data/german_reddit_corpus/gerede_85pct_politics.rds")
gerede_politics_90 <- readRDS("data/german_reddit_corpus/gerede_90pct_politics.rds")
gerede_politics_95 <- readRDS("data/german_reddit_corpus/gerede_95pct_politics.rds")
gerede_politics_100 <- readRDS("data/german_reddit_corpus/gerede_100pct_politics.rds")



gerede_politics_list <- c(gerede_politics_5, gerede_politics_10, gerede_politics_15, 
          gerede_politics_20, gerede_politics_25, gerede_politics_30,
          gerede_politics_35, gerede_politics_40, gerede_politics_45,
          gerede_politics_50, gerede_politics_55, gerede_politics_60,
          gerede_politics_65, gerede_politics_70, gerede_politics_75, 
          gerede_politics_80, gerede_politics_85, gerede_politics_90, 
          gerede_politics_95, gerede_politics_100) 

rm(gerede_politics_5)
rm(gerede_politics_10)
rm(gerede_politics_15)
rm(gerede_politics_20)
rm(gerede_politics_25)
rm(gerede_politics_30)
rm(gerede_politics_35)
rm(gerede_politics_40)
rm(gerede_politics_45)
rm(gerede_politics_50)
rm(gerede_politics_55)
rm(gerede_politics_60)
rm(gerede_politics_65)
rm(gerede_politics_70)
rm(gerede_politics_75)
rm(gerede_politics_80)
rm(gerede_politics_85)
rm(gerede_politics_90)
rm(gerede_politics_95)
rm(gerede_politics_100)

gerede_politics_list %>%
  View()

#write to file 
saveRDS(gerede_politics_list, "data/german_reddit_corpus/gerede_politics_list.rds")

###load pre-processed file 
gerede_politics_list <- readRDS("data/german_reddit_corpus/gerede_politics_list.rds")

# x = your big nested list, e.g. gerede_politics_all
# structure: x[[i]][[j]] is one post/comment (a named list)

to_cell <- function(z) {
  if (is.null(z)) return(NA_character_)
  
  # atomic vectors -> collapse to single string
  if (is.atomic(z)) return(paste(as.character(z), collapse = " | "))
  
  # nested lists -> store as compact string (so nothing breaks)
  paste(capture.output(str(z, max.level = 3)), collapse = " ")
}

one_row <- function(item, outer_id, inner_id) {
  item2 <- map(item, to_cell)
  item2$thread_id <- as.character(outer_id)
  item2$item_id   <- as.character(inner_id)
  as_tibble_row(item2)
}

gerede_politics_df <- imap_dfr(gerede_politics_list, function(thread, outer_id) {
  imap_dfr(thread, ~ one_row(.x, outer_id = outer_id, inner_id = .y))
})

rm(gerede_politics_list)

#write to file 
saveRDS(gerede_politics_df, "data/german_reddit_corpus/gerede_politics_df.rds")

###load pre-processed file 
gerede_politics_df <- readRDS("data/german_reddit_corpus/gerede_politics_df.rds")

#remove the original post - usually just a url
gerede_politics_comments <- gerede_politics_df %>% filter(item_id != "1") 

#filter out duplicates
gerede_politics_comments %<>% 
  distinct(body, .keep_all = TRUE) 

#remove 6 odd IDs that appear more than once
odd_ids <- gerede_politics_comments %>% count(id) %>% filter(n > 1) %>% pull(id)
gerede_politics_comments %<>% filter(!id %in% odd_ids) 

#remove missing values and deleted comments
gerede_politics_comments %<>% 
  filter(!is.na(body)) %>% 
  filter(body != "[deleted]")


#rename and reduce number of variables
gerede_politics_comments %<>% 
  mutate(
    text_id = paste0("gerede_reddit_", id),
    author_id = author,
    date = as_datetime(as.numeric(created_utc), tz = "UTC"),
    text_length = str_length(body),
    party = NA_character_
  ) %>%
  select(text_id, text = body, author_id, date, text_length, party) 



#write to file 
saveRDS(gerede_politics_comments, "data/german_reddit_corpus/gerede_politics_comments.rds")

rm(gerede_politics_df)

###load pre-processed file 
gerede_politics_comments <- readRDS("data/german_reddit_corpus/gerede_politics_comments.rds")

####To do: filter out very long and very short posts ----

#lower limit examples
#11 tokens in Aroyehun et al. 2025 - fairly inclusive
#500 words Bachmann & Gleibs 2024 - more conservative

gerede_politics_comments %>%
  #filter(
  #  text_length >= 100,
  #  text_length <= quantile(text_length, 0.99)
  #) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

quantile(gerede_reddit$text_length, c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 1))

#sentence splitting - run as background job in separate script
docs_gen <- en_md$pipe(gerede_politics_comments$text)

docs <- iterate(docs_gen)

gerede_politics_sentences <- map2_dfr(
  gerede_politics_comments$text_id,
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

gerede_politics_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
gerede_politics_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
gerede_politics_sentences %<>% 
  left_join(gerede_politics_comments) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(gerede_politics_comments)

#write to file 
saveRDS(gerede_politics_sentences, "data/german_reddit_corpus/gerede_politics_sentences.rds")

###load pre-processed file ----
gerede_politics_sentences <- readRDS("data/german_reddit_corpus/gerede_politics_sentences.rds")

##1.7.eMFD - Hopp et al., 2021 ####

#2k uncoded news articles
emfd_news_uncoded <- fromJSON("data/emfd_news/uncoded_news_text.json")

keep <- c("source", "text", "timestamp", "url")

emfd_news_uncoded <- emfd_news_uncoded[keep] %>%
  as_tibble() 

#convert from lists to vectors
emfd_news_uncoded %<>% 
  mutate(
    timestamp = map_chr(timestamp, ~ sprintf("%.0f", as.numeric(.x[[1]])))
  ) %>% 
  mutate(across(everything(), ~ map_chr(.x, ~ as.character(.x[[1]]))))

#convert timestamp column
emfd_news_uncoded %<>% 
  mutate(
    date = ymd(substr(timestamp, 1, 8))
  ) %>%
  select(-timestamp)

#1k coded news articles
df_py <- pd$read_pickle("data/emfd_news/coded_news.pkl")

# convert timestamp
df_py$timestamp <- df_py$timestamp$astype("int64")$astype("string")

emfd_news_coded <- py_to_r(df_py)
rm(df_py)

emfd_news_coded %<>% 
  mutate(
    date = ymd(substr(timestamp, 1, 8))
  ) %>%
  select(source, text, url, date)
  
#combine
emfd_news <- bind_rows(emfd_news_coded, emfd_news_uncoded)
rm(emfd_news_coded)
rm(emfd_news_uncoded)

#rename and reduce number of variables
emfd_news %<>% 
  mutate(
    text_id = paste0("emfd_", url),
    author_id = source,
    date = date,
    text_length = str_length(text),
    party = NA_character_
  ) %>%
  select(text_id, text, author_id, date, text_length, party) 


#write to file 
saveRDS(emfd_news, "data/emfd_news/emfd_news.rds")

###load pre-processed file ----
emfd_news <- readRDS("data/emfd_news/emfd_news.rds")

#split into sentences
docs_gen <- en_md$pipe(emfd_news$text)

docs <- iterate(docs_gen)

emfd_news_sentences <- map2_dfr(
  emfd_news$text_id,
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

emfd_news_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
emfd_news_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
emfd_news_sentences %<>% 
  left_join(emfd_news) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(emfd_news)

#write to file 
saveRDS(emfd_news_sentences, "data/emfd_news/emfd_news_sentences.rds")

###load pre-processed files ----
emfd_news_sentences <- readRDS("data/emfd_news/emfd_news_sentences.rds")

##1.8.German Election Programs (2002-2021) - Voit et al. 2024 ####

files <- list.files("data/german_election_programs", pattern = "\\.txt$", full.names = TRUE)
ger_programs <- tibble(path = character(), text = character())

for (i in seq_along(files)) {
 text <- paste(readLines(files[i], warn = FALSE, encoding = "latin1"), collapse = "\n")
 path <- files[i]  
 ger_programs %<>% 
   bind_rows(tibble(path = path, text = text))
}

ger_programs %<>% 
  mutate(
    party = case_when(
      str_detect(path, "NPD")  ~ "NPD",
      str_detect(path, "AfD")  ~ "AfD",
      str_detect(path, "CDU")  ~ "CDU-CSU",
      str_detect(path, "SPD")  ~ "SPD",
      str_detect(path, "Grüne")  ~ "Grüne",
      str_detect(path, "Linke")  ~ "Linke",
      str_detect(path, "FDP")  ~ "FDP"
    ),
    date = case_when(
      str_detect(path, "2002")  ~ ymd("2002-09-22"),
      str_detect(path, "2005")  ~ ymd("2005-09-18"),
      str_detect(path, "2009")  ~ ymd("2009-09-27"),
      str_detect(path, "2013")  ~ ymd("2013-09-22"),
      str_detect(path, "2017")  ~ ymd("2017-09-24"),
      str_detect(path, "2021")  ~ ymd("2021-09-26"),
      str_detect(path, "2025")  ~ ymd("2025-02-23")
    ),
    text = str_remove_all(text, "\\*") %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace_all("(?<!\n)\n(?!\n)", " ") %>% 
    str_replace_all("\n{2,}", "\n\n") %>%        
    str_squish()
  )

ger_programs %<>%
  mutate(
    text_id = paste0("ection_programs_", lubridate::year(date), "_", party), #data.table overwrites year function, to resolve
    author_id = party,
    date = date,
    text_length = str_length(text),
    party = party
  ) %>%
  select(text_id, text, author_id, date, text_length, party) 


#split into sentences
docs_gen <- en_md$pipe(ger_programs$text)

docs <- iterate(docs_gen)

ger_programs_sentences <- map2_dfr(
  ger_programs$text_id,
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

ger_programs_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

#add context (+/- 1 sentence)
ger_programs_sentences %<>% 
  group_by(text_id) %>%
  mutate(
    context = paste(lag(sentence, 1), sentence, lead(sentence, 1)) %>% 
      str_remove_all("^NA|NA$")
  ) %>%
  ungroup() 

#join with speech data
ger_programs_sentences %<>% 
  left_join(ger_programs) 

#remove objects from environment
rm(docs)
rm(docs_gen)
rm(ger_programs)

#write to file 
saveRDS(ger_programs_sentences, "data/german_election_programs/ger_programs_sentences.rds")

###load pre-processed files ----
ger_programs_sentences <- readRDS("data/german_election_programs/ger_programs_sentences.rds")
