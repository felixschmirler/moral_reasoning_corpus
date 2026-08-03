#Script to split german reddit data (Blombach et al., 2020) into sentences ahead of pre-sampling and annotation

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

###load pre-processed file 
gerede_politics_comments <- readRDS("data/german_reddit_corpus/gerede_politics_comments.rds")

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
