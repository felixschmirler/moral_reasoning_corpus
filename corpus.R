#Script to create a corpus across different sources and topics for annotations


#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(lsa) #for cosine similarity function
#library(dictvectoR) devtools::install_github("thieled/dictvectoR") #explore package for DDR from https://github.com/thieled/dictvectoR
library(httr)
library(jsonlite)

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()
#py_list_packages()

#import spaCy 
spacy <- import("spacy")
ger_md <- spacy$load("de_core_news_md") #mostly to split speeches into sentences
en_md <- spacy$load("en_core_web_md") #mostly to split speeches into sentences
#senter <- spacy$load("xx_sent_ud_sm")  #tested senter as an alternative for sentence splitting but didn't work at all on german parliamentary speeches

#import sentence BERT models
st <- import("sentence_transformers")$SentenceTransformer
#all_mini <- st("all-MiniLM-L6-v2")  # Or "all-mpnet-base-v2" for more precision
multi_lang <- st("paraphrase-multilingual-mpnet-base-v2") # multilingual model
#util_st <- import("sentence_transformers.util")

#1. Data ----
#1.1. Open Discourse Corpus - Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####

#contributions_extended <- readRDS("open_discourse_corpus/RDS/contributions_extended.RDS") #initially not relevant
#contributions_simplified <- readRDS("open_discourse_corpus/RDS/contributions_simplified.RDS") #initially not relevant
#electoral_terms <- readRDS("open_discourse_corpus/RDS/electoral_terms.RDS") #initially not relevant
factions <- readRDS("open_discourse_corpus/RDS/factions.RDS") 
politicians <- readRDS("open_discourse_corpus/RDS/politicians.RDS") 
speeches <- readRDS("open_discourse_corpus/RDS/speeches.RDS")

#filter speeches 
speeches_filtered <- speeches

#TO DO: filter out duplicates ----
speeches %>% select(-session, -id, -document_url) %>% nrow()
#919523

speeches %>% select(-id, -session, -electoral_term, -faction_id, -politician_id, -document_url, -position_short, -position_long, -date) %>% distinct() %>% nrow()
#896596

speeches %>% select(speech_content) %>% distinct() %>% nrow()
#794197


#filter out speeches from neutral positions and guests
speeches_filtered %>% count(position_short)

speeches_filtered %<>% filter(position_short != "Guest", 
                              position_short != "Not found",
                              position_short != "Presidium of Parliament")


#create variables to filter speeches relevant to topics of interest
speeches_filtered %<>% 
  mutate(
    speech_content = str_to_lower(speech_content),
    speech_length = str_length(speech_content),
    climate_count = str_count(speech_content, "klima"),
    covid_count = str_count(speech_content, "covid|corona|pandemie"),
    migration_count = str_count(speech_content, "migration|flüchtling"),
    war_count = str_count(speech_content, "krieg")
  ) 

#reduce to speeches since 2015 (migration debate, other option could be german unification)
speeches_filtered %<>% filter(date >= as.Date("2015-01-01")) #as.Date("1990-12-20")) 

#initially filter out any speeches that don't touch on any of the topics above
speeches_filtered %<>%
  mutate(helper_filter = climate_count + covid_count + migration_count + war_count) %>%
  filter(helper_filter > 0)


#To ensure the corpus captures a wide range of institutional moral reasoning, we included speeches by the Federal Chancellor and Cabinet Ministers. While such speeches are less frequent and more formalised than typical parliamentary interventions, they often articulate key normative positions. To avoid overrepresentation, we stratified our sampling by speaker role and controlled for speech length and frequency.

#filter out very long speeches and short comments
speeches_filtered %>%
  ggplot(aes(speech_length)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 15000))

quantile(speeches_filtered$speech_length, c(0, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 25% and bottom 25%
speeches_filtered %<>% filter(speech_length > 3020, speech_length < 5404)

#filter out speeches that are not highly relevant (top 97.5th percentile) for at least one of the topics

#climate
speeches_filtered %>%
  ggplot(aes(climate_count)) +
  geom_bar() +
  coord_cartesian(xlim = c(0, 20))

quantile(speeches_filtered$climate_count, c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99))

speeches_filtered %<>% mutate(min8_climate = climate_count >= 8)

#covid
speeches_filtered %>%
  ggplot(aes(covid_count)) +
  geom_bar() +
  coord_cartesian(xlim = c(0, 20))

quantile(speeches_filtered$covid_count, c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99))

speeches_filtered %<>% mutate(min6_covid = covid_count >= 6)

#migration
speeches_filtered %>%
  ggplot(aes(migration_count)) +
  geom_bar() +
  coord_cartesian(xlim = c(0, 20))

quantile(speeches_filtered$migration_count, c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99))

speeches_filtered %<>% mutate(min6_migration = migration_count >= 6)

#war
speeches_filtered %>%
  ggplot(aes(war_count)) +
  geom_bar() +
  coord_cartesian(xlim = c(0, 20))

quantile(speeches_filtered$war_count, c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99))

speeches_filtered %<>% mutate(min5_war = war_count >= 5)

#filter out speeches not relevant for one of the topics
speeches_filtered %<>%
  filter(min8_climate | min6_covid | min6_migration | min5_war) 

#almost no overlap between speeches, 250-300 speeches per topic
speeches_filtered %>% count(min8_climate, min6_covid, min6_migration, min5_war)

#add politician and party information to the dataset
speeches_filtered %<>% 
  left_join(politicians, join_by(politician_id == id)) 

speeches_filtered %<>% 
  filter(politician_id != - 1)

speeches_filtered %<>% 
  left_join(factions, join_by(faction_id == id))

speeches_filtered %<>% filter(full_name != "Fraktionslos", full_name != "not found")

#split speeches into sentence level - review ids and check for better ways to split sentences ----
speeches_sentences <- speeches_filtered %>%
  mutate(sentences = map2(id, speech_content, function(x, y) {
    doc <- ger_md(y)
    sents <- iterate(doc$sents) 
    data.frame(
      speech_id = x,
      sentence_id = seq_along(sents),
      sentence = sapply(sents, function(z) z$text),
      stringsAsFactors = FALSE
    )
  })) %>%
  unnest(sentences)

speeches_sentences %<>%
  mutate(
    sentence_length = str_length(sentence)
  )

txt <- c(
  "Dr. Müller kam um 10.30 Uhr an. Z. B. gestern. Okay?",
  "Prof. Smith arrived at 10.30 a.m. “Great—right?” Yes!"
)
split_sentences_ud(txt)

#filter out very long speeches and short comments
speeches_sentences %>%
  ggplot(aes(sentence_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 500))

quantile(speeches_sentences$sentence_length, c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 25% and bottom 25%
speeches_sentences %<>% filter(sentence_length > 48, sentence_length < 145)


#write to file 
saveRDS(speeches_sentences, "speeches_sentences.rds")
speeches_sentences <- readRDS("speeches_sentences.rds")

#sentence embeddings from multilanguage model 
sentence_embeddings_multilang <- multi_lang$encode(speeches_sentences$sentence)
saveRDS(sentence_embeddings_multilang, "sentence_embeddings_multilang.rds")
sentence_embeddings_multilang <- readRDS("sentence_embeddings_multilang.rds")

#1.2. MFT Reddit Corpus - Trager et al., 2022 ####
mft_reddit <- read_csv("mft_reddit_corpus/final_mfrc_data.csv")

#TO DO: filter out duplicates ----

mft_reddit_test <- mft_reddit %>% 
  select(text, subreddit, bucket) %>%
  distinct()

mft_reddit_test %<>%
  mutate(
    comment_id = row_number(),
  )

#TO DO: split comments into sentence level - review ids and check for better ways to split sentences ----
comments_sentences <- mft_reddit_test %>%
  mutate(sentences = map2(comment_id, text, function(x, y) {
    doc <- en_md(y)
    sents <- iterate(doc$sents) 
    data.frame(
      comment_id2 = x,
      sentence_id = seq_along(sents),
      sentence = sapply(sents, function(z) z$text),
      stringsAsFactors = FALSE
    )
  })) %>%
  unnest(sentences)

#write to file 
saveRDS(comments_sentences, "comments_sentences.rds")
comments_sentences <- readRDS("comments_sentences.rds")

#sentence embeddings from multilanguage model 
sentence_embeddings_reddit_multilang <- multi_lang$encode(comments_sentences$sentence)

#write to file
saveRDS(sentence_embeddings_reddit_multilang, "sentence_embeddings_reddit_multilang.rds")
sentence_embeddings_reddit_multilang <- readRDS("sentence_embeddings_reddit_multilang.rds")

#2. Distributed Dictionary Representations (Garten et al., 2018) with dynamic embeddings for sampling  ----

#cosine similarity function - needs reference and triple checking/ potentially not needed later
cosine_similarity <- function(x, y) {
  x_norms <- sqrt(rowSums(x^2))
  y_norm <- sqrt(sum(y^2))
  dot_products <- x %*% y
  sims <- dot_products / (x_norms * y_norm)
  as.numeric(sims)
}

#load LWIC dictionaries for deontology and utilitarianism (Wheeler & Laham, 2016) 
dict <- read_csv("dictionary/moral-justification-dictionary.csv")

#2.1. Full dictionaries ---- 
deont <- dict %>% filter(Deontology == "X") %>% pull(DicTerm) %>% str_remove("\\*")
conseq <- dict %>% filter(Consequentialism == "X") %>% pull(DicTerm) %>% str_remove("\\*")

#word embeddings for dictionaries
deont_embeddings <- multi_lang$encode(deont)
conseq_embeddings <- multi_lang$encode(conseq)

#compute the average vector (DDR representation)
deont_ddr_vector <- apply(deont_embeddings, 2, mean)
conseq_ddr_vector <- apply(conseq_embeddings, 2, mean)

#calculate cosine similarity
cosine(deont_ddr_vector, conseq_ddr_vector) #fairly high but might be due to nature of word lists

#add cosine similarity to data ----tbd

#2.2. Seed dictionaries ----
#future plan to run cluster analysis or pca but for now based on expert opinion 
deont_seed_en <- c("duty", "rights", "norm", "principle")
conseq_seed_en <- c("result", "consequence", "advantage", "disadvantage")

#get Sentence-BERT embeddings for english seed dictionaries
deont_seed_embeddings <- multi_lang$encode(deont_seed_en)
conseq_seed_embeddings <- multi_lang$encode(conseq_seed_en)

#compute the average vector (DDR representation)
deont_seed_ddr_vector <- apply(deont_seed_embeddings, 2, mean)
conseq_seed_ddr_vector <- apply(conseq_seed_embeddings, 2, mean)

#calculate cosine similarity
cosine(deont_seed_ddr_vector, conseq_seed_ddr_vector) #less similar for seed dictionaries

#add cosine similarity to data ----tbd

#2.3. expanding the deontology seed dictionary to sentences ----
response_deont <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-4.1",
    messages = list(
      list(role = "system", content = "You are a research assistent that helps 
           to build and annotate a text corpus that consists of social media data 
           and political speeches with information about moral reasoning. Moral 
           reasoning refers to the application of reasoning that either justifies 
           actions based on principles (deontology) or based on the consequences 
           (consequentialism/utilitarianism). 
           
           Definitions:
           Deontological reasoning — moral judgments based on universal rules, 
           duties, rights, or principles, regardless of the consequences.  
           Utilitarian/Consequentialist reasoning — moral judgments based on 
           expected outcomes, overall welfare, or utility.
           
           Dictionaries: 
           Deontological: duty, rights, norm, principle  
           Utilitarian: result, consequence, advantage, disadvantage  
          
           In this first step we want to expand on a seed dictionary that has 
           been validated to capture terms relatet to these constructs by creating 
           multiple example sentences in the context of political debates rather
           than just single words. The sentences should be 
           
           1. representative of the constructs as indicated by the definitions 
           and the terms from the dictionary 
           2. realistic and typical for sentences used in these debates rather 
           than artificial sentences not used by humans
           3. cover a variety of different expressions rather than giving only 
           narrow examples of the single best example"),
      list(role = "user", content = "Please use your instruction and create example
      sentences that are common for deontological/ rule-based reasoning in 
      political speeches. Please create 10 example sentences for each of the 
      following topics:
           
      1. reasoning that supports action against climate change
      2. reasoning that doesn't support actions against climate change
      3. reasoning that supports immigration
      4. reasoning that doesn't support immigration
      5. reasoning that supports covid restrictions
      6. reasoning that doesn't support covid restrictions
      7. reasoning to support other countries in war
      8. reasoning to not support other countries in war
           ")
    ),
    temperature = 0
  )
)

raw_text <- rawToChar(response_deont$content)

# Parse JSON to a list
parsed <- fromJSON(raw_text)

deont_sentence_examples <- parsed$choices$message$content

deont_sentence_examples %>% view()

writeLines(deont_sentence_examples, "test_sentences_deont.txt")

deont_sentences <- readLines("test_sentences_deont.txt") 
deont_sentences %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s")
  ) %>% 
  pull(value)

#expanding the consequentialism seed dictionary to sentences
response_conseq <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-4.1",
    messages = list(
      list(role = "system", content = "You are a research assistent that helps 
           to build and annotate a text corpus that consists of social media data 
           and political speeches with information about moral reasoning. Moral 
           reasoning refers to the application of reasoning that either justifies 
           actions based on principles (deontology) or based on the consequences 
           (consequentialism/utilitarianism). 
           
           Definitions:
           Deontological reasoning — moral judgments based on universal rules, 
           duties, rights, or principles, regardless of the consequences.  
           Utilitarian/Consequentialist reasoning — moral judgments based on 
           expected outcomes, overall welfare, or utility.
           
           Dictionaries: 
           Deontological: duty, rights, norm, principle  
           Utilitarian: result, consequence, advantage, disadvantage  
          
           In this first step we want to expand on a seed dictionary that has 
           been validated to capture terms relatet to these constructs by creating 
           multiple example sentences in the context of political debates rather
           than just single words. The sentences should be 
           
           1. representative of the constructs as indicated by the definitions 
           and the terms from the dictionary 
           2. realistic and typical for sentences used in these debates rather 
           than artificial sentences not used by humans
           3. cover a variety of different expressions rather than giving only 
           narrow examples of the single best example"),
      list(role = "user", content = "Please use your instruction and create example
      sentences that are common for consequentialist/ outcome-based reasoning in 
      political speeches. Please create 10 example sentences for each of the 
      following topics:
           
      1. reasoning that supports action against climate change
      2. reasoning that doesn't support actions against climate change
      3. reasoning that supports immigration
      4. reasoning that doesn't support immigration
      5. reasoning that supports covid restrictions
      6. reasoning that doesn't support covid restrictions
      7. reasoning to support other countries in war
      8. reasoning to not support other countries in war
           ")
    ),
    temperature = 0
  )
)

raw_text <- rawToChar(response_conseq$content)

# Parse JSON to a list
parsed <- fromJSON(raw_text)

conseq_sentence_examples <- parsed$choices$message$content

conseq_sentence_examples %>% view()

writeLines(conseq_sentence_examples, "test_sentences_conseq.txt")

conseq_sentences <- readLines("test_sentences_conseq.txt") 
conseq_sentences %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s")
  ) %>% 
  pull(value)


#get sentence-BERT embeddings for english example sentences with multilanguage model
deont_sentences_en <- multi_lang$encode(deont_sentences)
conseq_sentences_en <- multi_lang$encode(conseq_sentences)

#compute the average vector (DDR representation)
deont_sentence_ddr_vector <- apply(deont_sentences_en, 2, mean)
conseq_sentence_ddr_vector <- apply(conseq_sentences_en, 2, mean)

#just checking the magnitude of differences between the two vectors
cosine_similarity(deont_sentence_ddr_vector, conseq_sentence_ddr_vector)


#need to find a better solution for adding the scores ----
deont_scores <- cosine_similarity(sentence_embeddings_multilang, deont_ddr_vector)
conseq_scores <- cosine_similarity(sentence_embeddings_multilang, conseq_ddr_vector)

deont_seed_scores <- cosine_similarity(sentence_embeddings_multilang, deont_seed_ddr_vector)
conseq_seed_scores <- cosine_similarity(sentence_embeddings_multilang, conseq_seed_ddr_vector)

deont_sentence_scores <- cosine_similarity(sentence_embeddings_multilang, deont_sentence_ddr_vector)
conseq_sentence_scores <- cosine_similarity(sentence_embeddings_multilang, conseq_sentence_ddr_vector)

speeches_sentences %<>% add_column(deont_scores, conseq_scores, deont_seed_scores, conseq_seed_scores, deont_sentence_scores, conseq_sentence_scores) 



#3. quick and dirty filtering for first prototype ----
top_100_deont <- speeches_sentences %>% 
  mutate(label = "deontological") %>% 
  slice_max(deont_sentence_scores, n = 100, with_ties = FALSE)
top_100_conseq <- speeches_sentences %>% 
  mutate(label = "consequentialist") %>% 
  slice_max(conseq_sentence_scores, n = 100, with_ties = FALSE)
neutral_100 <- speeches_sentences %>% 
  mutate(label = "neutral") %>% 
  filter(deont_sentence_scores < 0.1, conseq_sentence_scores < 0.1)
test_data <- rbind(top_100_conseq, top_100_deont, neutral_100)
test_data_short <- test_data %>% 
  mutate(
    dataset = "open discourse"
  ) %>% select(doc_id = speech_id, sentence_id, dataset, label, sentence) 


#cosine similarity for reddit dataset
deont_sentence_reddit_scores <- cosine_similarity(sentence_embeddings_reddit_multilang, deont_sentence_ddr_vector)
conseq_sentence_reddit_scores <- cosine_similarity(sentence_embeddings_reddit_multilang, conseq_sentence_ddr_vector)

comments_sentences %<>% add_column(deont_sentence_reddit_scores, conseq_sentence_reddit_scores) 

#quick and dirty filtering for first prototype
top_100_deont_reddit <- comments_sentences %>% 
  mutate(label = "deontological") %>% 
  slice_max(deont_sentence_reddit_scores, n = 100, with_ties = FALSE)
top_100_conseq_reddit <- comments_sentences %>% 
  mutate(label = "consequentialist") %>% 
  slice_max(conseq_sentence_reddit_scores, n = 100, with_ties = FALSE)
neutral_100_reddit <- comments_sentences %>% 
  mutate(label = "neutral") %>% 
  filter(deont_sentence_reddit_scores < 0.01,  deont_sentence_reddit_scores > -0.01, 
         conseq_sentence_reddit_scores < 0.01, conseq_sentence_reddit_scores > - 0.01)
test_data2 <- rbind(top_100_conseq_reddit, top_100_deont_reddit, neutral_100_reddit)
test_data2_short <- test_data2 %>% 
  mutate(
    dataset = "mft reddit"
  ) %>% select(doc_id = comment_id, sentence_id, dataset, label, sentence) 

test_data_combined <- rbind(test_data_short, test_data2_short)

#trying quick fix for context variable (e.g. +/- one sentence) 
speech_context <- speeches_sentences %>% select(doc_id = speech_id, sentence_id, context = speech_content)
comment_context <- comments_sentences %>% select(doc_id = comment_id, sentence_id, context = text)
contexts <- rbind(speech_context, comment_context)
test_data_combined %<>% left_join(contexts) 

write_excel_csv(test_data_combined, "test_data_short.csv")
test_data_combined <- read_csv("test_data_short.csv")
