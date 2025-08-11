#Script to create a corpus across different sources and topics for annotations


#Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(lsa) #for cosine similarity function

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()
py_list_packages()

#import spaCy 
spacy <- import("spacy")
ger_md <- spacy$load("de_core_news_md") #mostly to split speeches into sentences

#import sentence BERT models
st <- import("sentence_transformers")$SentenceTransformer
all_mini <- st("all-MiniLM-L6-v2")  # Or "all-mpnet-base-v2" for more precision
multi_lang <- st("paraphrase-multilingual-mpnet-base-v2") # multilingual model
#util_st <- import("sentence_transformers.util")

#ETHICS dataset: Hendrycks et al. - 2023 - Aligning AI With Shared Human Values

deont_ethics <- read_csv("ethics_dataset/deontology/deontology_train.csv")
util_ethics <- read_csv("ethics_dataset/utilitarianism/util_train.csv")

deont_ethics_1 <- deont_ethics %>% filter(label == 1) %>% mutate(concat = paste(scenario, excuse))

#Open Discourse Corpus - Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####

#contributions_extended <- readRDS("open_discourse_corpus/RDS/contributions_extended.RDS") #initially not relevant
#contributions_simplified <- readRDS("open_discourse_corpus/RDS/contributions_simplified.RDS") #initially not relevant
#electoral_terms <- readRDS("open_discourse_corpus/RDS/electoral_terms.RDS") #initially not relevant
factions <- readRDS("open_discourse_corpus/RDS/factions.RDS") 
politicians <- readRDS("open_discourse_corpus/RDS/politicians.RDS") 
speeches <- readRDS("open_discourse_corpus/RDS/speeches.RDS")

#filter speeches 
speeches_filtered <- speeches

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

#split speeches into sentence level
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

#get sentence BERT embeddings for speech sentences
sentence_embeddings <- all_mini$encode(speeches_sentences$sentence)
saveRDS(sentence_embeddings, "sentence_embeddings.rds")

#sentence embeddings from multilanguage model 
sentence_embeddings_multilang <- multi_lang$encode(speeches_sentences$sentence)


#LWIC dictionaries for deontology and utilitarianism(Wheeler & Laham, 2016) ----
#dict <- read_csv("dictionary/moral-justification-dictionary.csv")
deont <- dict %>% filter(Deontology == "X") %>% pull(DicTerm) %>% str_remove("\\*")
conseq <- dict %>% filter(Consequentialism == "X") %>% pull(DicTerm) %>% str_remove("\\*")

#future plan to run cluster or pca but for now quick fix from chat-gpt
deont_seed_en <- c("duty", "rights", "norm", "principle")
conseq_seed_en <- c("result", "consequence", "advantage", "disadvantage")


#too much hassle with translation and lemmatization, manual option instead
deont_words <- c(
  "akzeptabel", "Rechenschaftspflicht", "einhalten", "erlauben", "genehmigen",
  "verbieten", "Berufung", "Kodex", "Befehl", "Verpflichtung",
  "befolgen", "verpflichten", "Vertrag", "Konvention", "Brauch",
  "fordern", "gehorchen", "Doktrin", "Dogma", "Beitrag",
  "Pflicht", "Etikette", "untersagen", "grundlegend", "Richtlinie",
  "Gesetz", "Maxime", "Auftrag", "notwendig", "Norm",
  "obligatorisch", "Praxis", "vorschreiben", "Vorschrift", "Prinzip",
  "Verbannung", "Regelung", "erforderlich", "Anforderung", "Verantwortung",
  "Recht", "Rolle", "Regel", "Entscheidung", "sanktionieren",
  "Standard", "Tabu", "Aufgabe", "Grundsatz", "übertreten",
  "verletzen", "wesentlich"
)

deont_seed <- c(
  "Pflicht", "Prinzip", "Verantwortung", "Recht")

conseq_words <- c(
  "Vorteil", "Nutzen", "Folge", "Nachteil", "Wirkung", "auswirken", "gedeihen",
  "gewinnen", "Glück", "Gesundheit", "Interesse", "Verlust", "maximieren",
  "Ergebnis", "Schmerz", "schmerzen", "schmerzhaft", "Vergnügen", "erfolgreich",
  "Auswirkung", "Resultat", "Erfolg", "leiden", "Nützlichkeit", "Wohlstand",
  "Wohlfahrt", "Wohlbefinden"
)

conseq_seed <- c(
  "Nutzen", "Folge", "Auswirkung", "Resultat")


#Distributed Dictionary Representations (Garten et al., 2017) with dynamic embeddings for sampling  ----

#get Sentence-BERT embeddings for dictionaries
deont_embeddings <- all_mini$encode(deont_words)
conseq_embeddings <- all_mini$encode(conseq_words)

#compute the average vector (DDR representation)
deont_ddr_vector <- apply(deont_embeddings, 2, mean)
conseq_ddr_vector <- apply(conseq_embeddings, 2, mean)

#just checking the magnitude of differences between the two vectors
cbind(deont_ddr_vector, conseq_ddr_vector) %>% as_tibble %>% mutate(diff = deont_ddr_vector - conseq_ddr_vector) %>% view()

#get Sentence-BERT embeddings for german seed dictionaries
deont_seed_embeddings <- all_mini$encode(deont_seed)
conseq_seed_embeddings <- all_mini$encode(conseq_seed)

#compute the average vector (DDR representation)
deont_seed_ddr_vector <- apply(deont_seed_embeddings, 2, mean)
conseq_seed_ddr_vector <- apply(conseq_seed_embeddings, 2, mean)

#just checking the magnitude of differences between the two vectors
cbind(deont_seed_ddr_vector, conseq_seed_ddr_vector) %>% as_tibble %>% mutate(diff = deont_seed_ddr_vector - conseq_seed_ddr_vector) %>% view()

#calculate cosine similarity
cosine(deont_ddr_vector, conseq_ddr_vector) #fairly high but might be due to nature of word lists
cosine(deont_seed_ddr_vector, conseq_seed_ddr_vector) #better for seed dictionaries

#get sentence-BERT embeddings for english seed dictionariers with multilanguage model
deont_seed_en <- multi_lang$encode(deont_seed)
conseq_seed_en <- multi_lang$encode(conseq_seed)

#compute the average vector (DDR representation)
deont_seed_ddr_vector <- apply(deont_seed_en, 2, mean)
conseq_seed_ddr_vector <- apply(conseq_seed_en, 2, mean)

#just checking the magnitude of differences between the two vectors
cbind(deont_seed_ddr_vector, conseq_seed_ddr_vector) %>% as_tibble %>% mutate(diff = deont_seed_ddr_vector - conseq_seed_ddr_vector) %>% view()


#cosine similarity function from chat gpt
cosine_similarity <- function(x, y) {
  x_norms <- sqrt(rowSums(x^2))
  y_norm <- sqrt(sum(y^2))
  dot_products <- x %*% y
  sims <- dot_products / (x_norms * y_norm)
  as.numeric(sims)
}

deont_scores <- cosine_similarity(sentence_embeddings, deont_ddr_vector)
conseq_scores <- cosine_similarity(sentence_embeddings, conseq_ddr_vector)

deont_seed_scores <- cosine_similarity(sentence_embeddings, deont_seed_ddr_vector)
conseq_seed_scores <- cosine_similarity(sentence_embeddings, conseq_seed_ddr_vector)

speeches_sentences %<>% add_column(deont_scores, conseq_scores, deont_seed_scores, conseq_seed_scores) 

#try the same with embeddings from ethics dataset
#get Sentence-BERT embeddings for dictionaries
deont_ethics_embeddings <- all_mini$encode(deont_words)
conseq_ethics_embeddings <- all_mini$encode(conseq_words)