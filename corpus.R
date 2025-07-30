#Script to create a corpus across different sources and topics for annotations

#load packages
library(tidyverse)
library(magrittr)
library(spacyr)
#spacy_install(lang_models = "de_core_news_md") #install medium model once


#Richter et al - Richter et al. - 2023 - Open Discourse Corpus ####

#contributions_extended <- readRDS("open_discourse_corpus/RDS/contributions_extended.RDS") #initially not relevant
#contributions_simplified <- readRDS("open_discourse_corpus/RDS/contributions_simplified.RDS") #initially not relevant
#electoral_terms <- readRDS("open_discourse_corpus/RDS/electoral_terms.RDS") #initially not relevant
factions <- readRDS("open_discourse_corpus/RDS/factions.RDS") 
politicians <- readRDS("open_discourse_corpus/RDS/politicians.RDS") 
speeches <- readRDS("open_discourse_corpus/RDS/speeches.RDS")

#filter speeches 

#reduce to speeches since german unification
speeches_post_unification <- speeches %>% filter(date >= as.Date("2015-01-02")) #as.Date("1990-12-20")) 

#create variables to filter speeches relevant to topics of interest
speeches_post_unification %<>% 
  mutate(
    speech_content = str_to_lower(speech_content),
    speech_length = str_length(speech_content),
    climate_count = str_count(speech_content, "klima"),
    covid_count = str_count(speech_content, "covid|corona|pandemie"),
    migration_count = str_count(speech_content, "migration|flüchtling"),
    war_count = str_count(speech_content, "krieg")
  ) 

#filter out any speeches that don't touch on any of the topics above
speeches_filtered <- speeches_post_unification %>%
  mutate(helper_filter = climate_count + covid_count + migration_count + war_count) %>%
  filter(helper_filter > 0)

#filter out speeches from neutral positions and guests
speeches_filtered %>% count(position_short)

speeches_filtered %<>% filter(position_short != "Guest", 
                             position_short != "Not found",
                             position_short != "Presidium of Parliament")

#To ensure the corpus captures a wide range of institutional moral reasoning, we included speeches by the Federal Chancellor and Cabinet Ministers. While such speeches are less frequent and more formalised than typical parliamentary interventions, they often articulate key normative positions. To avoid overrepresentation, we stratified our sampling by speaker role and controlled for speech length and frequency.

#filter out very long speeches and comments
speeches_filtered %>%
  ggplot(aes(speech_length)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 15000))

quantile(speeches_filtered$speech_length, c(0, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 1))

#filter out top 25% and bottom 25%
speeches_filtered %<>% filter(speech_length > 3020, speech_length < 5404)

#filter out speeches that are not highly relevant for one of the topics

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

#add politician and party information to the dataset
speeches_filtered %<>% 
  left_join(politicians, join_by(politician_id == id)) 

speeches_filtered %<>% 
  filter(politician_id != - 1)

speeches_filtered %<>% 
  left_join(factions, join_by(faction_id == id))

speeches_filtered %<>% filter(full_name != "Fraktionslos", full_name != "not found")

#split speeches into sentence level
spacy_initialize(model = "de_core_news_md")  # for German texts
results <- spacy_tokenize(speeches_filtered$speech_content,
                          what = "sentence",
                          output = "data.frame")

write_csv(results, "test_sentences.csv")
