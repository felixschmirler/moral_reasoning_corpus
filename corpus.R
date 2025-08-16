#Script to create a corpus across different sources and topics for annotations


#Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(lsa) #for cosine similarity function
library(httr)
library(jsonlite)

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()
py_list_packages()

#import spaCy 
spacy <- import("spacy")
ger_md <- spacy$load("de_core_news_md") #mostly to split speeches into sentences
en_md <- spacy$load("en_core_web_md") #mostly to split speeches into sentences


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
#sentence_embeddings <- all_mini$encode(speeches_sentences$sentence)
#saveRDS(sentence_embeddings, "sentence_embeddings.rds")

#sentence embeddings from multilanguage model 
sentence_embeddings_multilang <- multi_lang$encode(speeches_sentences$sentence)
saveRDS(sentence_embeddings_multilang, "sentence_embeddings_multilang.rds")

#MFT Reddit Corpus - Trager et al., 2022 ####
mft_reddit <- read_csv("mft_reddit_corpus/final_mfrc_data.csv")

mft_reddit_test <- mft_reddit %>% 
  select(text, subreddit, bucket) %>%
  distinct()

mft_reddit_test %<>%
  mutate(
    comment_id = row_number(),
  )

#split comments into sentence level
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

#sentence embeddings from multilanguage model 
sentence_embeddings_reddit_multilang <- multi_lang$encode(comments_sentences$sentence)
saveRDS(sentence_embeddings_reddit_multilang, "sentence_embeddings_reddit_multilang.rds")



#LWIC dictionaries for deontology and utilitarianism(Wheeler & Laham, 2016) ----
#dict <- read_csv("dictionary/moral-justification-dictionary.csv")
deont <- dict %>% filter(Deontology == "X") %>% pull(DicTerm) %>% str_remove("\\*")
conseq <- dict %>% filter(Consequentialism == "X") %>% pull(DicTerm) %>% str_remove("\\*")

#future plan to run cluster or pca but for now quick fix from chat-gpt
deont_seed_en <- c("duty", "rights", "norm", "principle")
conseq_seed_en <- c("result", "consequence", "advantage", "disadvantage")


#expanding the seed dictionaries to sentences

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

#manual vector creation based on above output
deont_sentences <- c(
  "We have a moral duty to protect the planet for future generations.",
  "It is our responsibility to uphold international agreements on environmental standards.",
  "Every citizen has a right to clean air and water, and we must defend that right.",
  "As stewards of the earth, it is our obligation to reduce harmful emissions.",
  "It is a matter of principle that we do not exploit natural resources irresponsibly.",
  "Our constitution guarantees the right to a healthy environment.",
  "We must act because it is simply the right thing to do, regardless of the cost.",
  "Respecting nature is a fundamental value of our society.",
  "We are bound by our ethical commitments to address climate change.",
  "It is unjust to ignore the harm our actions cause to vulnerable communities.",
  "We have a duty to protect our national sovereignty from external regulations.",
  "It is our right to develop our economy without undue interference.",
  "The principle of fairness means we should not bear an unfair share of the burden.",
  "Our primary obligation is to our citizens, not to international bodies.",
  "It is against our values to impose restrictions that harm our workers.",
  "We must respect the rights of individuals to choose their own lifestyles.",
  "It is not just to penalize our industries while others do nothing.",
  "Our constitution protects the right to private property and enterprise.",
  "We are committed to upholding the principle of economic freedom.",
  "It is a matter of national principle to resist policies that undermine our competitiveness.",
  "We have a moral obligation to offer refuge to those fleeing persecution.",
  "It is our duty to uphold the principle of equal treatment for all.",
  "Every human being has the right to seek a better life.",
  "Our nation was founded on the value of welcoming newcomers.",
  "Respecting human dignity means opening our doors to those in need.",
  "It is a matter of justice to reunite families separated by borders.",
  "We are bound by international law to protect asylum seekers.",
  "The principle of compassion guides our immigration policy.",
  "We must honor our commitments to human rights.",
  "It is simply the right thing to do to help those less fortunate.",
  "We have a duty to protect our borders and uphold the rule of law.",
  "It is our right as a nation to decide who may enter.",
  "The principle of national sovereignty must be respected.",
  "Our primary obligation is to safeguard the interests of our citizens.",
  "It is unjust to allow illegal entry while others wait their turn.",
  "We must uphold the integrity of our immigration system.",
  "It is a matter of principle to enforce our laws consistently.",
  "Our constitution grants us the authority to regulate immigration.",
  "We are committed to preserving our cultural values and traditions.",
  "It is not fair to those who follow the legal process to reward those who do not.",
  "We have a duty to protect public health above all else.",
  "It is our responsibility to follow the guidance of medical experts.",
  "Every person has the right to safety in public spaces.",
  "The principle of solidarity requires us to act for the common good.",
  "We must uphold the value of human life in all our decisions.",
  "It is a matter of justice to protect the most vulnerable among us.",
  "Our laws mandate that we take action in times of crisis.",
  "We are bound by our ethical obligations to prevent harm.",
  "Respecting the rights of others means adhering to public health measures.",
  "It is simply the right thing to do to wear masks and practice social distancing.",
  "We have a duty to defend individual freedoms and civil liberties.",
  "It is our right to make personal choices about our own health.",
  "The principle of autonomy must be respected, even in a crisis.",
  "Our constitution guarantees freedom of movement and assembly.",
  "It is unjust to impose blanket restrictions on everyone.",
  "We must uphold the rule of law and avoid arbitrary mandates.",
  "It is a matter of principle to protect privacy and bodily integrity.",
  "Our society values personal responsibility over government control.",
  "We are committed to defending the rights of small businesses.",
  "It is not right to sacrifice liberty for a false sense of security.",
  "We have a moral duty to defend our allies in times of need.",
  "It is our responsibility to uphold international law and order.",
  "Every nation has the right to self-determination, and we must support that.",
  "The principle of justice demands that we stand against aggression.",
  "We are bound by our treaty obligations to provide assistance.",
  "It is a matter of honor to keep our promises to our partners.",
  "Our values require us to oppose violations of human rights.",
  "We must act because it is the right thing to do, not just because it benefits us.",
  "Respecting the sovereignty of nations is a core principle of our foreign policy.",
  "We are committed to upholding the norms of the international community.",
  "We have a duty to prioritize peace and avoid unnecessary conflict.",
  "It is our right to remain neutral in foreign disputes.",
  "The principle of non-intervention must be respected.",
  "Our constitution prohibits engaging in wars without just cause.",
  "It is unjust to risk our soldiers’ lives for interests that are not our own.",
  "We must uphold the value of national sovereignty and self-determination.",
  "It is a matter of principle to avoid entangling alliances.",
  "Our laws require parliamentary approval before military action.",
  "We are committed to resolving conflicts through diplomacy, not force.",
  "It is not right to impose our will on other nations."
)


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

#manual vector creation based on above output
conseq_sentences <- c(
  "If we invest in renewable energy now, we will create millions of new jobs and boost our economy in the long run.",
  "Taking action to reduce emissions will prevent catastrophic weather events that could devastate our communities.",
  "By addressing climate change, we can avoid the enormous healthcare costs associated with pollution-related illnesses.",
  "The consequences of inaction will be rising sea levels, which threaten our coastal cities and infrastructure.",
  "Implementing stricter environmental regulations will ensure a safer, healthier future for our children.",
  "If we fail to act, the agricultural sector will suffer, leading to food shortages and higher prices.",
  "Transitioning to green technologies will give our country a competitive advantage in the global market.",
  "The benefits of reducing our carbon footprint far outweigh the short-term costs of policy changes.",
  "By leading on climate action, we can inspire other nations to follow suit, amplifying the positive impact worldwide.",
  "Investing in climate resilience now will save us billions in disaster relief and recovery expenses later.",
  "Imposing strict regulations on industry could result in massive job losses and economic decline.",
  "The costs of transitioning to renewable energy outweigh the potential benefits at this stage.",
  "If we implement these policies, energy prices will skyrocket, hurting low-income families the most.",
  "Drastic climate measures could make our businesses less competitive internationally.",
  "The economic disadvantages of these regulations will be felt by every household in the country.",
  "Restricting fossil fuel use now could lead to energy shortages and unreliable power supplies.",
  "The negative impact on our manufacturing sector would far exceed any environmental gains.",
  "These policies could drive investment and jobs overseas, weakening our economy.",
  "The immediate consequences of such drastic action would be increased unemployment and social unrest.",
  "The disadvantages of rapid decarbonization, such as economic instability, are too great to ignore.",
  "Allowing more immigrants will strengthen our workforce and drive economic growth.",
  "Immigrants contribute to innovation and help fill critical labor shortages in key industries.",
  "The overall benefits of a diverse society include increased creativity and global competitiveness.",
  "Welcoming refugees can enhance our international reputation and foster goodwill.",
  "The positive impact of immigration on our tax base helps fund essential public services.",
  "By accepting skilled migrants, we can address gaps in our healthcare and technology sectors.",
  "The long-term advantages of a multicultural society include greater resilience and adaptability.",
  "Immigrants often start businesses, creating jobs and boosting local economies.",
  "The economic gains from immigration far surpass the costs of integration programs.",
  "Supporting immigration leads to a more dynamic and prosperous society for everyone.",
  "High levels of immigration could strain our public services and infrastructure.",
  "The influx of migrants may result in lower wages and fewer job opportunities for local workers.",
  "The disadvantages of open borders include increased pressure on housing and schools.",
  "If we allow too many immigrants, social cohesion could be undermined, leading to instability.",
  "The economic costs of integrating large numbers of newcomers may outweigh the benefits.",
  "Uncontrolled immigration could lead to higher crime rates and security concerns.",
  "The negative consequences for our welfare system could be significant if immigration is not managed.",
  "Allowing more immigrants might result in cultural tensions and divisions within society.",
  "The potential burden on healthcare and social services is too great to ignore.",
  "The risks of increased unemployment and social unrest outweigh the possible advantages of higher immigration.",
  "Implementing restrictions will reduce transmission rates and save countless lives.",
  "The benefits of temporary lockdowns include preventing hospitals from being overwhelmed.",
  "By enforcing mask mandates, we can minimize the economic impact of widespread illness.",
  "The overall advantage of restrictions is a quicker return to normalcy for everyone.",
  "Limiting gatherings now will help us avoid more severe and prolonged disruptions later.",
  "The positive outcome of these measures is fewer deaths and less suffering in our communities.",
  "Early intervention with restrictions leads to better health and economic results in the long run.",
  "The short-term inconvenience of restrictions is justified by the long-term benefits to public health.",
  "By acting decisively, we can protect vulnerable populations and reduce the overall burden on society.",
  "The consequences of not imposing restrictions would be far more damaging to our economy and well-being.",
  "Prolonged restrictions could lead to severe economic downturn and widespread job losses.",
  "The negative impact on mental health from lockdowns outweighs the potential benefits.",
  "If we keep businesses closed, the long-term consequences for small enterprises will be devastating.",
  "The disadvantages of school closures include learning loss and increased inequality among students.",
  "The economic costs of restrictions are too high compared to the limited reduction in cases.",
  "Extended mandates could erode public trust and lead to non-compliance, undermining their effectiveness.",
  "The harm to personal freedoms and livelihoods is greater than the potential health benefits.",
  "The result of ongoing restrictions is increased poverty and social isolation.",
  "The negative outcomes for children and families make these measures unsustainable.",
  "The disadvantages of continued restrictions, such as rising unemployment, cannot be justified."
)


#under construction ----
deont_seed_sent_en <- c("duty", "rights", "norm", "principle")
conseq_seed_sent_en <- c("result", "consequence", "advantage", "disadvantage")

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
cosine(deont_seed_ddr_vector, conseq_seed_ddr_vector)

#get sentence-BERT embeddings for english example sentences with multilanguage model
deont_sentences_en <- multi_lang$encode(deont_sentences)
conseq_sentences_en <- multi_lang$encode(conseq_sentences)

#compute the average vector (DDR representation)
deont_sentence_ddr_vector <- apply(deont_sentences_en, 2, mean)
conseq_sentence_ddr_vector <- apply(conseq_sentences_en, 2, mean)

#just checking the magnitude of differences between the two vectors
cbind(deont_sentence_ddr_vector, conseq_sentence_ddr_vector) %>% as_tibble %>% mutate(diff = deont_sentence_ddr_vector - conseq_sentence_ddr_vector) %>% view()
cosine(deont_sentence_ddr_vector, conseq_sentence_ddr_vector)

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

deont_seed_scores <- cosine_similarity(sentence_embeddings_multilang, deont_seed_ddr_vector)
conseq_seed_scores <- cosine_similarity(sentence_embeddings_multilang, conseq_seed_ddr_vector)

deont_sentence_scores <- cosine_similarity(sentence_embeddings_multilang, deont_sentence_ddr_vector)
conseq_sentence_scores <- cosine_similarity(sentence_embeddings_multilang, conseq_sentence_ddr_vector)

speeches_sentences %<>% add_column(deont_scores, conseq_scores, deont_seed_scores, conseq_seed_scores, deont_sentence_scores, conseq_sentence_scores) 



#quick and dirty filtering for first prototype
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

write_excel_csv(test_data_combined, "test_data_short.csv")

#chat-gpt based sampling for prototype leveraging seed dictionaries to prompt and full dictionaries to prompt ----
api_key <- readLines("api_key.txt")

model   <- "gpt-4.1-mini"   # or your exact model name
temperature <- 0 # set temperature


# 1) System prompt: put ALL rules here (stable, reused across batches)
system_prompt <- "
=== DEFINITIONS ===
Deontological reasoning — moral judgments based on universal rules, duties, rights, or principles, regardless of the consequences.  
Utilitarian/Consequentialist reasoning — moral judgments based on expected outcomes, overall welfare, or utility.

=== DICTIONARIES (examples only) ===
Deontological: duty, rights, norm, principle  
Utilitarian: result, consequence, advantage, disadvantage  
Note: Variations depending on context (e.g., duty in context of climate change could be 'we need to protect nature at all costs' vs. consequentialist could look like 'we must act because climate change will destroy our harvest').

=== GUIDELINES ===
- Use the definitions first; the dictionaries are examples, not strict matchers.  
- Consider both English and German text.  
- Both constructs can be present; score each independently from 0.0 (no evidence) to 1.0 (clear and strong evidence).  
- If neither is clearly present, keep both scores low and set label to 'uncertain'.  
- Always output valid JSON in this schema without extra text:  
Output schema per sentence:
{
  \"score_deon\": number in [0,1],
  \"score_util\": number in [0,1],
  \"label\": \"deontological\" | \"utilitarian\" | \"uncertain\",
  \"confidence\": number in [0,1]
}

Decision rule:
- If score_deon - score_util >= 0.2 → label = \"deontological\"
- If score_util - score_deon >= 0.2 → label = \"utilitarian\"
- Else → \"uncertain\"
"

# 2) Build a tiny user message per batch (just the sentences)
build_user_prompt <- function(sentences) {
  paste0(
    "Return a JSON array (one object per sentence is very important, same order) matching the schema. \n\n",
    "If a sentence is unclear, still return an object with low scores and label 'uncertain'.\n\n",
    "Don't return any extra sentences, summaries etc. since the output is going straight into a dataframe. so max 20 outputs matching the number of sentences per batch",
    paste(sprintf("[%03d] %s", seq_along(sentences), sentences), collapse = "\n")
  )
}

# 3) Single API call for a batch
score_batch <- function(sentences, temperature = 0) {
  resp <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      temperature = temperature,
      max_tokens = 2000,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = build_user_prompt(sentences))
      )
    )
  )
  stop_for_status(resp)
  txt <- content(resp, as = "parsed", type = "application/json")$choices[[1]]$message$content
  fromJSON(txt, simplifyVector = TRUE)
}


# 4) Run over the whole corpus in batches
score_corpus <- function(all_sentences, batch_size = 20) {
  idx <- split(seq_along(all_sentences), ceiling(seq_along(all_sentences)/batch_size))
  out_list <- vector("list", length(idx))
  for (i in seq_along(idx)) {
    sents <- all_sentences[idx[[i]]]
    res   <- score_batch(sents)
    res$index <- idx[[i]]
    out_list[[i]] <- res
    message(sprintf("Batch %d/%d (%d items) ✓", i, length(idx), length(sents)))
  }
  # Reorder to original sequence
  out <- do.call(rbind, out_list)
  out[order(unlist(out$index)), c("score_deon","score_util","label","confidence")]
}

sentences <- enc2utf8(speeches_sentences$sentence)
results <- score_corpus(sentences, batch_size = 20)
# write.csv(results, "morality_scores.csv", row.names = FALSE)


#construction side ----
#defining a function to send a prompt to openAI api model options tested: gpt-3.5 turbo
score_morality <- function(prompt, model = "gpt-5", temperature = 0) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(
        list(role = "system", content = "
You are a precise and consistent annotator for a research project on moral reasoning in political discourse.

Your task is to analyse sentences and assign scores for two constructs:

Deontological reasoning — moral judgments based on universal rules, duties, rights, or principles, regardless of the consequences.

Utilitarian/Consequentialist reasoning — moral judgments based on expected outcomes, overall welfare, or utility.

Guidelines:

Use the definitions first; the dictionaries are examples, not strict matchers. 
Particularly look for context specific forms of the words in the dictionaries like in the following examples:
climate change: duty could be expressed as 'we need to protect nature at all costs' results on the other hand could be expressed as 'we need to act because climate change will destroy our harvest'
search for similar variations depending on contexts such as migration, covid and war

Both constructs can be present; score each independently from 0.0 (no evidence) to 1.0 (clear and strong evidence).

If neither is clearly present, keep both scores low and set label to 'uncertain'.

Consider both English and German text.

Always output valid JSON in the required schema without extra text.

Deontological examples: duty, rights, norm, principle
Utilitarian examples: result, consequence, advantage, disadvantage

Output schema per sentence:

  'score_deon': 0.0–1.0,
  'score_util': 0.0–1.0,
  'label': 'deontological' | 'utilitarian' | 'uncertain',
  'confidence': 0.0–1.0
  
'deontological' if score_deon − score_util ≥ 0.2

'utilitarian' if score_util − score_deon ≥ 0.2

use 'uncertain' otherwise"),
        list(role = "user", content = prompt)
      ),
      temperature = temperature
    )
  )
  
  content <- content(response, as = "parsed", type = "application/json")
  
  # Extract response text
  content$choices[[1]]$message$content
}


