#Distributed Dictionary Representations Method (Garten et al., 2018) 
#with contextualised embeddings for sampling

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(irr) #Krippendorff's alpha

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()

#import sentence BERT model
st <- import("sentence_transformers")$SentenceTransformer
#all_mini <- st("all-MiniLM-L6-v2")  # Or "all-mpnet-base-v2" for more precision
sbert_multiling <- st("paraphrase-multilingual-mpnet-base-v2") # multilingual model
#util_st <- import("sentence_transformers.util")

##1. LIWC dictionaries ---- 

#load LWIC dictionaries for deontology and utilitarianism (Wheeler & Laham, 2016) 
dict_deont_full <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Deontology == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

dict_conseq_full <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Consequentialism == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

#load reduced LWIC dictionaries for deontology and consequentialism 
dict_deont_core <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Deontology_Core == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

dict_conseq_core <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Consequentialism_Core == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

dict_deont_seed <- c("rules", "duties", "rights", "prohibitions")
dict_conseq_seed <- c("consequences", "outcomes",  "benefits", "costs")

##2. Exemplary Sentences ---- 

###2.1. Load Pre-study Sentences ----

#load sentences generated with gpt-4.1 via the open AI api 
#deontology
sentences_deont_pre <- readLines("content/test_sentences_deont.txt") 
sentences_deont_pre %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s"),
    value = str_remove(value, '"')
  ) %>% 
  pull(value)

#consequentialism
sentences_conseq_pre <- readLines("content/test_sentences_conseq.txt") 
sentences_conseq_pre %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s"),
    value = str_remove(value, '"')
  ) %>% 
  pull(value)

###2.2. Sentences created for the main study with gpt-5.2 via the open AI api ----

#read deontological sentences from file 
sentences_deont_main <- readRDS("content/sentences_deont_main.rds")

#read consequentialist sentences from file 
sentences_conseq_main <- readRDS("content/sentences_conseq_main.rds")

#3. Create embeddings & average vectors ----

##3.1. deontology & consequentialism ----

#seed dictionaries
dict_deont_seed_embed <- sbert_multiling$encode(dict_deont_seed)
dict_deont_seed_ddr <- apply(dict_deont_seed_embed, 2, mean)

dict_conseq_seed_embed <- sbert_multiling$encode(dict_conseq_seed)
dict_conseq_seed_ddr <- apply(dict_conseq_seed_embed, 2, mean)

#core dictionaries
dict_deont_core_embed <- sbert_multiling$encode(dict_deont_core)
dict_deont_core_ddr <- apply(dict_deont_core_embed, 2, mean)

dict_conseq_core_embed <- sbert_multiling$encode(dict_conseq_core)
dict_conseq_core_ddr <- apply(dict_conseq_core_embed, 2, mean)

#full dictionaries
dict_deont_full_embed <- sbert_multiling$encode(dict_deont_full)
dict_deont_full_ddr <- apply(dict_deont_full_embed, 2, mean)

dict_conseq_full_embed <- sbert_multiling$encode(dict_conseq_full)
dict_conseq_full_ddr <- apply(dict_conseq_full_embed, 2, mean)

#pre study sentences
sentences_deont_pre_embed <- sbert_multiling$encode(sentences_deont_pre)
sentences_deont_pre_ddr <- apply(sentences_deont_pre_embed, 2, mean)
saveRDS(sentences_deont_pre_ddr, "content/sentences_deont_pre_ddr.rds")

sentences_conseq_pre_embed <- sbert_multiling$encode(sentences_conseq_pre)
sentences_conseq_pre_ddr <- apply(sentences_conseq_pre_embed, 2, mean)
saveRDS(sentences_conseq_pre_ddr, "content/sentences_conseq_pre_ddr.rds")

#main study sentences
sentences_deont_main_embed <- sbert_multiling$encode(sentences_deont_main$text)
sentences_deont_main_ddr <- apply(sentences_deont_main_embed, 2, mean)

#write to file
saveRDS(sentences_deont_main_ddr, "content/sentences_deont_main_ddr.rds")
sentences_deont_main_ddr <- readRDS("content/sentences_deont_main_ddr.rds")

sentences_conseq_main_embed <- sbert_multiling$encode(sentences_conseq_main$text)
sentences_conseq_main_ddr <- apply(sentences_conseq_main_embed, 2, mean)

#write to file
saveRDS(sentences_conseq_main_ddr, "content/sentences_conseq_main_ddr_rds")
sentences_conseq_main_ddr <- readRDS("content/sentences_conseq_main_ddr_rds")

##3.2.further vectors by topic, language etc. within sentences ----
sentences_main_all <- rbind(sentences_deont_main, sentences_conseq_main)

#stance
sentences_for_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(stance == "for") 
  %>% pull(text)
  )
sentences_for_ddr <- apply(sentences_for_embed, 2, mean)

sentences_against_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(stance == "against") 
  %>% pull(text)
)
sentences_against_ddr <- apply(sentences_against_embed, 2, mean)

#language
sentences_eng_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(language == "English") 
  %>% pull(text)
)
sentences_eng_ddr <- apply(sentences_eng_embed, 2, mean)

sentences_ger_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(language == "German") 
  %>% pull(text)
)
sentences_ger_ddr <- apply(sentences_ger_embed, 2, mean)

#style
sentences_social_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(style == "social media") 
  %>% pull(text)
)
sentences_social_ddr <- apply(sentences_social_embed, 2, mean)

sentences_news_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(style == "newspaper") 
  %>% pull(text)
)
sentences_news_ddr <- apply(sentences_news_embed, 2, mean)

sentences_politics_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(style == "politics") 
  %>% pull(text)
)
sentences_politics_ddr <- apply(sentences_politics_embed, 2, mean)

#tense
sentences_past_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(tense == "past") 
  %>% pull(text)
)
sentences_past_ddr <- apply(sentences_past_embed, 2, mean)

sentences_future_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(tense == "future") 
  %>% pull(text)
)
sentences_future_ddr <- apply(sentences_future_embed, 2, mean)

sentences_present_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(tense == "present") 
  %>% pull(text)
)
sentences_present_ddr <- apply(sentences_present_embed, 2, mean)

#topic
sentences_t0_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t0_notopic") 
  %>% pull(text)
)
sentences_t0_ddr <- apply(sentences_t0_embed, 2, mean)

sentences_t1_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t1_climate") 
  %>% pull(text)
)
sentences_t1_ddr <- apply(sentences_t1_embed, 2, mean)

sentences_t2_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t2_covid") 
  %>% pull(text)
)
sentences_t2_ddr <- apply(sentences_t2_embed, 2, mean)

sentences_t3_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t3_immigration") 
  %>% pull(text)
)
sentences_t3_ddr <- apply(sentences_t3_embed, 2, mean)

sentences_t4_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t4_militarysupport") 
  %>% pull(text)
)
sentences_t4_ddr <- apply(sentences_t4_embed, 2, mean)

sentences_t5_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t5_sanctions") 
  %>% pull(text)
)
sentences_t5_ddr <- apply(sentences_t5_embed, 2, mean)

sentences_t6_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t6_genderequality") 
  %>% pull(text)
)
sentences_t6_ddr <- apply(sentences_t6_embed, 2, mean)

sentences_t7_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t7_ethincity") 
  %>% pull(text)
)
sentences_t7_ddr <- apply(sentences_t7_embed, 2, mean)

sentences_t8_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t8_lgbtqrights") 
  %>% pull(text)
)
sentences_t8_ddr <- apply(sentences_t8_embed, 2, mean)

sentences_t9_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t9_hatespeech") 
  %>% pull(text)
)
sentences_t9_ddr <- apply(sentences_t9_embed, 2, mean)

sentences_t10_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t10_socialwelfare") 
  %>% pull(text)
)
sentences_t10_ddr <- apply(sentences_t10_embed, 2, mean)

sentences_t11_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t11_taxation") 
  %>% pull(text)
)
sentences_t11_ddr <- apply(sentences_t11_embed, 2, mean)

sentences_t12_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t12_abortionlaws") 
  %>% pull(text)
)
sentences_t12_ddr <- apply(sentences_t12_embed, 2, mean)

sentences_t13_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t13_referendums") 
  %>% pull(text)
)
sentences_t13_ddr <- apply(sentences_t13_embed, 2, mean)

sentences_t14_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t14_beautyideals") 
  %>% pull(text)
)
sentences_t14_ddr <- apply(sentences_t14_embed, 2, mean)

sentences_t15_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t15_meat") 
  %>% pull(text)
)
sentences_t15_ddr <- apply(sentences_t15_embed, 2, mean)

sentences_t16_embed <- sbert_multiling$encode(
  sentences_main_all %>% 
    filter(topic == "t16_religion") 
  %>% pull(text)
)
sentences_t16_ddr <- apply(sentences_t16_embed, 2, mean)

#4. Explore cosine similarity  ----

#Cosine Similarity - Teitelbaum & Simchon (2025)
cos_sim <- function(x, y){  
  dot <- x %*% y  
  normx <- sqrt(sum(x^2))  
  normy <- sqrt(sum(y^2))  
  as.vector( dot / (normx*normy) )  
  }

##4.1. between construct similarity/ "discriminant validity" ----

#seed dictionaries
cos_sim(dict_deont_seed_ddr, dict_conseq_seed_ddr) #0.4875429


#core dictionaries
cos_sim(dict_deont_core_ddr, dict_conseq_core_ddr) #0.663562


#full dictionaries
cos_sim(dict_deont_full_ddr, dict_conseq_full_ddr) #0.7038378


#pre study sentences
cos_sim(sentences_deont_pre_ddr, sentences_conseq_pre_ddr) #0.6002762


#main study sentences
cos_sim(sentences_deont_main_ddr, sentences_conseq_main_ddr) #0.6711041


##4.2. within construct similarity/ "internal consistency" ----

###4.2.1 deontology

#deontology - dict seed
cos_sim(dict_deont_seed_ddr, dict_deont_core_ddr) #0.9214693
cos_sim(dict_deont_seed_ddr, dict_deont_full_ddr) #0.8972324
cos_sim(dict_deont_seed_ddr, sentences_deont_pre_ddr) #0.6091986
cos_sim(dict_deont_seed_ddr, sentences_deont_main_ddr) #0.6668121

#deontology - dict core
cos_sim(dict_deont_core_ddr, dict_deont_full_ddr) #0.9545042
cos_sim(dict_deont_core_ddr, sentences_deont_pre_ddr) #0.6007679
cos_sim(dict_deont_core_ddr, sentences_deont_main_ddr) #0.6358025

#deontology - dict full
cos_sim(dict_deont_full_ddr, sentences_deont_pre_ddr) #0.5804898
cos_sim(dict_deont_full_ddr, sentences_deont_main_ddr) #0.6287212

#deontology - sentences pre
cos_sim(sentences_deont_pre_ddr, sentences_deont_main_ddr) #0.8866646

###4.2.2 consequentialism

#consequentialism - dict seed
cos_sim(dict_conseq_seed_ddr, dict_conseq_core_ddr) #0.8374133
cos_sim(dict_conseq_seed_ddr, dict_conseq_full_ddr) #0.8121827
cos_sim(dict_conseq_seed_ddr, sentences_conseq_pre_ddr) #0.515327
cos_sim(dict_conseq_seed_ddr, sentences_conseq_main_ddr) #0.5732755

#consequentialism - dict core
cos_sim(dict_conseq_core_ddr, dict_conseq_full_ddr) #0.9749984
cos_sim(dict_conseq_core_ddr, sentences_conseq_pre_ddr) #0.4526247
cos_sim(dict_conseq_core_ddr, sentences_conseq_main_ddr) #0.4865344

#consequentialism - dict full
cos_sim(dict_conseq_full_ddr, sentences_conseq_pre_ddr) #0.435711
cos_sim(dict_conseq_full_ddr, sentences_conseq_main_ddr) #0.4699336

#consequentialism - sentences pre
cos_sim(sentences_conseq_pre_ddr, sentences_conseq_main_ddr) #0.8967789

##4.3. Explore similarities between topics, languages, styles etc. for sentences ----

#language 
cos_sim(sentences_eng_ddr, sentences_ger_ddr) #0.9803894

#stance 
cos_sim(sentences_for_ddr, sentences_against_ddr) #0.8759859

#style 
cos_sim(sentences_social_ddr, sentences_politics_ddr) #0.9485579
cos_sim(sentences_social_ddr, sentences_news_ddr) #0.8800009
cos_sim(sentences_politics_ddr, sentences_news_ddr) #0.9332967

#tense 
cos_sim(sentences_present_ddr, sentences_future_ddr) #0.9426222
cos_sim(sentences_present_ddr, sentences_past_ddr) #0.9083975
cos_sim(sentences_future_ddr, sentences_past_ddr) #0.872857

#topics
cos_sim(sentences_t0_ddr, sentences_t1_ddr) #0.8388712
cos_sim(sentences_t0_ddr, sentences_t2_ddr) #0.882947
cos_sim(sentences_t0_ddr, sentences_t3_ddr) #0.8253898
cos_sim(sentences_t0_ddr, sentences_t4_ddr) #0.8052788
cos_sim(sentences_t0_ddr, sentences_t5_ddr) #0.8444093
cos_sim(sentences_t0_ddr, sentences_t6_ddr) #0.8367222
cos_sim(sentences_t0_ddr, sentences_t7_ddr) #0.8047988
cos_sim(sentences_t0_ddr, sentences_t8_ddr) #0.8541767
cos_sim(sentences_t0_ddr, sentences_t9_ddr) #0.805733
cos_sim(sentences_t0_ddr, sentences_t10_ddr) #0.8050058
cos_sim(sentences_t0_ddr, sentences_t11_ddr) #0.7720081
cos_sim(sentences_t0_ddr, sentences_t12_ddr) #0.8680292
cos_sim(sentences_t0_ddr, sentences_t13_ddr) #0.8722397
cos_sim(sentences_t0_ddr, sentences_t14_ddr) #0.8374626
cos_sim(sentences_t0_ddr, sentences_t15_ddr) #0.7362483
cos_sim(sentences_t0_ddr, sentences_t16_ddr) #0.8101762

cos_sim(sentences_t1_ddr, sentences_t2_ddr) #0.8352036
cos_sim(sentences_t1_ddr, sentences_t3_ddr) #0.8008765
cos_sim(sentences_t1_ddr, sentences_t4_ddr) #0.6975395
cos_sim(sentences_t1_ddr, sentences_t5_ddr) #0.77785
cos_sim(sentences_t1_ddr, sentences_t6_ddr) #0.792294
cos_sim(sentences_t1_ddr, sentences_t7_ddr) #0.7398655
cos_sim(sentences_t1_ddr, sentences_t8_ddr) #0.7744938
cos_sim(sentences_t1_ddr, sentences_t9_ddr) #0.711592
cos_sim(sentences_t1_ddr, sentences_t10_ddr) #0.8266384
cos_sim(sentences_t1_ddr, sentences_t11_ddr) #0.8331704
cos_sim(sentences_t1_ddr, sentences_t12_ddr) #0.7757081
cos_sim(sentences_t1_ddr, sentences_t13_ddr) #0.812717
cos_sim(sentences_t1_ddr, sentences_t14_ddr) #0.7209723
cos_sim(sentences_t1_ddr, sentences_t15_ddr) #0.7577158
cos_sim(sentences_t1_ddr, sentences_t16_ddr) #0.729561

cos_sim(sentences_t2_ddr, sentences_t3_ddr) #0.8676041
cos_sim(sentences_t2_ddr, sentences_t4_ddr) #0.7710061
cos_sim(sentences_t2_ddr, sentences_t5_ddr) #0.8342928
cos_sim(sentences_t2_ddr, sentences_t6_ddr) #0.8345012
cos_sim(sentences_t2_ddr, sentences_t7_ddr) #0.8027667
cos_sim(sentences_t2_ddr, sentences_t8_ddr) #0.8675299
cos_sim(sentences_t2_ddr, sentences_t9_ddr) #0.8347761
cos_sim(sentences_t2_ddr, sentences_t10_ddr) #0.8747604
cos_sim(sentences_t2_ddr, sentences_t11_ddr) #0.7615905
cos_sim(sentences_t2_ddr, sentences_t12_ddr) #0.8864045
cos_sim(sentences_t2_ddr, sentences_t13_ddr) #0.8034645
cos_sim(sentences_t2_ddr, sentences_t14_ddr) #0.8539083
cos_sim(sentences_t2_ddr, sentences_t15_ddr) #0.7433694
cos_sim(sentences_t2_ddr, sentences_t16_ddr) #0.823094

cos_sim(sentences_t3_ddr, sentences_t4_ddr) #0.756943
cos_sim(sentences_t3_ddr, sentences_t5_ddr) #0.8130909
cos_sim(sentences_t3_ddr, sentences_t6_ddr) #0.8394122
cos_sim(sentences_t3_ddr, sentences_t7_ddr) #0.8412961
cos_sim(sentences_t3_ddr, sentences_t8_ddr) #0.8587851
cos_sim(sentences_t3_ddr, sentences_t9_ddr) #0.7588804
cos_sim(sentences_t3_ddr, sentences_t10_ddr) #0.8525633
cos_sim(sentences_t3_ddr, sentences_t11_ddr) #0.765488
cos_sim(sentences_t3_ddr, sentences_t12_ddr) #0.8250617
cos_sim(sentences_t3_ddr, sentences_t13_ddr) #0.8279272
cos_sim(sentences_t3_ddr, sentences_t14_ddr) #0.7457971
cos_sim(sentences_t3_ddr, sentences_t15_ddr) #0.6642904
cos_sim(sentences_t3_ddr, sentences_t16_ddr) #0.8338384

#5. Criterion Validity ---- 

#load labelled data from pre study
pre_study <- read_csv("content/prestudy_data.csv", col_select = 1:6)

##5.1. compare synthetic data and natural data ----

#retrieve word embeddings for pre study sentences: all
pre_study_embed <- sbert_multiling$encode(pre_study$sentence)
pre_study_ddr <- apply(pre_study_embed, 2, mean)

#retrieve word embeddings for generated sentences: all
main_study_embed <- sbert_multiling$encode(sentences_main_all$text)
main_study_ddr <- apply(main_study_embed, 2, mean)

#compare consine similarity between synthetic and natural data
cos_sim(pre_study_ddr, main_study_ddr) #0.9019951

#retrieve word embeddings for pre study sentences: deontology
pre_study_deont_embed <- sbert_multiling$encode(
  pre_study %>%
    filter(deontology_majority == 1) %>%
    pull(sentence)
    )
pre_study_deont_ddr <- apply(pre_study_deont_embed, 2, mean)

#compare consine similarity between synthetic and natural data: deontology
cos_sim(pre_study_deont_ddr, sentences_deont_main_ddr) #0.9188292

#retrieve word embeddings for pre study sentences: consequentialism
pre_study_conseq_embed <- sbert_multiling$encode(
  pre_study %>%
    filter(consequentialism_majority == 1) %>%
    pull(sentence)
)
pre_study_conseq_ddr <- apply(pre_study_conseq_embed, 2, mean)

#compare consine similarity between synthetic and natural data: consequentialism
cos_sim(pre_study_conseq_ddr, sentences_conseq_main_ddr) #0.8929092

#retrieve word embeddings for pre study sentences: social media
pre_study_social_embed <- sbert_multiling$encode(
  pre_study %>%
    filter(str_detect(id, "mft")) %>%
    pull(sentence)
)
pre_study_social_ddr <- apply(pre_study_social_embed, 2, mean)

#compare consine similarity between synthetic and natural data: social media
cos_sim(pre_study_social_ddr, sentences_social_ddr) #0.9132879

#retrieve word embeddings for pre study sentences: politics
pre_study_politics_embed <- sbert_multiling$encode(
  pre_study %>%
    filter(str_detect(id, "open")) %>%
    pull(sentence)
)
pre_study_politics_ddr <- apply(pre_study_politics_embed, 2, mean)

#compare consine similarity between synthetic and natural data: politics
cos_sim(pre_study_politics_ddr, sentences_politics_ddr) #0.8883638

##5.2. calculate cosine similarity to different ddr vectors ----

#deontology sentences main
cosim_sent_deont_main <- apply(pre_study_embed, 1, cos_sim, sentences_deont_main_ddr)
pre_study$sent_deont_main <- cosim_sent_deont_main

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_deont = mean(sent_deont_main))

#deontology sentences pre
cosim_sent_deont_pre <- apply(pre_study_embed, 1, cos_sim, sentences_deont_pre_ddr)
pre_study$sent_deont_pre <- cosim_sent_deont_pre

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_deont = mean(sent_deont_pre))

#deontology full dictionary
cosim_dict_deont_full <- apply(pre_study_embed, 1, cos_sim, dict_deont_full_ddr)
pre_study$dict_deont_full <- cosim_dict_deont_full

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_deont = mean(dict_deont_full))

#deontology core dictionary
cosim_dict_deont_core <- apply(pre_study_embed, 1, cos_sim, dict_deont_core_ddr)
pre_study$dict_deont_core <- cosim_dict_deont_core

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_deont = mean(dict_deont_core))

#deontology seed dictionary
cosim_dict_deont_seed <- apply(pre_study_embed, 1, cos_sim, dict_deont_seed_ddr)
pre_study$dict_deont_seed <- cosim_dict_deont_seed

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_deont = mean(dict_deont_seed))

#consequentialism sentences main
cosim_sent_conseq_main <- apply(pre_study_embed, 1, cos_sim, sentences_conseq_main_ddr)
pre_study$sent_conseq_main <- cosim_sent_conseq_main

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_conseq = mean(sent_conseq_main))

#consequentialism sentences pre
cosim_sent_conseq_pre <- apply(pre_study_embed, 1, cos_sim, sentences_conseq_pre_ddr)
pre_study$sent_conseq_pre <- cosim_sent_conseq_pre

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_conseq = mean(sent_conseq_pre))

#consequentialism full dictionary
cosim_dict_conseq_full <- apply(pre_study_embed, 1, cos_sim, dict_conseq_full_ddr)
pre_study$dict_conseq_full <- cosim_dict_conseq_full

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_conseq = mean(dict_conseq_full))

#consequentialism core dictionary
cosim_dict_conseq_core <- apply(pre_study_embed, 1, cos_sim, dict_conseq_core_ddr)
pre_study$dict_conseq_core <- cosim_dict_conseq_core

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_conseq = mean(dict_conseq_core))

#consequentialism seed dictionary
cosim_dict_conseq_seed <- apply(pre_study_embed, 1, cos_sim, dict_conseq_seed_ddr)
pre_study$dict_conseq_seed <- cosim_dict_conseq_seed

pre_study %>% 
  group_by(multiclass_label) %>%
  summarise(av_conseq = mean(dict_conseq_seed))



##5.3. point-biserial correlations ----

#deontology
cor(pre_study$deontology_majority, pre_study$sent_deont_main, method = "pearson") #0.5779202
cor(pre_study$deontology_majority, pre_study$sent_deont_pre, method = "pearson") #0.6199682
cor(pre_study$deontology_majority, pre_study$dict_deont_full, method = "pearson") #0.5593538
cor(pre_study$deontology_majority, pre_study$dict_deont_core, method = "pearson") #0.5848614
cor(pre_study$deontology_majority, pre_study$dict_deont_seed, method = "pearson") #0.6124191

#consequentialism
cor(pre_study$consequentialism_majority, pre_study$sent_conseq_main, method = "pearson") #0.6286407
cor(pre_study$consequentialism_majority, pre_study$sent_conseq_pre, method = "pearson") #0.6309026
cor(pre_study$consequentialism_majority, pre_study$dict_conseq_full, method = "pearson") #0.4031094
cor(pre_study$consequentialism_majority, pre_study$dict_conseq_core, method = "pearson") #0.4283869
cor(pre_study$consequentialism_majority, pre_study$dict_conseq_seed, method = "pearson") #0.5873526

#neutral
cor(pre_study$neutral_majority, pre_study$sent_conseq_main, method = "pearson") #-0.6796928
cor(pre_study$neutral_majority, pre_study$sent_conseq_pre, method = "pearson") #-0.692944

cor(pre_study$neutral_majority, pre_study$sent_deont_main, method = "pearson") #-0.6644662
cor(pre_study$neutral_majority, pre_study$sent_deont_pre, method = "pearson") #-0.6452136

###5.3.1. MCC ----
cor(pre_study$deontology_majority, pre_study$deontology_cosim_main, method = "pearson") #0.5940144
cor(pre_study$deontology_majority, pre_study$deontology_cosim_pre, method = "pearson") #0.6290625
cor(pre_study$consequentialism_majority, pre_study$consequentialism_cosim_main, method = "pearson") #0.6248342
cor(pre_study$consequentialism_majority, pre_study$consequentialism_cosim_pre, method = "pearson") #0.6222895

##5.4. Precision, Recall, F1-Score exploratory based on cosine similarity cut-off ----

#read file ----
pre_study <- readRDS("content/pre_study_ddr.rds") 

#logistic regression for cut-off points

#deont main
model_deont_main <- glm(deontology_majority ~ sent_deont_main,
                        data = pre_study,
                        family = binomial(link = "logit"))

summary(model_deont_main)

b0 <- coef(model_deont_main)[1]
b1 <- coef(model_deont_main)[2]

p <- 0.8
cutoff_x_08 <- (log(p / (1 - p)) - b0) / b1
cutoff_x_08 # 0.7145314

#deont pre
model_deont_pre <- glm(deontology_majority ~ sent_deont_pre,
                        data = pre_study,
                        family = binomial(link = "logit"))

summary(model_deont_pre)

b0 <- coef(model_deont_pre)[1]
b1 <- coef(model_deont_pre)[2]

p <- 0.8
cutoff_x_08 <- (log(p / (1 - p)) - b0) / b1
cutoff_x_08 # 0.7299026 

#plot for slide
ggplot(pre_study, aes(sent_deont_pre, deontology_majority)) +
  geom_point(size = 4, alpha = 0.2) +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              colour = "blue",
              
              linewidth = 1.2) +
  labs(
    x = "Cosine similarity to rule-based examples",
    y = "Probability that a sentence is rule-based"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  ) +
  geom_vline(xintercept = cutoff_x,
             colour = "red",
             linewidth = 1) +
  annotate("text",
           x = cutoff_x,
           y = 0.55,
           label = paste0("   p = .5 at x = ",
                          round(cutoff_x, 2)),
           colour = "red",
           hjust = -0.1) + 
  ggtitle("Classification of rule-based sentences")

#conseq main
model_conseq_main <- glm(consequentialism_majority ~ sent_conseq_main,
                        data = pre_study,
                        family = binomial(link = "logit"))

summary(model_conseq_main)

b0 <- coef(model_conseq_main)[1]
b1 <- coef(model_conseq_main)[2]

p <- 0.8
cutoff_x_08 <- (log(p / (1 - p)) - b0) / b1
cutoff_x_08 # 0.623824 

#conseq pre
model_conseq_pre <- glm(consequentialism_majority ~ sent_conseq_pre,
                       data = pre_study,
                       family = binomial(link = "logit"))

summary(model_conseq_pre)

b0 <- coef(model_conseq_pre)[1]
b1 <- coef(model_conseq_pre)[2]

p <- 0.8
cutoff_x_08 <- (log(p / (1 - p)) - b0) / b1
cutoff_x_08 # 0.636774 

#plot for slide
ggplot(pre_study, aes(sent_conseq_pre, consequentialism_majority)) +
  geom_point(size = 4, alpha = 0.2) +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              colour = "blue",
              
              linewidth = 1.2) +
  labs(
    x = "Cosine similarity to outcome-based examples",
    y = "Probability that a sentence is outcome-based"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  ) +
  geom_vline(xintercept = cutoff_x,
             colour = "red",
             linewidth = 1) +
  annotate("text",
           x = cutoff_x,
           y = 0.55,
           label = paste0("   p = .5 at x = ",
                          round(cutoff_x, 2)),
           colour = "red",
           hjust = -0.1) + 
  ggtitle("Classification of outcome-based sentences")

#create labels based on cosine_similarity cut-offs
pre_study %<>%
  mutate(
    deontology_cosim_main = if_else(#sent_deont_main > sent_conseq_main &
                                      sent_deont_main > 0.6108675, #logistic regression p = 0.5 
                                    1, 0),
    deontology_cosim_pre = if_else(#sent_deont_pre > sent_conseq_pre &
                                     sent_deont_pre > 0.6238497, #logistic regression p = 0.5                                               ,
                                   1, 0),
    consequentialism_cosim_main = if_else(#sent_conseq_main > sent_deont_main &
                                            sent_conseq_main > 0.539263, #logistic regression p = 0.5
                                          1, 0),
    consequentialism_cosim_pre = if_else(#sent_conseq_pre > sent_deont_pre &
                                           sent_conseq_pre > 0.5555583, #logistic regression p = 0.5
                                         1, 0),
    
  )

#write to file
saveRDS(pre_study, "content/pre_study_ddr.rds")
write_excel_csv(pre_study, "content/pre_study_ddr.csv")

#read file ----
pre_study <- readRDS("content/pre_study_ddr.rds") 


#deontology 

#main
TP_d_m <- sum(pre_study$deontology_majority == 1 & pre_study$deontology_cosim_main == 1)
FP_d_m <- sum(pre_study$deontology_majority != 1 & pre_study$deontology_cosim_main == 1)
FN_d_m <- sum(pre_study$deontology_majority == 1 & pre_study$deontology_cosim_main != 1)

precision_d_m <- TP_d_m / (TP_d_m + FP_d_m)
recall_d_m    <- TP_d_m / (TP_d_m + FN_d_m)
f1_d_m        <- 2 * precision_d_m * recall_d_m / (precision_d_m + recall_d_m)

precision_d_m 
recall_d_m    
f1_d_m        

#pre
TP_d_p <- sum(pre_study$deontology_majority == 1 & pre_study$deontology_cosim_pre == 1)
FP_d_p <- sum(pre_study$deontology_majority != 1 & pre_study$deontology_cosim_pre == 1)
FN_d_p <- sum(pre_study$deontology_majority == 1 & pre_study$deontology_cosim_pre != 1)

precision_d_p <- TP_d_p / (TP_d_p + FP_d_p)
recall_d_p    <- TP_d_p / (TP_d_p + FN_d_p)
f1_d_p        <- 2 * precision_d_p * recall_d_p / (precision_d_p + recall_d_p)

precision_d_p 
recall_d_p    
f1_d_p        

#consequentialism 

#main
TP_c_m <- sum(pre_study$consequentialism_majority == 1 & pre_study$consequentialism_cosim_main == 1)
FP_c_m <- sum(pre_study$consequentialism_majority != 1 & pre_study$consequentialism_cosim_main == 1)
FN_c_m <- sum(pre_study$consequentialism_majority == 1 & pre_study$consequentialism_cosim_main != 1)

precision_c_m <- TP_c_m / (TP_c_m + FP_c_m)
recall_c_m    <- TP_c_m / (TP_c_m + FN_c_m)
f1_c_m        <- 2 * precision_c_m * recall_c_m / (precision_c_m + recall_c_m)

precision_c_m 
recall_c_m    
f1_c_m        

#pre
TP_c_p <- sum(pre_study$consequentialism_majority == 1 & pre_study$consequentialism_cosim_pre == 1)
FP_c_p <- sum(pre_study$consequentialism_majority != 1 & pre_study$consequentialism_cosim_pre == 1)
FN_c_p <- sum(pre_study$consequentialism_majority == 1 & pre_study$consequentialism_cosim_pre != 1)

precision_c_p <- TP_c_p / (TP_c_p + FP_c_p)
recall_c_p    <- TP_c_p / (TP_c_p + FN_c_p)
f1_c_p        <- 2 * precision_c_p * recall_c_p / (precision_c_p + recall_c_p)

precision_c_p 
recall_c_p    
f1_c_p    

##5.5. Interrater Reliability - Krippendorff's alpha ----
x <- pre_study %>%
  select(deontology_majority, deontology_cosim_pre)

# force atomic matrix (handles many tibble/list-column weirdness cases)
xm <- data.matrix(x)

xm <- t(xm)

kripp.alpha(xm, method = "nominal")

x <- pre_study %>%
  select(consequentialism_majority, consequentialism_cosim_pre)

# force atomic matrix (handles many tibble/list-column weirdness cases)
xm <- data.matrix(x)

xm <- t(xm)

kripp.alpha(xm, method = "nominal")

