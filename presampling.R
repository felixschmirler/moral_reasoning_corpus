#Script to pre sample the larger text corpora


#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness

#1.DDRs and Cosine Similarity Function

#load DDR vectors - main
sentences_deont_main_ddr <- readRDS("content/sentences_deont_main_ddr.rds")
sentences_conseq_main_ddr <- readRDS("content/sentences_conseq_main_ddr.rds")

#load DDR vectors - pre study
sentences_deont_pre_ddr <- readRDS("content/sentences_deont_pre_ddr.rds")
sentences_conseq_pre_ddr <- readRDS("content/sentences_conseq_pre_ddr.rds")

#Cosine Similarity - Teitelbaum & Simchon (2025)
cos_sim <- function(x, y){  
  dot <- x %*% y  
  normx <- sqrt(sum(x^2))  
  normy <- sqrt(sum(y^2))  
  as.vector( dot / (normx*normy) )  
}

#2. Data ----

##2.1. Open Discourse Corpus - Richter et al (2023) ----

#load data
open_discourse_sentences_s <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s.rds")

#embeddings
open_discourse_embed <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(open_discourse_embed, 1, cos_sim, sentences_deont_main_ddr)
open_discourse_sentences_s$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(open_discourse_embed, 1, cos_sim, sentences_deont_pre_ddr)
open_discourse_sentences_s$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(open_discourse_embed, 1, cos_sim, sentences_conseq_main_ddr)
open_discourse_sentences_s$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(open_discourse_embed, 1, cos_sim, sentences_conseq_pre_ddr)
open_discourse_sentences_s$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

saveRDS(open_discourse_sentences_s, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s_ddr.rds")

open_discourse_sentences_s_ddr <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s_ddr.rds")

#sample based on scores
open_discourse_sentences_s_ddr %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 


#explore distributions
open_discourse_sentences_s_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

open_discourse_sentences_s_ddr %>% summary()

open_discourse_sentences_s_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(open_discourse_sentences_s_ddr$sent_conseq_main, open_discourse_sentences_s_ddr$sent_deont_main)
cor(open_discourse_sentences_s_ddr$sent_deont_pre, open_discourse_sentences_s_ddr$sent_deont_main)

##1.2. US Congress ----

#load data
uscongress_combined_sentences <- readRDS("data/us_congress/uscongress_sentences.rds")

##1.3. UK Parliament ----

#load data
parlspeech_uk_sentences <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences.rds")

#filter out very long and very short speeches 
parlspeech_uk_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
    filter(
      text_length > 100,
      text_length <= quantile(text_length, 0.99)
    ) %>% 
  ggplot(aes(text_length)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 1000))

parlspeech_uk_sentences %>%
  distinct(text_id, .keep_all = TRUE) %>% 
  pull(text_length) %>%
  quantile(c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 
             0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 0.99, 1))

#filter out 1% longest speeches and speeches with less than 100 characters (~5%)
parlspeech_uk_sentences %<>% filter(text_length > 100, text_length < 9541) 

