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

#create score ranks
open_discourse_sentences_s %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(open_discourse_sentences_s, "data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s_ddr.rds")

open_discourse_sentences_s_ddr <- readRDS("data/german_bundestag/open_discourse_corpus/open_discourse_sentences_s_ddr.rds")

#explore distributions
open_discourse_sentences_s_ddr %>%
  ggplot(aes(sent_conseq_main)) +
  geom_histogram()

open_discourse_sentences_s_ddr %>% summary()

open_discourse_sentences_s_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(open_discourse_sentences_s_ddr$sent_conseq_main, open_discourse_sentences_s_ddr$sent_deont_main)
cor(open_discourse_sentences_s_ddr$sent_deont_pre, open_discourse_sentences_s_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- open_discourse_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_deont_main > .73) %>% # ~ 7k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- open_discourse_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .64) %>% # ~33k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600)%>%
  mutate(sample = "outcome-based")

#no target
notarget_sample <- open_discourse_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300)%>%
  mutate(sample = "no target")

#combine
open_discourse_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^- ") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) 

saveRDS(open_discourse_sample, "corpus/open_discourse_sample.rds")
write_excel_csv(open_discourse_sample, "corpus/open_discourse_sample.csv")


##2.2. US Congress (pre 2016 - Gentzkow et al; post 2016 - Judd et al) ----

#load data
us_congress_sentences_s <- readRDS("data/us_congress/uscongress_sentences_c_s.rds")

#embeddings
us_congress_embed1 <- rbind(
  readRDS("data/us_congress/uscongress_combined_embed_1m_c.rds"),
  readRDS("data/us_congress/uscongress_combined_embed_2m_c.rds"),
  readRDS("data/us_congress/uscongress_combined_embed_3m_c.rds")
)

us_congress_embed2 <- rbind(
  readRDS("data/us_congress/uscongress_combined_embed_5m_c.rds"),
  readRDS("data/us_congress/uscongress_combined_embed_6m_c.rds"),
  readRDS("data/us_congress/uscongress_combined_embed_7m_c.rds")
)

#split sentences due to size of embeddings

us_congress_sentences_s_1 <- us_congress_sentences_s[1:nrow(us_congress_embed1),]
us_congress_sentences_s_2 <- us_congress_sentences_s[(nrow(us_congress_embed1)+1):nrow(us_congress_sentences_s),]

#cosine similiarity to deontology sentences main
cosim_sent_deont_main1 <- apply(us_congress_embed1, 1, cos_sim, sentences_deont_main_ddr)
us_congress_sentences_s_1$sent_deont_main <- cosim_sent_deont_main1
rm(cosim_sent_deont_main1)

cosim_sent_deont_main2 <- apply(us_congress_embed2, 1, cos_sim, sentences_deont_main_ddr)
us_congress_sentences_s_2$sent_deont_main <- cosim_sent_deont_main2
rm(cosim_sent_deont_main2)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre1 <- apply(us_congress_embed1, 1, cos_sim, sentences_deont_pre_ddr)
us_congress_sentences_s_1$sent_deont_pre <- cosim_sent_deont_pre1
rm(cosim_sent_deont_pre1)

cosim_sent_deont_pre2 <- apply(us_congress_embed2, 1, cos_sim, sentences_deont_pre_ddr)
us_congress_sentences_s_2$sent_deont_pre <- cosim_sent_deont_pre2
rm(cosim_sent_deont_pre2)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main1 <- apply(us_congress_embed1, 1, cos_sim, sentences_conseq_main_ddr)
us_congress_sentences_s_1$sent_conseq_main <- cosim_sent_conseq_main1
rm(cosim_sent_conseq_main1)

cosim_sent_conseq_main2 <- apply(us_congress_embed2, 1, cos_sim, sentences_conseq_main_ddr)
us_congress_sentences_s_2$sent_conseq_main <- cosim_sent_conseq_main2
rm(cosim_sent_conseq_main2)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre1 <- apply(us_congress_embed1, 1, cos_sim, sentences_conseq_pre_ddr)
us_congress_sentences_s_1$sent_conseq_pre <- cosim_sent_conseq_pre1
rm(cosim_sent_conseq_pre1)

cosim_sent_conseq_pre2 <- apply(us_congress_embed2, 1, cos_sim, sentences_conseq_pre_ddr)
us_congress_sentences_s_2$sent_conseq_pre <- cosim_sent_conseq_pre2
rm(cosim_sent_conseq_pre2)

#combine dataset again
us_congress_sentences_s <- rbind(us_congress_sentences_s_1, us_congress_sentences_s_2)
rm(us_congress_sentences_s_1, us_congress_sentences_s_2, us_congress_embed1, us_congress_embed2)

#create score ranks
us_congress_sentences_s %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(us_congress_sentences_s, "data/us_congress/us_congress_sentences_s_ddr.rds")

us_congress_sentences_s_ddr <- readRDS("data/us_congress/us_congress_sentences_s_ddr.rds")

#explore distributions
us_congress_sentences_s_ddr %>%
  ggplot(aes(sent_conseq_main)) +
  geom_histogram()

us_congress_sentences_s_ddr %>% summary()

us_congress_sentences_s_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(us_congress_sentences_s_ddr$sent_conseq_main, us_congress_sentences_s_ddr$sent_deont_main)
cor(us_congress_sentences_s_ddr$sent_deont_pre, us_congress_sentences_s_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- us_congress_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_deont_main > .73) %>%  # ~ 1k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- us_congress_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .64) %>% # ~10k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600)%>%
  mutate(sample = "outcome-based")

#no target
notarget_sample <- us_congress_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300)%>%
  mutate(sample = "no target")

#combine
us_congress_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^- ") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) 

saveRDS(us_congress_sample, "corpus/us_congress_sample.rds")
write_excel_csv(us_congress_sample, "corpus/us_congress_sample.csv")

##2.3. ParlSpeech V2 (UK Parliament) - Rauh & Schwalbach, 2020 ----

#load data
uk_parliament_sentences_s <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_s.rds")

#embeddings
uk_parliament_embed1 <- rbind(
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_3m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_4m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_5m.rds")
)

uk_parliament_embed2 <- rbind(
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_6m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_7m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_8m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_9m.rds")
)

#split sentences due to size of embeddings

uk_parliament_sentences_s_1 <- uk_parliament_sentences_s[1:nrow(uk_parliament_embed1),]
uk_parliament_sentences_s_2 <- uk_parliament_sentences_s[(nrow(uk_parliament_embed1)+1):nrow(uk_parliament_sentences_s),]

#cosine similiarity to deontology sentences main
cosim_sent_deont_main1 <- apply(uk_parliament_embed1, 1, cos_sim, sentences_deont_main_ddr)
uk_parliament_sentences_s_1$sent_deont_main <- cosim_sent_deont_main1
rm(cosim_sent_deont_main1)

cosim_sent_deont_main2 <- apply(uk_parliament_embed2, 1, cos_sim, sentences_deont_main_ddr)
uk_parliament_sentences_s_2$sent_deont_main <- cosim_sent_deont_main2
rm(cosim_sent_deont_main2)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre1 <- apply(uk_parliament_embed1, 1, cos_sim, sentences_deont_pre_ddr)
uk_parliament_sentences_s_1$sent_deont_pre <- cosim_sent_deont_pre1
rm(cosim_sent_deont_pre1)

cosim_sent_deont_pre2 <- apply(uk_parliament_embed2, 1, cos_sim, sentences_deont_pre_ddr)
uk_parliament_sentences_s_2$sent_deont_pre <- cosim_sent_deont_pre2
rm(cosim_sent_deont_pre2)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main1 <- apply(uk_parliament_embed1, 1, cos_sim, sentences_conseq_main_ddr)
uk_parliament_sentences_s_1$sent_conseq_main <- cosim_sent_conseq_main1
rm(cosim_sent_conseq_main1)

cosim_sent_conseq_main2 <- apply(uk_parliament_embed2, 1, cos_sim, sentences_conseq_main_ddr)
uk_parliament_sentences_s_2$sent_conseq_main <- cosim_sent_conseq_main2
rm(cosim_sent_conseq_main2)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre1 <- apply(uk_parliament_embed1, 1, cos_sim, sentences_conseq_pre_ddr)
uk_parliament_sentences_s_1$sent_conseq_pre <- cosim_sent_conseq_pre1
rm(cosim_sent_conseq_pre1)

cosim_sent_conseq_pre2 <- apply(uk_parliament_embed2, 1, cos_sim, sentences_conseq_pre_ddr)
uk_parliament_sentences_s_2$sent_conseq_pre <- cosim_sent_conseq_pre2
rm(cosim_sent_conseq_pre2)

#combine dataset again
uk_parliament_sentences_s <- rbind(uk_parliament_sentences_s_1, uk_parliament_sentences_s_2)
rm(uk_parliament_sentences_s_1, uk_parliament_sentences_s_2, uk_parliament_embed1, uk_parliament_embed2)

#create score ranks
uk_parliament_sentences_s %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(uk_parliament_sentences_s, "data/uk_parliament/uk_parliament_sentences_s_ddr.rds")

uk_parliament_sentences_s_ddr <- readRDS("data/uk_parliament/uk_parliament_sentences_s_ddr.rds")

#explore distributions
uk_parliament_sentences_s_ddr %>%
  ggplot(aes(sent_conseq_main)) +
  geom_histogram()

uk_parliament_sentences_s_ddr %>% summary()

uk_parliament_sentences_s_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(uk_parliament_sentences_s_ddr$sent_conseq_main, uk_parliament_sentences_s_ddr$sent_deont_main)
cor(uk_parliament_sentences_s_ddr$sent_deont_pre, uk_parliament_sentences_s_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- uk_parliament_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_deont_main > .73) %>%  # ~ 1k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- uk_parliament_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .64) %>% # ~10k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600)%>%
  mutate(sample = "outcome-based")

#no target
notarget_sample <- uk_parliament_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300)%>%
  mutate(sample = "no target")

#combine
uk_parliament_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^- ") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) 

saveRDS(uk_parliament_sample, "corpus/uk_parliament_sample.rds")
write_excel_csv(uk_parliament_sample, "corpus/uk_parliament_sample.csv")
