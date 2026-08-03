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
  ggplot(aes(sent_deont_main)) +
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
  filter(sent_deont_main > .71) %>% # ~ 14k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- open_discourse_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .62) %>% # ~58k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
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
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(open_discourse_sample, "corpus/open_discourse_sample.rds")
write_excel_csv(open_discourse_sample, "corpus/open_discourse_sample.csv")
readRDs(open_discourse_sample, "corpus/open_discourse_sample.rds")

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
  filter(sent_deont_main > .71) %>% # ~ 1.8k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- us_congress_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .62) %>% # ~16k sentences, threshold based on p = .8 from logistic regression model in pilot study
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
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(us_congress_sample, "corpus/us_congress_sample.rds")
write_excel_csv(us_congress_sample, "corpus/us_congress_sample.csv")

##2.3. ParlSpeech V2 (UK Parliament) - Rauh & Schwalbach, 2020 ----

#load data
uk_parliament_sentences_s <- readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_sentences_s.rds")

#embeddings
uk_parliament_embed1 <- rbind(
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_1m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_2m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_3m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_4m.rds")
)

uk_parliament_embed2 <- rbind(
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_5m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_6m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_7m.rds"),
  readRDS("data/uk_parliament/uk_parlspeechv2/parlspeech_uk_embed_8m.rds")
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

saveRDS(uk_parliament_sentences_s, "data/uk_parliament/uk_parlspeechv2/uk_parliament_sentences_s_ddr.rds")

uk_parliament_sentences_s_ddr <- readRDS("data/uk_parliament/uk_parlspeechv2/uk_parliament_sentences_s_ddr.rds")

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
set.seed(178)

#deontological
deont_sample <- uk_parliament_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_deont_main > .71) %>% # ~ 2.5k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- uk_parliament_sentences_s_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .62) %>% # ~33k sentences, threshold based on p = .8 from logistic regression model in pilot study
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
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(uk_parliament_sample, "corpus/uk_parliament_sample.rds")
write_excel_csv(uk_parliament_sample, "corpus/uk_parliament_sample.csv")

##2.4. MFT Reddit Corpus - Trager et al., 2022 ----

#load data
mft_reddit_sentences <- readRDS("data/mft_reddit_corpus/mft_reddit_sentences.rds")

#embeddings
mft_reddit_embed <- readRDS("data/mft_reddit_corpus/mft_reddit_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(mft_reddit_embed, 1, cos_sim, sentences_deont_main_ddr)
mft_reddit_sentences$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(mft_reddit_embed, 1, cos_sim, sentences_deont_pre_ddr)
mft_reddit_sentences$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(mft_reddit_embed, 1, cos_sim, sentences_conseq_main_ddr)
mft_reddit_sentences$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(mft_reddit_embed, 1, cos_sim, sentences_conseq_pre_ddr)
mft_reddit_sentences$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

#create score ranks
mft_reddit_sentences %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(mft_reddit_sentences, "data/mft_reddit_corpus/mft_reddit_sentences_ddr.rds")

mft_reddit_sentences_ddr <- readRDS("data/mft_reddit_corpus/mft_reddit_sentences_ddr.rds")

#explore distributions
mft_reddit_sentences_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

mft_reddit_sentences_ddr %>% summary()

mft_reddit_sentences_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(mft_reddit_sentences_ddr$sent_conseq_main, mft_reddit_sentences_ddr$sent_deont_main)
cor(mft_reddit_sentences_ddr$sent_deont_pre, mft_reddit_sentences_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- mft_reddit_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_deont_main > .71) %>% #only 3 sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_deont_main) %>%
  slice_head(n = 663) %>% 
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- mft_reddit_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_conseq_main > .62) %>% #only 20 sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_conseq_main) %>%
  slice_head(n = 663) %>% 
  mutate(sample = "outcome-based")

#no target
notarget_sample <- mft_reddit_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300) %>%
  mutate(sample = "no target")

#combine
mft_reddit_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(mft_reddit_sample, "corpus/mft_reddit_sample.rds")
write_excel_csv(mft_reddit_sample, "corpus/mft_reddit_sample.csv")

##2.5. MFT Twitter Corpus - Hoover et al., 2020 ----

#load data
mft_twitter_sentences <- readRDS("data/mft_twitter_corpus/mft_twitter_sentences.rds")

#embeddings
mft_twitter_embed <- readRDS("data/mft_twitter_corpus/mft_twitter_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(mft_twitter_embed, 1, cos_sim, sentences_deont_main_ddr)
mft_twitter_sentences$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(mft_twitter_embed, 1, cos_sim, sentences_deont_pre_ddr)
mft_twitter_sentences$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(mft_twitter_embed, 1, cos_sim, sentences_conseq_main_ddr)
mft_twitter_sentences$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(mft_twitter_embed, 1, cos_sim, sentences_conseq_pre_ddr)
mft_twitter_sentences$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

#create score ranks
mft_twitter_sentences %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(mft_twitter_sentences, "data/mft_twitter_corpus/mft_twitter_sentences_ddr.rds")

mft_twitter_sentences_ddr <- readRDS("data/mft_twitter_corpus/mft_twitter_sentences_ddr.rds")

#explore distributions
mft_twitter_sentences_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

mft_twitter_sentences_ddr %>% summary()

mft_twitter_sentences_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(mft_twitter_sentences_ddr$sent_conseq_main, mft_twitter_sentences_ddr$sent_deont_main)
cor(mft_twitter_sentences_ddr$sent_deont_pre, mft_twitter_sentences_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- mft_twitter_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_deont_main > .71) %>% #only 1 sentence, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_deont_main) %>%
  slice_head(n = 650) %>% 
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- mft_twitter_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_conseq_main > .62) %>% #only 3 sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_conseq_main) %>%
  slice_head(n = 650) %>% 
  mutate(sample = "outcome-based")

#no target
notarget_sample <- mft_twitter_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300) %>%
  mutate(sample = "no target")

#combine
mft_twitter_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(mft_twitter_sample, "corpus/mft_twitter_sample.rds")
write_excel_csv(mft_twitter_sample, "corpus/mft_twitter_sample.csv")

##2.6. GeRedE German Reddit Corpus - Blombach et al. (2020) ----

#load data
gerede_politics_sentences <- readRDS("data/german_reddit_corpus/gerede_politics_sentences.rds")

#embeddings
gerede_politics_embed <- readRDS("data/german_reddit_corpus/gerede_politics_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(gerede_politics_embed, 1, cos_sim, sentences_deont_main_ddr)
gerede_politics_sentences$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(gerede_politics_embed, 1, cos_sim, sentences_deont_pre_ddr)
gerede_politics_sentences$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(gerede_politics_embed, 1, cos_sim, sentences_conseq_main_ddr)
gerede_politics_sentences$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(gerede_politics_embed, 1, cos_sim, sentences_conseq_pre_ddr)
gerede_politics_sentences$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

#create score ranks
gerede_politics_sentences %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(gerede_politics_sentences, "data/german_reddit_corpus/gerede_politics_sentences_ddr.rds")

gerede_politics_sentences_ddr <- readRDS("data/german_reddit_corpus/gerede_politics_sentences_ddr.rds")

#explore distributions
gerede_politics_sentences_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

gerede_politics_sentences_ddr %>% summary()

gerede_politics_sentences_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(gerede_politics_sentences_ddr$sent_conseq_main, gerede_politics_sentences_ddr$sent_deont_main)
cor(gerede_politics_sentences_ddr$sent_deont_pre, gerede_politics_sentences_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- gerede_politics_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_deont_main > .71) %>% # 8k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- gerede_politics_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  filter(sent_conseq_main > .62) %>% # 45k sentences, threshold based on p = .8 from logistic regression model in pilot study
  slice_sample(n = 600) %>%
  mutate(sample = "outcome-based")

#no target
notarget_sample <- gerede_politics_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300) %>%
  mutate(sample = "no target")

#combine
gerede_politics_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(gerede_politics_sample, "corpus/gerede_politics_sample.rds")
write_excel_csv(gerede_politics_sample, "corpus/gerede_politics_sample.csv")

##2.7. eMFD - Hopp et al., 2021 ----

#load data
emfd_news_sentences <- readRDS("data/emfd_news/emfd_news_sentences.rds")

#embeddings
emfd_news_embed <- readRDS("data/emfd_news/emfd_news_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(emfd_news_embed, 1, cos_sim, sentences_deont_main_ddr)
emfd_news_sentences$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(emfd_news_embed, 1, cos_sim, sentences_deont_pre_ddr)
emfd_news_sentences$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(emfd_news_embed, 1, cos_sim, sentences_conseq_main_ddr)
emfd_news_sentences$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(emfd_news_embed, 1, cos_sim, sentences_conseq_pre_ddr)
emfd_news_sentences$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

#create score ranks
emfd_news_sentences %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(emfd_news_sentences, "data/emfd_news/emfd_news_sentences_ddr.rds")

emfd_news_sentences_ddr <- readRDS("data/emfd_news/emfd_news_sentences_ddr.rds")

#explore distributions
emfd_news_sentences_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

emfd_news_sentences_ddr %>% summary()

emfd_news_sentences_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(emfd_news_sentences_ddr$sent_conseq_main, emfd_news_sentences_ddr$sent_deont_main)
cor(emfd_news_sentences_ddr$sent_deont_pre, emfd_news_sentences_ddr$sent_deont_main)

#sample
set.seed(187)

#deontological
deont_sample <- emfd_news_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_deont_main > .71) %>% #only 1 sentence, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_deont_main) %>%
  slice_head(n = 620) %>% 
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- emfd_news_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_conseq_main > .62) %>% #only 3 sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_conseq_main) %>%
  slice_head(n = 620) %>% 
  mutate(sample = "outcome-based")

#no target
notarget_sample <- emfd_news_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300) %>%
  mutate(sample = "no target")

#combine
emfd_news_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  )%>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(emfd_news_sample, "corpus/emfd_news_sample.rds")
write_excel_csv(emfd_news_sample, "corpus/emfd_news_sample.csv")

##2.8. German Election Programs (2002-2021) - Voit et al. 2024 ----

#load data
ger_programs_sentences <- readRDS("data/german_election_programs/ger_programs_sentences.rds")

#embeddings
ger_programs_embed <- readRDS("data/german_election_programs/ger_programs_embed.rds")

#cosine similiarity to deontology sentences main
cosim_sent_deont_main <- apply(ger_programs_embed, 1, cos_sim, sentences_deont_main_ddr)
ger_programs_sentences$sent_deont_main <- cosim_sent_deont_main
rm(cosim_sent_deont_main)

#cosine similiarity to deontology sentences pre
cosim_sent_deont_pre <- apply(ger_programs_embed, 1, cos_sim, sentences_deont_pre_ddr)
ger_programs_sentences$sent_deont_pre <- cosim_sent_deont_pre
rm(cosim_sent_deont_pre)

#cosine similiarity to consequentialism sentences main
cosim_sent_conseq_main <- apply(ger_programs_embed, 1, cos_sim, sentences_conseq_main_ddr)
ger_programs_sentences$sent_conseq_main <- cosim_sent_conseq_main
rm(cosim_sent_conseq_main)

#cosine similiarity to consequentialism sentences pre
cosim_sent_conseq_pre <- apply(ger_programs_embed, 1, cos_sim, sentences_conseq_pre_ddr)
ger_programs_sentences$sent_conseq_pre <- cosim_sent_conseq_pre
rm(cosim_sent_conseq_pre)

#create score ranks
ger_programs_sentences %<>%
  arrange(-sent_deont_main) %>%
  mutate(deont_main_rank = row_number()) %>%
  arrange(-sent_deont_pre) %>%
  mutate(deont_pre_rank = row_number()) %>%
  arrange(-sent_conseq_main) %>%
  mutate(conseq_main_rank = row_number()) %>%
  arrange(-sent_conseq_pre) %>%
  mutate(conseq_pre_rank = row_number()) 

saveRDS(ger_programs_sentences, "data/german_election_programs/ger_programs_sentences_ddr.rds")

ger_programs_sentences_ddr <- readRDS("data/german_election_programs/ger_programs_sentences_ddr.rds")

#explore distributions
ger_programs_sentences_ddr %>%
  ggplot(aes(sent_deont_main)) +
  geom_histogram()

ger_programs_sentences_ddr %>% summary()

ger_programs_sentences_ddr %>% 
  summarise(
    quantiles = quantile(sent_deont_main, c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  )

cor(ger_programs_sentences_ddr$sent_conseq_main, ger_programs_sentences_ddr$sent_deont_main)
cor(ger_programs_sentences_ddr$sent_deont_pre, ger_programs_sentences_ddr$sent_deont_main)

ger_programs_sentences_ddr <- ger_programs_sentences
rm(ger_programs_sentences)

#sample
set.seed(187)

#deontological
deont_sample <- ger_programs_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_deont_main > .71) %>% #only 400 sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_deont_main) %>%
  slice_head(n = 609) %>% 
  mutate(sample = "rule-based")

#consequentialist
conseq_sample <- ger_programs_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  filter(!str_detect(sentence, "\\?")) %>% 
  #filter(sent_conseq_main > .62) %>%  #2k sentences, threshold based on p = .8 from logistic regression model in pilot study
  #slice_sample(n = 600) %>%
  arrange(-sent_conseq_main) %>%
  slice_head(n = 609) %>% 
  mutate(sample = "outcome-based")

#no target
notarget_sample <- ger_programs_sentences_ddr %>%
  filter(str_count(sentence, boundary("word")) > 3) %>% 
  distinct(sentence, .keep_all = TRUE) %>%
  slice_sample(n = 300) %>%
  mutate(sample = "no target")

#combine
ger_programs_sample <- rbind(deont_sample, conseq_sample, notarget_sample) %>%
  mutate(
    sentence = str_remove(sentence, "^-") #avoid "-" at the beginning of sentences for csv
  ) %>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-text)

saveRDS(ger_programs_sample, "corpus/ger_programs_sample.rds")
write_excel_csv(ger_programs_sample, "corpus/ger_programs_sample.csv")

#combine sample ----

total_sample <- rbind(open_discourse_sample, us_congress_sample, 
                      uk_parliament_sample, mft_twitter_sample, 
                      mft_reddit_sample, gerede_politics_sample,
                      emfd_news_sample, ger_programs_sample)

#remove urls

saveRDS(total_sample, "corpus/total_sample.rds")
write_excel_csv(total_sample, "corpus/total_sample.csv")
