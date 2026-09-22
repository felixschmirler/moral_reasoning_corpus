#inter-annotater reliability, cleaning and descriptives for corpus annotation

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(irr) #for reliability analysis
library(readxl)

#1. Expert Annotations ----

annotated <- read_excel("corpus/annotatedc.xlsx")

annotated %<>%
  mutate(
    `rule-based-r1` = ifelse(is.na(`rule-based-r1`), 0, 1),
    `outcome-based-r1` = ifelse(is.na(`outcome-based-r1`), 0, 1),
    `no-target-r1` = ifelse(`outcome-based-r1` == 0 & `rule-based-r1` == 0, 1, 0),
    `rule-based-r2` = ifelse(is.na(`rule-based-r2`), 0, 1),
    `outcome-based-r2` = ifelse(is.na(`outcome-based-r2`), 0, 1),
    `no-target-r2` = ifelse(`outcome-based-r2` == 0 & `rule-based-r2` == 0, 1, 0),
    `rule-based-r3` = ifelse(is.na(`rule-based-r3`), 0, 1),
    `outcome-based-r3` = ifelse(is.na(`outcome-based-r3`), 0, 1),
    `no-target-r3` = ifelse(`outcome-based-r3` == 0 & `rule-based-r3` == 0, 1, 0),
    `rule-based-r4` = ifelse(is.na(`rule-based-r4`), 0, 1),
    `outcome-based-r4` = ifelse(is.na(`outcome-based-r4`), 0, 1),
    `no-target-r4` = ifelse(`outcome-based-r4` == 0 & `rule-based-r4` == 0, 1, 0),
    confidence_r1 = ifelse(is.na(`high-confidence-r1`), 0, 1),
    confidence_r2 = ifelse(is.na(`high-confidence-r2`), 0, 1),
    confidence_r3 = ifelse(is.na(`high-confidence-r3`), 0, 1),
    confidence_r4 = ifelse(is.na(`high-confidence-r4`), 0, 1),
    source = text_sentence_id %>% str_sub(1, 9),
    language = if_else(source %in% c("election_", "gerede_re", "opendisco"), "German", "English"),
    source_type = if_else(source %in% c("gerede_re", "mft_reddi", "mft_twitt"), "social", 
                          if_else(source %in% c("opendisco", "uk_parlsp", "uscongres"), "parliament", 
                                  "other")),
    `half sentence` = ifelse(is.na(`half sentence`), 0, 1)
  ) 

#annotated_subset <- annotated #%>% filter(!is.na(`half sentence`))#[3001:3250,]

##1.1 agreement & confidence ---- 
annotated %<>% 
  mutate(
    sum_rule = `rule-based-r1` + `rule-based-r3` + `rule-based-r4`,
    sum_outcome = `outcome-based-r1` + `outcome-based-r3` + `outcome-based-r4`,
    majority_rule = if_else(sum_rule >= 2, 1, 0),
    majority_outcome = if_else(sum_outcome >= 2, 1, 0),
    confidence = confidence_r1 + confidence_r3 + confidence_r4,
    hconf_rule = if_else((sum_rule == 0 | sum_rule == 3) & confidence == 3, 1, 0),
    hconf_outcome = if_else((sum_outcome == 0 | sum_outcome == 3) & confidence == 3, 1, 0)
  )


#counts & percentages

#confidence
confidence_count <- annotated %>%   
  count(confidence) %>%
  mutate(
    perc = n/7000
  )
confidence_count

#rule-based
table(annotated$majority_rule) #/ 7000

rule_count <- annotated %>%   
  count(sum_rule) %>%
  mutate(
    perc = n/7000
  )
rule_count

rule_count_c <- annotated %>%   
  count(sum_rule, confidence) %>%
  mutate(
    perc = n/7000
  )

sum(annotated$hconf_rule)

rule_count_c %>%
  filter(sum_rule == 0 | sum_rule == 3, confidence == 3)

#outcome-based
table(annotated$majority_outcome) #/ 7000

outcome_count <- annotated %>%   
  count(sum_outcome) %>%
  mutate(
    perc = n/7000
  )
outcome_count

outcome_count_c <- annotated %>%   
  count(sum_outcome, confidence) %>%
  mutate(
    perc = n/7000
  )

sum(annotated$hconf_outcome)

outcome_count_c %>%
  filter(sum_outcome == 0 | sum_outcome == 3, confidence == 3)

#both labels
table(annotated$majority_rule, annotated$majority_outcome, annotated$confidence)

table(annotated$hconf_rule, annotated$hconf_outcome)

##1.2 reliabilities ---- 


#rule-based
alpha_rule <- kripp.alpha(
  t(annotated[, c("rule-based-r1", "rule-based-r3", "rule-based-r4")]),
  method = "nominal"
)
alpha_rule$value

#by half sentence

annotated %>% count(source, `half sentence`)

annotated %>%
  group_by(`half sentence`) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("rule-based-r1", "rule-based-r3", "rule-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by source type
annotated %>%
  group_by(source_type) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("rule-based-r1", "rule-based-r3", "rule-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by language
annotated %>%
  group_by(language) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("rule-based-r1", "rule-based-r3", "rule-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by source
annotated %>%
  group_by(source) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("rule-based-r1", "rule-based-r3", "rule-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#outcome-based
alpha_outcome <- kripp.alpha(
  t(annotated[, c("outcome-based-r1", "outcome-based-r3", "outcome-based-r4")]),
  method = "nominal"
)
alpha_outcome$value

#by half sentence
annotated %>%
  group_by(`half sentence`) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("outcome-based-r1", "outcome-based-r3", "outcome-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by source type
annotated %>%
  group_by(source_type) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("outcome-based-r1", "outcome-based-r3", "outcome-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by language
annotated %>%
  group_by(language) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("outcome-based-r1", "outcome-based-r3", "outcome-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#by source
annotated %>%
  group_by(source) %>%
  summarise(
    n = n(),
    alpha = kripp.alpha(
      t(as.matrix(pick(all_of(c("outcome-based-r1", "outcome-based-r3", "outcome-based-r4"))))),
      method = "nominal"
    )$value,
    .groups = "drop"
  )

#old ------

rule_ratings <- annotated_subset %>% 
  select(`rule-based-r1`, `rule-based-r4`, `rule-based-r3`)

kappam.fleiss(rule_ratings)

rule_alpha <- kripp.alpha(
  t(as.matrix(rule_ratings)),
  method = "nominal"
)
rule_alpha

kappa2(annotated_subset %>% select(`rule-based-r1`, `rule-based-r4`))
kappa2(annotated_subset %>% select(`rule-based-r1`, `rule-based-r3`))
kappa2(annotated_subset %>% select(`rule-based-r4`, `rule-based-r3`))


outcome_ratings <- annotated_subset %>% 
  select(`outcome-based-r1`, `outcome-based-r4`, `outcome-based-r3`)

kappam.fleiss(outcome_ratings)

outcome_alpha <- kripp.alpha(
  t(as.matrix(outcome_ratings)),
  method = "nominal"
)
outcome_alpha

kappa2(annotated_subset %>% select(`outcome-based-r1`, `outcome-based-r4`))
kappa2(annotated_subset %>% select(`outcome-based-r1`, `outcome-based-r3`))
kappa2(annotated_subset %>% select(`outcome-based-r4`, `outcome-based-r3`))

notarget_ratings <- annotated_subset %>% 
  select(`no-target-r1`, `no-target-r4`, `no-target-r3`)

kappam.fleiss(notarget_ratings)

notarget_alpha <- kripp.alpha(
  t(as.matrix(notarget_ratings)),
  method = "nominal"
)
notarget_alpha

kappa2(annotated_subset %>% select(`no-target-r1`, `no-target-r4`))
kappa2(annotated_subset %>% select(`no-target-r1`, `no-target-r3`))
kappa2(annotated_subset %>% select(`no-target-r4`, `no-target-r3`))

#2. Comparison with MFT Datasets ----

##2.1 Reddit Corpus ----
mft_reddit <- read_csv("data/mft_reddit_corpus/final_mfrc_data.csv")

# Which annotators actually rated which texts?
rated_pairs <- mft_reddit %>%
  distinct(text, annotator)

# Convert selected labels into separate binary-positive rows
selected_labels <- mft_reddit %>%
  separate_rows(annotation, sep = "\\s*,\\s*") %>%
  mutate(
    annotation = str_squish(annotation),
    annotation = str_replace_all(annotation, "[^A-Za-z0-9]+", "_"),
    value = 1L
  ) %>%
  filter(
    !is.na(annotation),
    annotation != ""
  ) %>%
  distinct(text, annotator, annotation, .keep_all = TRUE)

# All possible annotation dimensions
all_labels <- sort(unique(selected_labels$annotation))

# Create zeros only for texts that the annotator actually rated
mft_reddit_binary <- rated_pairs %>%
  crossing(annotation = all_labels) %>%
  left_join(
    selected_labels %>%
      select(text, annotator, annotation, value),
    by = c("text", "annotator", "annotation")
  ) %>%
  mutate(
    value = replace_na(value, 0L)
  )

# Convert to wide format
mft_reddit_wide <- mft_reddit_binary %>%
  pivot_wider(
    id_cols = text,
    names_from = c(annotator, annotation),
    values_from = value,
    names_glue = "{annotator}_{annotation}"
    # Do not use values_fill = 0
  )

#authority
authority_ratings <- mft_reddit_wide %>% 
  select(contains("Authority"))

authority_alpha <- kripp.alpha(
  t(as.matrix(authority_ratings)),
  method = "nominal"
)
authority_alpha

#equality
equality_ratings <- mft_reddit_wide %>% 
  select(contains("equality"))

equality_alpha <- kripp.alpha(
  t(as.matrix(equality_ratings)),
  method = "nominal"
)
equality_alpha

#care
care_ratings <- mft_reddit_wide %>% 
  select(contains("care"))

care_alpha <- kripp.alpha(
  t(as.matrix(care_ratings)),
  method = "nominal"
)
care_alpha

#loyalty
loyalty_ratings <- mft_reddit_wide %>% 
  select(contains("loyalty"))

loyalty_alpha <- kripp.alpha(
  t(as.matrix(loyalty_ratings)),
  method = "nominal"
)
loyalty_alpha

#purity
purity_ratings <- mft_reddit_wide %>% 
  select(contains("purity"))

purity_alpha <- kripp.alpha(
  t(as.matrix(purity_ratings)),
  method = "nominal"
)
purity_alpha


##2.2 Twitter Corpus ----
mft_twitter <- fromJSON("data/mft_twitter_corpus/MFTC_V4_text.json")

mft_twitter %<>% unnest() %<>% unnest() 

# Which annotators actually rated which texts?
rated_pairs <- mft_twitter %>%
  distinct(tweet_text, annotator)

# Convert selected labels into separate binary-positive rows
selected_labels <- mft_twitter %>%
  separate_rows(annotation, sep = "\\s*,\\s*") %>%
  mutate(
    annotation = str_squish(annotation),
    annotation = str_replace_all(annotation, "[^A-Za-z0-9]+", "_"),
    value = 1L
  ) %>%
  filter(
    !is.na(annotation),
    annotation != ""
  ) %>%
  distinct(tweet_text, annotator, annotation, .keep_all = TRUE)

# All possible annotation dimensions
all_labels <- sort(unique(selected_labels$annotation))

# Create zeros only for texts that the annotator actually rated
mft_twitter_binary <- rated_pairs %>%
  crossing(annotation = all_labels) %>%
  left_join(
    selected_labels %>%
      select(tweet_text, annotator, annotation, value),
    by = c("tweet_text", "annotator", "annotation")
  ) %>%
  mutate(
    value = replace_na(value, 0L)
  )

# Convert to wide format
mft_twitter_wide <- mft_twitter_binary %>%
  pivot_wider(
    id_cols = tweet_text,
    names_from = c(annotator, annotation),
    values_from = value,
    names_glue = "{annotator}_{annotation}"
    # Do not use values_fill = 0
  )

#authority
authority_ratings <- mft_twitter_wide %>% 
  select(contains("Authority"))

authority_alpha <- kripp.alpha(
  t(as.matrix(authority_ratings)),
  method = "nominal"
)
authority_alpha

#equality
equality_ratings <- mft_twitter_wide %>% 
  select(contains("equality"))

equality_alpha <- kripp.alpha(
  t(as.matrix(equality_ratings)),
  method = "nominal"
)
equality_alpha

#care
care_ratings <- mft_twitter_wide %>% 
  select(contains("care"))

care_alpha <- kripp.alpha(
  t(as.matrix(care_ratings)),
  method = "nominal"
)
care_alpha

#loyalty
loyalty_ratings <- mft_twitter_wide %>% 
  select(contains("loyalty"))

loyalty_alpha <- kripp.alpha(
  t(as.matrix(loyalty_ratings)),
  method = "nominal"
)
loyalty_alpha

#purity
purity_ratings <- mft_twitter_wide %>% 
  select(contains("purity"))

purity_alpha <- kripp.alpha(
  t(as.matrix(purity_ratings)),
  method = "nominal"
)
purity_alpha
