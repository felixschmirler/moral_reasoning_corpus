#inter-annotater reliability, cleaning and descriptives for corpus annotation

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(irr) #for reliability analysis
library(readxl)

#1. Expert Annotations ----

annotated <- read_excel("corpus/annotated.xlsx")

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
    confidence_r1 = ifelse(`high-confidence-r1` == "f", "high", "low"),
    confidence_r2 = ifelse(`high-confidence-r2` == "a", "high", "low"),
    confidence_r3 = ifelse(`high-confidence-r3` == "c", "high", "low")
    
  ) 

annotated_subset <- annotated[2501:2750,]  #%>% filter(`3 Labels` == "x")

rule_ratings <- annotated_subset %>% 
  select(`rule-based-r1`, `rule-based-r2`, `rule-based-r3`)

kappam.fleiss(rule_ratings)

rule_alpha <- kripp.alpha(
  t(as.matrix(rule_ratings)),
  method = "nominal"
)
rule_alpha

kappa2(annotated_subset %>% select(`rule-based-r1`, `rule-based-r2`))
kappa2(annotated_subset %>% select(`rule-based-r1`, `rule-based-r3`))
kappa2(annotated_subset %>% select(`rule-based-r2`, `rule-based-r3`))


outcome_ratings <- annotated_subset %>% 
  select(`outcome-based-r1`, `outcome-based-r2`, `outcome-based-r3`)

kappam.fleiss(outcome_ratings)

outcome_alpha <- kripp.alpha(
  t(as.matrix(outcome_ratings)),
  method = "nominal"
)
outcome_alpha

kappa2(annotated_subset %>% select(`outcome-based-r1`, `outcome-based-r2`))
kappa2(annotated_subset %>% select(`outcome-based-r1`, `outcome-based-r3`))
kappa2(annotated_subset %>% select(`outcome-based-r2`, `outcome-based-r3`))

notarget_ratings <- annotated_subset %>% 
  select(`no-target-r1`, `no-target-r2`, `no-target-r3`)

kappam.fleiss(notarget_ratings)

notarget_alpha <- kripp.alpha(
  t(as.matrix(notarget_ratings)),
  method = "nominal"
)
notarget_alpha

kappa2(annotated_subset %>% select(`no-target-r1`, `no-target-r2`))
kappa2(annotated_subset %>% select(`no-target-r1`, `no-target-r3`))
kappa2(annotated_subset %>% select(`no-target-r2`, `no-target-r3`))

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
