#Script to produce exemplary reasoning sentences for Distributed Dictionary Representations Method (Garten et al., 2018) with dynamic embeddings for sampling

#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(httr) #for accessing the open AI api
library(jsonlite) #for dealing with json files

#python setup
library(reticulate) #to work with python libraries
use_condaenv("nlp_env", required = TRUE) #use specific conda environment with spacy, sentence BERT etc.
py_config()


#1.  Creating the basis for DDR ----

##1.1. LIWC dictionaries ---- 

#load LWIC dictionaries for deontology and utilitarianism (Wheeler & Laham, 2016) 
dict_deont <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Deontology == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

dict_conseq <- read_csv("content/moral-justification-dictionaries.csv") %>% 
  filter(Consequentialism == "X") %>% 
  pull(DicTerm) %>% 
  str_remove("\\*")

#load reduced LWIC dictionaries for deontology and utilitarianism 
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

##2.1. Exemplary Sentences ---- 

#open ai api key
api_key <- read_lines("api_key.txt")

###2.1.1. Load Pre-study Sentences ----

#load files generated with gpt-4.1 via the open AI api 
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

###2.2.1. Create New Sentences ----

####deontology - English - topic 1: climate change ----
response_deont_t1_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about actions against climate change.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t1_en$content)
rm(response_deont_t1_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t1_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t1_en <- fromJSON(sentences_deont_t1_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t1_en, "content/sentences_deont_t1_en.rds")
rm(sentences_deont_t1_en)
#read file
sentences_deont_t1_en <- readRDS("content/sentences_deont_t1_en.rds")

####deontology - English - topic 2: COVID-19 restrictions ----
response_deont_t2_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about COVID-19 restrictions.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t2_en$content)
rm(response_deont_t2_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t2_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t2_en <- fromJSON(sentences_deont_t2_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t2_en, "content/sentences_deont_t2_en.rds")
rm(sentences_deont_t2_en)
#read file
sentences_deont_t2_en <- readRDS("content/sentences_deont_t2_en.rds")

####deontology - English - topic 3: immigration ----
response_deont_t3_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about immigration and asylum policies.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t3_en$content)
rm(response_deont_t3_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t3_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t3_en <- fromJSON(sentences_deont_t3_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t3_en, "content/sentences_deont_t3_en.rds")
rm(sentences_deont_t3_en)
#read file
sentences_deont_t3_en <- readRDS("content/sentences_deont_t3_en.rds")

####deontology - English - topic 4: military support ----
response_deont_t4_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about military support in a foreign conflict.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t4_en$content)
rm(response_deont_t4_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t4_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t4_en <- fromJSON(sentences_deont_t4_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t4_en, "content/sentences_deont_t4_en.rds")
rm(sentences_deont_t4_en)
#read file
sentences_deont_t4_en <- readRDS("content/sentences_deont_t4_en.rds")

####deontology - English - topic 5: sanctions ----
response_deont_t5_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sanctioning another country.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t5_en$content)
rm(response_deont_t5_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t5_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t5_en <- fromJSON(sentences_deont_t5_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t5_en, "content/sentences_deont_t5_en.rds")
rm(sentences_deont_t5_en)
#read file
sentences_deont_t5_en <- readRDS("content/sentences_deont_t5_en.rds")

####deontology - English - topic 6:  gender equality ----
response_deont_t6_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about gender equality.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t6_en$content)
rm(response_deont_t6_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t6_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t6_en <- fromJSON(sentences_deont_t6_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t6_en, "content/sentences_deont_t6_en.rds")
rm(sentences_deont_t6_en)
#read file
sentences_deont_t6_en <- readRDS("content/sentences_deont_t6_en.rds")

####deontology - English - topic 7:  minority rights ----
response_deont_t7_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about minority rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t7_en$content)
rm(response_deont_t7_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t7_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t7_en <- fromJSON(sentences_deont_t7_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t7_en, "content/sentences_deont_t7_en.rds")
rm(sentences_deont_t7_en)
#read file
sentences_deont_t7_en <- readRDS("content/sentences_deont_t7_en.rds")

####deontology - English - topic 8:  sexual orientation and gender identity rights ----
response_deont_t8_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sexual orientation and gender identity rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t8_en$content)
rm(response_deont_t8_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t8_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t8_en <- fromJSON(sentences_deont_t8_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t8_en, "content/sentences_deont_t8_en.rds")
rm(sentences_deont_t8_en)
#read file
sentences_deont_t8_en <- readRDS("content/sentences_deont_t8_en.rds")

####deontology - English - topic 9:  freedom of speech vs regulation of harmful speech ----
response_deont_t9_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about freedom of speech vs regulation of harmful speech.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t9_en$content)
rm(response_deont_t9_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t9_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t9_en <- fromJSON(sentences_deont_t9_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t9_en, "content/sentences_deont_t9_en.rds")
rm(sentences_deont_t9_en)
#read file
sentences_deont_t9_en <- readRDS("content/sentences_deont_t9_en.rds")

####deontology - English - topic 10:  social welfare ----
response_deont_t10_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about social welfare.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t10_en$content)
rm(response_deont_t10_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t10_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t10_en <- fromJSON(sentences_deont_t10_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t10_en, "content/sentences_deont_t10_en.rds")
rm(sentences_deont_t10_en)
#read file
sentences_deont_t10_en <- readRDS("content/sentences_deont_t10_en.rds")

####deontology - English - topic 11:  taxation and economic redistribution ----
response_deont_t11_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about taxation and economic redistribution.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t11_en$content)
rm(response_deont_t11_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t11_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t11_en <- fromJSON(sentences_deont_t11_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t11_en, "content/sentences_deont_t11_en.rds")
rm(sentences_deont_t11_en)
#read file
sentences_deont_t11_en <- readRDS("content/sentences_deont_t11_en.rds")

####deontology - English - topic 12:  abortion laws ----
response_deont_t12_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about abortion laws.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t12_en$content)
rm(response_deont_t12_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t12_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t12_en <- fromJSON(sentences_deont_t12_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t12_en, "content/sentences_deont_t12_en.rds")
rm(sentences_deont_t12_en)
#read file
sentences_deont_t12_en <- readRDS("content/sentences_deont_t12_en.rds")

####deontology - English - topic 13:  independence movements and referendums ----
response_deont_t13_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about independence movements and referendums.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t13_en$content)
rm(response_deont_t13_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t13_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t13_en <- fromJSON(sentences_deont_t13_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t13_en, "content/sentences_deont_t13_en.rds")
rm(sentences_deont_t13_en)
#read file
sentences_deont_t13_en <- readRDS("content/sentences_deont_t13_en.rds")

####deontology - English - topic 14:  body positivity and traditional beauty standards ----
response_deont_t14_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural tone, wording, and length found in polarized debates 
      about body positivity and pursuing traditional beauty ideals
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t14_en$content)
rm(response_deont_t14_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t14_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t14_en <- fromJSON(sentences_deont_t14_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t14_en, "content/sentences_deont_t14_en.rds")
rm(sentences_deont_t14_en)
#read file
sentences_deont_t14_en <- readRDS("content/sentences_deont_t14_en.rds")

####deontology - English - topic 15:  meat consumption ----
response_deont_t15_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about meat consumption.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t15_en$content)
rm(response_deont_t15_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t15_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t15_en <- fromJSON(sentences_deont_t15_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t15_en, "content/sentences_deont_t15_en.rds")
rm(sentences_deont_t15_en)
#read file
sentences_deont_t15_en <- readRDS("content/sentences_deont_t15_en.rds")


####deontology - English - topic 0:  topic agnostic ----
response_deont_t0_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      without actually addressing the specific content of a topic.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t0_en$content)
rm(response_deont_t0_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t0_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t0_en <- fromJSON(sentences_deont_t0_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t0_en, "content/sentences_deont_t0_en.rds")
rm(sentences_deont_t0_en)
#read file
sentences_deont_t0_en <- readRDS("content/sentences_deont_t0_en.rds")

####deontology - German - topic 1: climate change ----
response_deont_t1_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about actions against climate change.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t1_ger$content)
rm(response_deont_t1_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t1_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t1_ger <- fromJSON(sentences_deont_t1_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t1_ger, "content/sentences_deont_t1_ger.rds")
rm(sentences_deont_t1_ger)
#read file
sentences_deont_t1_ger <- readRDS("content/sentences_deont_t1_ger.rds")

####deontology - German - topic 2: COVID-19 restrictions ----
response_deont_t2_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about COVID-19 restrictions.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t2_ger$content)
rm(response_deont_t2_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t2_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t2_ger <- fromJSON(sentences_deont_t2_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t2_ger, "content/sentences_deont_t2_ger.rds")
rm(sentences_deont_t2_ger)
#read file
sentences_deont_t2_ger <- readRDS("content/sentences_deont_t2_ger.rds")

####deontology - German - topic 3: immigration ----
response_deont_t3_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about immigration and asylum policies.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t3_ger$content)
rm(response_deont_t3_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t3_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t3_ger <- fromJSON(sentences_deont_t3_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t3_ger, "content/sentences_deont_t3_ger.rds")
rm(sentences_deont_t3_ger)
#read file
sentences_deont_t3_ger <- readRDS("content/sentences_deont_t3_ger.rds")

####deontology - German - topic 4: military support ----
response_deont_t4_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about military support in a foreign conflict.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t4_ger$content)
rm(response_deont_t4_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t4_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t4_ger <- fromJSON(sentences_deont_t4_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t4_ger, "content/sentences_deont_t4_ger.rds")
rm(sentences_deont_t4_ger)
#read file
sentences_deont_t4_ger <- readRDS("content/sentences_deont_t4_ger.rds")

####deontology - German - topic 5: sanctions ----
response_deont_t5_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sanctioning another country.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t5_ger$content)
rm(response_deont_t5_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t5_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t5_ger <- fromJSON(sentences_deont_t5_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t5_ger, "content/sentences_deont_t5_ger.rds")
rm(sentences_deont_t5_ger)
#read file
sentences_deont_t5_ger <- readRDS("content/sentences_deont_t5_ger.rds")

####deontology - German - topic 6:  gender equality ----
response_deont_t6_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about gender equality.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t6_ger$content)
rm(response_deont_t6_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t6_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t6_ger <- fromJSON(sentences_deont_t6_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_deont_t6_ger, "content/sentences_deont_t6_ger.rds")
rm(sentences_deont_t6_ger)
#read file
sentences_deont_t6_ger <- readRDS("content/sentences_deont_t6_ger.rds")

####deontology - German - topic 7:  minority rights ----
response_deont_t7_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about minority rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t7_ger$content)
rm(response_deont_t7_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t7_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t7_ger <- fromJSON(sentences_deont_t7_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t7_ger, "content/sentences_deont_t7_ger.rds")
rm(sentences_deont_t7_ger)
#read file
sentences_deont_t7_ger <- readRDS("content/sentences_deont_t7_ger.rds")

####deontology - German - topic 8:  sexual orientation and gender identity rights ----
response_deont_t8_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sexual orientation and gender identity rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t8_ger$content)
rm(response_deont_t8_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t8_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t8_ger <- fromJSON(sentences_deont_t8_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t8_ger, "content/sentences_deont_t8_ger.rds")
rm(sentences_deont_t8_ger)
#read file
sentences_deont_t8_ger <- readRDS("content/sentences_deont_t8_ger.rds")

####deontology - German - topic 9:  freedom of speech vs regulation of harmful speech ----
response_deont_t9_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about freedom of speech vs regulation of harmful speech.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t9_ger$content)
rm(response_deont_t9_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t9_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t9_ger <- fromJSON(sentences_deont_t9_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t9_ger, "content/sentences_deont_t9_ger.rds")
rm(sentences_deont_t9_ger)
#read file
sentences_deont_t9_ger <- readRDS("content/sentences_deont_t9_ger.rds")

####deontology - German - topic 10:  social welfare ----
response_deont_t10_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about social welfare.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t10_ger$content)
rm(response_deont_t10_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t10_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t10_ger <- fromJSON(sentences_deont_t10_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t10_ger, "content/sentences_deont_t10_ger.rds")
rm(sentences_deont_t10_ger)
#read file
sentences_deont_t10_ger <- readRDS("content/sentences_deont_t10_ger.rds")

####deontology - German - topic 11:  taxation and economic redistribution ----
response_deont_t11_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about taxation and economic redistribution.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t11_ger$content)
rm(response_deont_t11_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t11_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t11_ger <- fromJSON(sentences_deont_t11_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t11_ger, "content/sentences_deont_t11_ger.rds")
rm(sentences_deont_t11_ger)
#read file
sentences_deont_t11_ger <- readRDS("content/sentences_deont_t11_ger.rds")

####deontology - German - topic 12:  abortion laws ----
response_deont_t12_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about abortion laws.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t12_ger$content)
rm(response_deont_t12_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t12_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t12_ger <- fromJSON(sentences_deont_t12_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t12_ger, "content/sentences_deont_t12_ger.rds")
rm(sentences_deont_t12_ger)
#read file
sentences_deont_t12_ger <- readRDS("content/sentences_deont_t12_ger.rds")

####deontology - German - topic 13:  independence movements and referendums ----
response_deont_t13_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about independence movements and referendums.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t13_ger$content)
rm(response_deont_t13_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t13_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t13_ger <- fromJSON(sentences_deont_t13_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t13_ger, "content/sentences_deont_t13_ger.rds")
rm(sentences_deont_t13_ger)
#read file
sentences_deont_t13_ger <- readRDS("content/sentences_deont_t13_ger.rds")

####deontology - German - topic 14:  body positivity and traditional beauty standards ----
response_deont_t14_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural tone, wording, and length found in polarized debates 
      about body positivity and pursuing traditional beauty ideals
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t14_ger$content)
rm(response_deont_t14_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t14_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t14_ger <- fromJSON(sentences_deont_t14_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t14_ger, "content/sentences_deont_t14_ger.rds")
rm(sentences_deont_t14_ger)
#read file
sentences_deont_t14_ger <- readRDS("content/sentences_deont_t14_ger.rds")

####deontology - German - topic 15:  meat consumption ----
response_deont_t15_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about meat consumption.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t15_ger$content)
rm(response_deont_t15_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t15_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t15_ger <- fromJSON(sentences_deont_t15_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t15_ger, "content/sentences_deont_t15_ger.rds")
rm(sentences_deont_t15_ger)
#read file
sentences_deont_t15_ger <- readRDS("content/sentences_deont_t15_ger.rds")


####deontology - German - topic 0:  topic agnostic ----
response_deont_t0_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties that need to be followed - 
      implying a prioritization over outcomes. 
      
      Potential cue words: e.g. duty, law, norm, principle, rights, rules, 
      custom, mission, responsibility, standards, contract, prohibited, taboo
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: Rule-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to protect right B (rule-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of rules, rights or duties that are 
      implicitly linked to decisions or actions.
      
      Example: We need to protect right B.
      
      Do not use: 
      
      •	cue words appear without  evaluation or implications for actions  
      (e.g., „Citizens’ rights were discussed in the committee meeting.“, 
      „Die Rechte der Bürger wurden in der Ausschusssitzung besprochen.“)
      •	Verbs like “have to” or ”need to” or ”must“ alone are not sufficient to be 
      interpreted as rule-based since they are just a common expression 
      of what should be done (e.g., “We have to invest more into education”, 
      “Wir müssen mehr in Bildung investieren”). In order to be interpreted as 
      rule, duty, principle etc. they need to appear with additional cues that 
      indicate that something has to be done without a cost-benefit analysis of 
      the outcomes, e.g. “at all cost”, “absolute priority” etc.

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      without actually addressing the specific content of a topic.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_deont_t0_ger$content)
rm(response_deont_t0_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t0_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t0_ger <- fromJSON(sentences_deont_t0_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t0_ger, "content/sentences_deont_t0_ger.rds")
rm(sentences_deont_t0_ger)
#read file
sentences_deont_t0_ger <- readRDS("content/sentences_deont_t0_ger.rds")

####consequentialism - English - topic 1: climate change ----
response_conseq_t1_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 
      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about actions against climate change.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t1_en$content)
rm(response_conseq_t1_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t1_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t1_en <- fromJSON(sentences_conseq_t1_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t1_en, "content/sentences_conseq_t1_en.rds")
rm(sentences_conseq_t1_en)
#read file
sentences_conseq_t1_en <- readRDS("content/sentences_conseq_t1_en.rds")

####consequentialism - English - topic 2: COVID-19 restrictions ----
response_conseq_t2_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about COVID-19 restrictions.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t2_en$content)
rm(response_conseq_t2_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t2_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t2_en <- fromJSON(sentences_conseq_t2_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t2_en, "content/sentences_conseq_t2_en.rds")
rm(sentences_conseq_t2_en)
#read file
sentences_conseq_t2_en <- readRDS("content/sentences_conseq_t2_en.rds")

####consequentialism - English - topic 3: immigration ----
response_conseq_t3_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about immigration and asylum policies.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t3_en$content)
rm(response_conseq_t3_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t3_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t3_en <- fromJSON(sentences_conseq_t3_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t3_en, "content/sentences_conseq_t3_en.rds")
rm(sentences_conseq_t3_en)
#read file
sentences_conseq_t3_en <- readRDS("content/sentences_conseq_t3_en.rds")

####consequentialism - English - topic 4: military support ----
response_conseq_t4_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about military support in a foreign conflict.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t4_en$content)
rm(response_conseq_t4_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t4_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t4_en <- fromJSON(sentences_conseq_t4_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t4_en, "content/sentences_conseq_t4_en.rds")
rm(sentences_conseq_t4_en)
#read file
sentences_conseq_t4_en <- readRDS("content/sentences_conseq_t4_en.rds")

####consequentialism - English - topic 5: sanctions ----
response_conseq_t5_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sanctioning another country.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t5_en$content)
rm(response_conseq_t5_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t5_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t5_en <- fromJSON(sentences_conseq_t5_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t5_en, "content/sentences_conseq_t5_en.rds")
rm(sentences_conseq_t5_en)
#read file
sentences_conseq_t5_en <- readRDS("content/sentences_conseq_t5_en.rds")

####consequentialism - English - topic 6:  gender equality ----
response_conseq_t6_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about gender equality.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t6_en$content)
rm(response_conseq_t6_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t6_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t6_en <- fromJSON(sentences_conseq_t6_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t6_en, "content/sentences_conseq_t6_en.rds")
rm(sentences_conseq_t6_en)
#read file
sentences_conseq_t6_en <- readRDS("content/sentences_conseq_t6_en.rds")

####consequentialism - English - topic 7:  minority rights ----
response_conseq_t7_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about minority rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t7_en$content)
rm(response_conseq_t7_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t7_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t7_en <- fromJSON(sentences_conseq_t7_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t7_en, "content/sentences_conseq_t7_en.rds")
rm(sentences_conseq_t7_en)
#read file
sentences_conseq_t7_en <- readRDS("content/sentences_conseq_t7_en.rds")

####consequentialism - English - topic 8:  sexual orientation and gender identity rights ----
response_conseq_t8_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sexual orientation and gender identity rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t8_en$content)
rm(response_conseq_t8_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t8_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t8_en <- fromJSON(sentences_conseq_t8_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t8_en, "content/sentences_conseq_t8_en.rds")
rm(sentences_conseq_t8_en)
#read file
sentences_conseq_t8_en <- readRDS("content/sentences_conseq_t8_en.rds")

####consequentialism - English - topic 9:  freedom of speech vs regulation of harmful speech ----
response_conseq_t9_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about freedom of speech vs regulation of harmful speech.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t9_en$content)
rm(response_conseq_t9_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t9_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t9_en <- fromJSON(sentences_conseq_t9_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t9_en, "content/sentences_conseq_t9_en.rds")
rm(sentences_conseq_t9_en)
#read file
sentences_conseq_t9_en <- readRDS("content/sentences_conseq_t9_en.rds")

####consequentialism - English - topic 10:  social welfare ----
response_conseq_t10_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about social welfare.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t10_en$content)
rm(response_conseq_t10_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t10_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t10_en <- fromJSON(sentences_conseq_t10_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t10_en, "content/sentences_conseq_t10_en.rds")
rm(sentences_conseq_t10_en)
#read file
sentences_conseq_t10_en <- readRDS("content/sentences_conseq_t10_en.rds")

####consequentialism - English - topic 11:  taxation and economic redistribution ----
response_conseq_t11_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about taxation and economic redistribution.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t11_en$content)
rm(response_conseq_t11_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t11_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t11_en <- fromJSON(sentences_conseq_t11_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t11_en, "content/sentences_conseq_t11_en.rds")
rm(sentences_conseq_t11_en)
#read file
sentences_conseq_t11_en <- readRDS("content/sentences_conseq_t11_en.rds")

####consequentialism - English - topic 12:  abortion laws ----
response_conseq_t12_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about abortion laws.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t12_en$content)
rm(response_conseq_t12_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t12_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t12_en <- fromJSON(sentences_conseq_t12_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t12_en, "content/sentences_conseq_t12_en.rds")
rm(sentences_conseq_t12_en)
#read file
sentences_conseq_t12_en <- readRDS("content/sentences_conseq_t12_en.rds")

####consequentialism - English - topic 13:  independence movements and referendums ----
response_conseq_t13_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about independence movements and referendums.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t13_en$content)
rm(response_conseq_t13_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t13_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t13_en <- fromJSON(sentences_conseq_t13_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t13_en, "content/sentences_conseq_t13_en.rds")
rm(sentences_conseq_t13_en)
#read file
sentences_conseq_t13_en <- readRDS("content/sentences_conseq_t13_en.rds")

####consequentialism - English - topic 14:  body positivity and traditional beauty standards ----
response_conseq_t14_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural tone, wording, and length found in polarized debates 
      about body positivity and pursuing traditional beauty ideals
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t14_en$content)
rm(response_conseq_t14_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t14_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t14_en <- fromJSON(sentences_conseq_t14_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t14_en, "content/sentences_conseq_t14_en.rds")
rm(sentences_conseq_t14_en)
#read file
sentences_conseq_t14_en <- readRDS("content/sentences_conseq_t14_en.rds")

####consequentialism - English - topic 15:  meat consumption ----
response_conseq_t15_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about meat consumption.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t15_en$content)
rm(response_conseq_t15_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t15_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t15_en <- fromJSON(sentences_conseq_t15_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t15_en, "content/sentences_conseq_t15_en.rds")
rm(sentences_conseq_t15_en)
#read file
sentences_conseq_t15_en <- readRDS("content/sentences_conseq_t15_en.rds")


####consequentialism - English - topic 0:  topic agnostic ----
response_conseq_t0_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate English-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      without actually addressing the specific content of a topic.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t0_en$content)
rm(response_conseq_t0_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t0_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t0_en <- fromJSON(sentences_conseq_t0_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t0_en, "content/sentences_conseq_t0_en.rds")
rm(sentences_conseq_t0_en)
#read file
sentences_conseq_t0_en <- readRDS("content/sentences_conseq_t0_en.rds")

####consequentialism - German - topic 1: climate change ----
response_conseq_t1_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about actions against climate change.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t1_ger$content)
rm(response_conseq_t1_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t1_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t1_ger <- fromJSON(sentences_conseq_t1_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t1_ger, "content/sentences_conseq_t1_ger.rds")
rm(sentences_conseq_t1_ger)
#read file
sentences_conseq_t1_ger <- readRDS("content/sentences_conseq_t1_ger.rds")

####consequentialism - German - topic 2: COVID-19 restrictions ----
response_conseq_t2_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about COVID-19 restrictions.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t2_ger$content)
rm(response_conseq_t2_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t2_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t2_ger <- fromJSON(sentences_conseq_t2_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t2_ger, "content/sentences_conseq_t2_ger.rds")
rm(sentences_conseq_t2_ger)
#read file
sentences_conseq_t2_ger <- readRDS("content/sentences_conseq_t2_ger.rds")

####consequentialism - German - topic 3: immigration ----
response_conseq_t3_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about immigration and asylum policies.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t3_ger$content)
rm(response_conseq_t3_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t3_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t3_ger <- fromJSON(sentences_conseq_t3_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t3_ger, "content/sentences_conseq_t3_ger.rds")
rm(sentences_conseq_t3_ger)
#read file
sentences_conseq_t3_ger <- readRDS("content/sentences_conseq_t3_ger.rds")

####consequentialism - German - topic 4: military support ----
response_conseq_t4_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about military support in a foreign conflict.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t4_ger$content)
rm(response_conseq_t4_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t4_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t4_ger <- fromJSON(sentences_conseq_t4_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t4_ger, "content/sentences_conseq_t4_ger.rds")
rm(sentences_conseq_t4_ger)
#read file
sentences_conseq_t4_ger <- readRDS("content/sentences_conseq_t4_ger.rds")

####consequentialism - German - topic 5: sanctions ----
response_conseq_t5_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sanctioning another country.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t5_ger$content)
rm(response_conseq_t5_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t5_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t5_ger <- fromJSON(sentences_conseq_t5_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t5_ger, "content/sentences_conseq_t5_ger.rds")
rm(sentences_conseq_t5_ger)
#read file
sentences_conseq_t5_ger <- readRDS("content/sentences_conseq_t5_ger.rds")

####consequentialism - German - topic 6:  gender equality ----
response_conseq_t6_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about gender equality.
      
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t6_ger$content)
rm(response_conseq_t6_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t6_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t6_ger <- fromJSON(sentences_conseq_t6_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 


#write to file
saveRDS(sentences_conseq_t6_ger, "content/sentences_conseq_t6_ger.rds")
rm(sentences_conseq_t6_ger)
#read file
sentences_conseq_t6_ger <- readRDS("content/sentences_conseq_t6_ger.rds")

####consequentialism - German - topic 7:  minority rights ----
response_conseq_t7_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about minority rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t7_ger$content)
rm(response_conseq_t7_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t7_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t7_ger <- fromJSON(sentences_conseq_t7_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t7_ger, "content/sentences_conseq_t7_ger.rds")
rm(sentences_conseq_t7_ger)
#read file
sentences_conseq_t7_ger <- readRDS("content/sentences_conseq_t7_ger.rds")

####consequentialism - German - topic 8:  sexual orientation and gender identity rights ----
response_conseq_t8_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates  
      about sexual orientation and gender identity rights.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t8_ger$content)
rm(response_conseq_t8_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t8_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t8_ger <- fromJSON(sentences_conseq_t8_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t8_ger, "content/sentences_conseq_t8_ger.rds")
rm(sentences_conseq_t8_ger)
#read file
sentences_conseq_t8_ger <- readRDS("content/sentences_conseq_t8_ger.rds")

####consequentialism - German - topic 9:  freedom of speech vs regulation of harmful speech ----
response_conseq_t9_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about freedom of speech vs regulation of harmful speech.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t9_ger$content)
rm(response_conseq_t9_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t9_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t9_ger <- fromJSON(sentences_conseq_t9_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t9_ger, "content/sentences_conseq_t9_ger.rds")
rm(sentences_conseq_t9_ger)
#read file
sentences_conseq_t9_ger <- readRDS("content/sentences_conseq_t9_ger.rds")

####consequentialism - German - topic 10:  social welfare ----
response_conseq_t10_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about social welfare.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t10_ger$content)
rm(response_conseq_t10_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t10_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t10_ger <- fromJSON(sentences_conseq_t10_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t10_ger, "content/sentences_conseq_t10_ger.rds")
rm(sentences_conseq_t10_ger)
#read file
sentences_conseq_t10_ger <- readRDS("content/sentences_conseq_t10_ger.rds")

####consequentialism - German - topic 11:  taxation and economic redistribution ----
response_conseq_t11_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
    •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about taxation and economic redistribution.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t11_ger$content)
rm(response_conseq_t11_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t11_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t11_ger <- fromJSON(sentences_conseq_t11_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t11_ger, "content/sentences_conseq_t11_ger.rds")
rm(sentences_conseq_t11_ger)
#read file
sentences_conseq_t11_ger <- readRDS("content/sentences_conseq_t11_ger.rds")

####consequentialism - German - topic 12:  abortion laws ----
response_conseq_t12_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about abortion laws.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t12_ger$content)
rm(response_conseq_t12_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t12_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t12_ger <- fromJSON(sentences_conseq_t12_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t12_ger, "content/sentences_conseq_t12_ger.rds")
rm(sentences_conseq_t12_ger)
#read file
sentences_conseq_t12_ger <- readRDS("content/sentences_conseq_t12_ger.rds")

####consequentialism - German - topic 13:  independence movements and referendums ----
response_conseq_t13_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about independence movements and referendums.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t13_ger$content)
rm(response_conseq_t13_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t13_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t13_ger <- fromJSON(sentences_conseq_t13_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t13_ger, "content/sentences_conseq_t13_ger.rds")
rm(sentences_conseq_t13_ger)
#read file
sentences_conseq_t13_ger <- readRDS("content/sentences_conseq_t13_ger.rds")

####consequentialism - German - topic 14:  body positivity and traditional beauty standards ----
response_conseq_t14_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 
      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural tone, wording, and length found in polarized debates 
      about body positivity and pursuing traditional beauty ideals
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t14_ger$content)
rm(response_conseq_t14_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t14_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t14_ger <- fromJSON(sentences_conseq_t14_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t14_ger, "content/sentences_conseq_t14_ger.rds")
rm(sentences_conseq_t14_ger)
#read file
sentences_conseq_t14_ger <- readRDS("content/sentences_conseq_t14_ger.rds")

####consequentialism - German - topic 15:  meat consumption ----
response_conseq_t15_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
      •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      about meat consumption.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t15_ger$content)
rm(response_conseq_t15_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t15_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t15_ger <- fromJSON(sentences_conseq_t15_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t15_ger, "content/sentences_conseq_t15_ger.rds")
rm(sentences_conseq_t15_ger)
#read file
sentences_conseq_t15_ger <- readRDS("content/sentences_conseq_t15_ger.rds")


####consequentialism - German - topic 0:  topic agnostic ----
response_conseq_t0_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rule and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (harms) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth, well-being
      etc.) or negative (e.g. disadvantage, loss, pain etc.).
      
      Cue words are good first indicators, but there are structural 
      patterns that define if a sentence contains a form of reasoning  
      (even if none of the cue words are present) or if a sentence 
      doesn’t contain a form of reasoning (even if one of the cue words is 
      present). The order within a sentence may vary.
      
      Structural pattern 1: outcome-based justifications that state why a decision 
      or action is good or bad
      
      Example: Policy A is important (evaluation) 
      to avoid consequence C (outcome-based reasoning).
      
      Often the reasoning is just an emphasis on the justification 
      without an explicit evaluation of an action or decision. Rather a rule or 
      outcome is called out to indirectly influence or evaluate a decision or 
      action. 
      
      Structural pattern 2: Emphasis of quantifiable outcomes that are 
      implicitly linked to decisions or actions.
      
      Example: We need to avoid consequence C.
      
      Do not use: 
      
     •	Outcome cue words appear without  evaluation or implications for actions, 
      (e.g., “The new law had several economic effects.”, „ Das neue Gesetz 
      hatte mehrere wirtschaftliche Auswirkungen.“).
      •	Outcomes are stated as facts only, with no obvious implications for 
      actions (e.g. „ This is at a record level of 780 million euros“, „diese 
      ist auf einem rekordniveau von 780 millionen euro.“) 

      
      2. Please use the information above to generate German-language sentences that
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) match the natural wording, formality, and length found in polarized debates 
      without actually addressing the specific content of a topic.
      
      3. Within this batch, systematically vary: 
      - stance: for a topic, against the topic
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × register × tense, generate exactly 5 
      distinct sentences. Total items = 2 × 3 × 3 × 5 = 90.
      
      OUTPUT FORMAT
      Return a JSON array of exactly 90 objects with fields: topic, moral_style, 
      stance, style, tense, sentence_index, text
      Output ONLY JSON. No commentary.
           ")
    ),
    temperature = 0,
    top_p = 1,
    n = 1,
    seed = 187,
    presence_penalty = 0,
    frequency_penalty = 0
  )
)

raw_text <- rawToChar(response_conseq_t0_ger$content)
rm(response_conseq_t0_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t0_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t0_ger <- fromJSON(sentences_conseq_t0_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t0_ger, "content/sentences_conseq_t0_ger.rds")
rm(sentences_conseq_t0_ger)
#read file
sentences_conseq_t0_ger <- readRDS("content/sentences_conseq_t0_ger.rds")

###combine sentences ----

#deontological
sentences_deont_main <- rbind(sentences_deont_t0_en, sentences_deont_t0_ger,  
                              sentences_deont_t1_en, sentences_deont_t1_ger,  
                              sentences_deont_t10_en, sentences_deont_t10_ger, 
                              sentences_deont_t11_en,  sentences_deont_t11_ger,
                              sentences_deont_t12_en, sentences_deont_t12_ger, 
                              sentences_deont_t13_en,  sentences_deont_t13_ger,
                              sentences_deont_t14_en,  sentences_deont_t14_ger, 
                              sentences_deont_t15_en, sentences_deont_t15_ger, 
                              sentences_deont_t2_en,   sentences_deont_t2_ger, 
                              sentences_deont_t3_en,   sentences_deont_t3_ger,  
                              sentences_deont_t4_en,  sentences_deont_t4_ger,  
                              sentences_deont_t5_en,   sentences_deont_t5_ger, 
                              sentences_deont_t6_en,   sentences_deont_t6_ger,  
                              sentences_deont_t7_en, sentences_deont_t7_ger,  
                              sentences_deont_t8_en,   sentences_deont_t8_ger, 
                              sentences_deont_t9_en,   sentences_deont_t9_ger)

#write to file
saveRDS(sentences_deont_main, "content/sentences_deont_main.rds")

#remove individual objects
rm(sentences_deont_t0_en, sentences_deont_t0_ger,  
   sentences_deont_t1_en, sentences_deont_t1_ger,  
   sentences_deont_t10_en, sentences_deont_t10_ger, 
   sentences_deont_t11_en,  sentences_deont_t11_ger,
   sentences_deont_t12_en, sentences_deont_t12_ger, 
   sentences_deont_t13_en,  sentences_deont_t13_ger,
   sentences_deont_t14_en,  sentences_deont_t14_ger, 
   sentences_deont_t15_en, sentences_deont_t15_ger, 
   sentences_deont_t2_en,   sentences_deont_t2_ger, 
   sentences_deont_t3_en,   sentences_deont_t3_ger,  
   sentences_deont_t4_en,  sentences_deont_t4_ger,  
   sentences_deont_t5_en,   sentences_deont_t5_ger, 
   sentences_deont_t6_en,   sentences_deont_t6_ger,  
   sentences_deont_t7_en, sentences_deont_t7_ger,  
   sentences_deont_t8_en,   sentences_deont_t8_ger, 
   sentences_deont_t9_en,   sentences_deont_t9_ger)

#####read deontological sentences from file ----
sentences_deont_main <- readRDS("content/sentences_deont_main.rds")

#consequentialist
sentences_conseq_main <- rbind(sentences_conseq_t0_en, sentences_conseq_t0_ger,  
                              sentences_conseq_t1_en, sentences_conseq_t1_ger,  
                              sentences_conseq_t10_en, sentences_conseq_t10_ger, 
                              sentences_conseq_t11_en,  sentences_conseq_t11_ger,
                              sentences_conseq_t12_en, sentences_conseq_t12_ger, 
                              sentences_conseq_t13_en,  sentences_conseq_t13_ger,
                              sentences_conseq_t14_en,  sentences_conseq_t14_ger, 
                              sentences_conseq_t15_en, sentences_conseq_t15_ger, 
                              sentences_conseq_t2_en,   sentences_conseq_t2_ger, 
                              sentences_conseq_t3_en,   sentences_conseq_t3_ger,  
                              sentences_conseq_t4_en,  sentences_conseq_t4_ger,  
                              sentences_conseq_t5_en,   sentences_conseq_t5_ger, 
                              sentences_conseq_t6_en,   sentences_conseq_t6_ger,  
                              sentences_conseq_t7_en, sentences_conseq_t7_ger,  
                              sentences_conseq_t8_en,   sentences_conseq_t8_ger, 
                              sentences_conseq_t9_en,   sentences_conseq_t9_ger)
#write to file
saveRDS(sentences_conseq_main, "content/sentences_conseq_main.rds")

#remove individual objects
rm(sentences_conseq_t0_en, sentences_conseq_t0_ger,  
   sentences_conseq_t1_en, sentences_conseq_t1_ger,  
   sentences_conseq_t10_en, sentences_conseq_t10_ger, 
   sentences_conseq_t11_en,  sentences_conseq_t11_ger,
   sentences_conseq_t12_en, sentences_conseq_t12_ger, 
   sentences_conseq_t13_en,  sentences_conseq_t13_ger,
   sentences_conseq_t14_en,  sentences_conseq_t14_ger, 
   sentences_conseq_t15_en, sentences_conseq_t15_ger, 
   sentences_conseq_t2_en,   sentences_conseq_t2_ger, 
   sentences_conseq_t3_en,   sentences_conseq_t3_ger,  
   sentences_conseq_t4_en,  sentences_conseq_t4_ger,  
   sentences_conseq_t5_en,   sentences_conseq_t5_ger, 
   sentences_conseq_t6_en,   sentences_conseq_t6_ger,  
   sentences_conseq_t7_en, sentences_conseq_t7_ger,  
   sentences_conseq_t8_en,   sentences_conseq_t8_ger, 
   sentences_conseq_t9_en,   sentences_conseq_t9_ger)

#####read deontological sentences from file ----
sentences_conseq_main <- readRDS("content/sentences_conseq_main.rds")


#old ----

#deontology (code that produced sentences for pre-study but doesn't fully reproduce the same sentences)
response_deont_raw <- POST(
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

raw_text <- rawToChar(response_deont_raw$content)
rm(response_deont_raw)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

response_deont_pre <- parsed$choices$message$content
rm(parsed)

#write to file
writeLines(response_deont_pre, "content/sentences_deont_pre.txt")
rm(response_deont_pre)

#load file
sentences_deont_pre <- readLines("content/sentences_deont_pre.txt") 

sentences_deont_pre %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s"),
    value = str_remove(value, '"')
  ) %>% 
  pull(value)

#write to file
writeLines(sentences_deont_pre, "content/sentences_deont_pre.txt")

####load pre-processed file ----
sentences_deont_pre <- readLines("content/sentences_deont_pre.txt") 

#expanding the consequentialism seed dictionary to sentences
response_conseq_raw <- POST(
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

raw_text <- rawToChar(response_conseq_raw$content)
rm(response_conseq_raw)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

response_conseq_pre <- parsed$choices$message$content
rm(parsed)

#write to file
writeLines(response_conseq_pre, "content/sentences_conseq_pre.txt")
rm(response_conseq_pre)

#load file
sentences_conseq_pre <- readLines("content/sentences_conseq_pre.txt") 

sentences_conseq_pre %<>% 
  as_tibble() %>% 
  filter(str_detect(value, "^\\d")) %>%
  mutate(
    value = str_remove(value, "^\\d+.\\s"),
    value = str_remove(value, '"')
  ) %>% 
  pull(value)

#write to file
writeLines(sentences_conseq_pre, "content/sentences_conseq_pre.txt")

####load pre-processed file ----
sentences_conseq_pre <- readLines("content/sentences_conseq_pre.txt") 

#under construction ----

#word embeddings for dictionaries
deont_embeddings <- multi_lang$encode(deont)
conseq_embeddings <- multi_lang$encode(conseq)

#compute the average vector (DDR representation)
deont_ddr_vector <- apply(deont_embeddings, 2, mean)
conseq_ddr_vector <- apply(conseq_embeddings, 2, mean)

#calculate cosine similarity
#cosine similarity function - needs reference and triple checking/ potentially not needed later
cosine_similarity <- function(x, y) {
  x_norms <- sqrt(rowSums(x^2))
  y_norm <- sqrt(sum(y^2))
  dot_products <- x %*% y
  sims <- dot_products / (x_norms * y_norm)
  as.numeric(sims)
}
cosine(deont_ddr_vector, conseq_ddr_vector) #fairly high but might be due to nature of word lists

#add cosine similarity to data ----tbd

##2.2. Seed dictionaries ----
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

##2.3. expanding the deontology seed dictionary to sentences ----
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

