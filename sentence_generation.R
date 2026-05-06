#Script to produce exemplary reasoning sentences for Distributed Dictionary 
#Representations Method (Garten et al., 2018) to sample larger corpus


#0.Set up ----

#load R packages
library(tidyverse) #for general data wrangling
library(magrittr) #just for the %<>% operator out of laziness
library(httr) #for accessing the open AI api
library(jsonlite) #for dealing with json files


#1. Generate Sentences based on definition and abbreviated LIWC dictionaries ---- 

#open ai api key
api_key <- read_lines("api_key.txt")


#1.1. Deontology - English ----

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties - 
      often implying a prioritization over outcomes. 
      
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions against climate change that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about COVID-19 restrictions that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about refugees and immigration that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about arms deliveries and military support in foreign conflicts that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about sanctions against foreign countries that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support gender equality that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - English - topic 7: ethnicity minority rights ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
     2. Please generate typical English-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to ethnicity that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support rights and tackle discrimination relating to 
      sexual orientation and diverse gender identities that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - English - topic 9: hate speech, harrassment and misinformation ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions against hate speech, harrassment and misinformation that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about social protection and social security systems (e.g. healthcare 
      coverage, housing support, unemployment benefits) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about corporate taxation and economic redistribution that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about more liberal abortion laws that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about political self-determination decided through democratic votes (e.g. 
      referendums on aindependence or withdrawal from political unions) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - English - topic 14:  beauty ideals ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about the right to pursue traditional beauty ideals through cosmetic 
      products and procedures that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about animal rights and reducing meat consumption that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - English - topic 16:  culture and religion ----
response_deont_t16_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to religion and culture that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

raw_text <- rawToChar(response_deont_t16_en$content)
rm(response_deont_t16_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t16_en <- parsed$choices$message$content
rm(parsed)

sentences_deont_t16_en <- fromJSON(sentences_deont_t16_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t16_en, "content/sentences_deont_t16_en.rds")
rm(sentences_deont_t16_en)

#read file
sentences_deont_t16_en <- readRDS("content/sentences_deont_t16_en.rds")

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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical English-language sentences from polarized debates 
      about a decision or action (without referencing the topic or domain) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

#1.2. Deontology - German ----

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Rule-based moral reasoning (Deontology)
      
      Appealing to rules, rights or duties - 
      often implying a prioritization over outcomes. 
      
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions against climate change that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about COVID-19 restrictions that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about refugees and immigration that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about arms deliveries and military support in foreign conflicts that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about sanctions against foreign countries that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support gender equality that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - German - topic 7: ethnicity minority rights ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
     2. Please generate typical German-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to ethnicity that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support rights and tackle discrimination relating to 
      sexual orientation and diverse gender identities that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - German - topic 9: hate speech, harrassment and misinformation ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions against hate speech, harrassment and misinformation that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about social protection and social security systems (e.g. healthcare 
      coverage, housing support, unemployment benefits) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about corporate taxation and economic redistribution that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about more liberal abortion laws that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about political self-determination decided through democratic votes (e.g. 
      referendums on aindependence or withdrawal from political unions) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - German - topic 14:  beauty ideals ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about the right to pursue traditional beauty ideals through cosmetic 
      products and procedures that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about animal rights and reducing meat consumption that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####deontology - German - topic 16:  culture and religion ----
response_deont_t16_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to religion and culture that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

raw_text <- rawToChar(response_deont_t16_ger$content)
rm(response_deont_t16_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_deont_t16_ger <- parsed$choices$message$content
rm(parsed)

sentences_deont_t16_ger <- fromJSON(sentences_deont_t16_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_deont_t16_ger, "content/sentences_deont_t16_ger.rds")
rm(sentences_deont_t16_ger)

#read file
sentences_deont_t16_ger <- readRDS("content/sentences_deont_t16_ger.rds")

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
           analysis. You strictly follow provided definitions, rules and parameters."),
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
      
      Example: It is our duty to protect right B.
      
      2. Please generate typical German-language sentences from polarized debates 
      about a decision or action (without referencing the topic or domain) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

#1.3. consequentialism - English ----

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions against climate change that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about COVID-19 restrictions that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about refugees and immigration that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about arms deliveries and military support in foreign conflicts that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about sanctions against foreign countries that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support gender equality that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - English - topic 7: ethnicity minority rights ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
     2. Please generate typical English-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to ethnicity that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support rights and tackle discrimination relating to 
      sexual orientation and diverse gender identities that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - English - topic 9: hate speech, harrassment and misinformation ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions against hate speech, harrassment and misinformation that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
     1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about social protection and social security systems (e.g. healthcare 
      coverage, housing support, unemployment benefits) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about corporate taxation and economic redistribution that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about more liberal abortion laws that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about political self-determination decided through democratic votes (e.g. 
      referendums on aindependence or withdrawal from political unions) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - English - topic 14:  beauty ideals ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about the right to pursue traditional beauty ideals through cosmetic 
      products and procedures that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about animal rights and reducing meat consumption that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - English - topic 16:  culture and religion ----
response_conseq_t16_en <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to religion and culture that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

raw_text <- rawToChar(response_conseq_t16_en$content)
rm(response_conseq_t16_en)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t16_en <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t16_en <- fromJSON(sentences_conseq_t16_en, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t16_en, "content/sentences_conseq_t16_en.rds")
rm(sentences_conseq_t16_en)

#read file
sentences_conseq_t16_en <- readRDS("content/sentences_conseq_t16_en.rds")

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical English-language sentences from polarized debates 
      about a decision or action (without referencing the topic or domain) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

#1.4. consequentialism - German ----

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions against climate change that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about COVID-19 restrictions that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about refugees and immigration that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about arms deliveries and military support in foreign conflicts that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
    1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about sanctions against foreign countries that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support gender equality that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - German - topic 7: ethnicity minority rights ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
     2. Please generate typical German-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to ethnicity that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support rights and tackle discrimination relating to 
      sexual orientation and diverse gender identities that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - German - topic 9: hate speech, harrassment and misinformation ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions against hate speech, harrassment and misinformation that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about social protection and social security systems (e.g. healthcare 
      coverage, housing support, unemployment benefits) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about corporate taxation and economic redistribution that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about more liberal abortion laws that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
     1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about political self-determination decided through democratic votes (e.g. 
      referendums on aindependence or withdrawal from political unions) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - German - topic 14:  beauty ideals ----
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about the right to pursue traditional beauty ideals through cosmetic 
      products and procedures that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
     1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about animal rights and reducing meat consumption that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

####consequentialism - German - topic 16:  culture and religion ----
response_conseq_t16_ger <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", api_key)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-5.2",
    messages = list(
      list(role = "system", content = "You are a computational social scientist 
           generating theory-aligned prototype texts for moral reasoning 
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
     1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about actions to support diversity, minority rights and tackle 
      discrimination relating to religion and culture that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

raw_text <- rawToChar(response_conseq_t16_ger$content)
rm(response_conseq_t16_ger)

# Parse JSON to a list
parsed <- fromJSON(raw_text)
rm(raw_text)

#convert to tibble
parsed %>% pluck(1) %>%
  as_tibble()

sentences_conseq_t16_ger <- parsed$choices$message$content
rm(parsed)

sentences_conseq_t16_ger <- fromJSON(sentences_conseq_t16_ger, simplifyDataFrame = TRUE) %>%
  as_tibble() 

#write to file
saveRDS(sentences_conseq_t16_ger, "content/sentences_conseq_t16_ger.rds")
rm(sentences_conseq_t16_ger)

#read file
sentences_conseq_t16_ger <- readRDS("content/sentences_conseq_t16_ger.rds")

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
           analysis. You strictly follow provided definitions, rules and parameters."),
      list(role = "user", content = "
      
      1. Conceptual Definition: Outcome-Based Reasoning (Consequentialism)
      
      Appealing to quantifiable outcomes of actions (or inactions), often framed 
      as good (benefits) or bad (costs) - implying a cost-benefit analysis. 
      
      Potential cue words: e.g. outcomes, consequences, results, effects etc.; 
      positive (e.g. advantage, benefit, happiness, health, wealth
      etc.) or negative (e.g. cost, disadvantage, loss, pain etc.).
      
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
      
      Example: What matters is consequence C.
      
      2. Please generate typical German-language sentences from polarized debates 
      about a decision or action (without referencing the topic or domain) that
      
      (a) clearly reflect the moral reasoning style described above
      (b) vary key justification and sentence structure
      (c) mimic the natural vocabulary, tone, and length found in those debates 
      
      3. Within this batch, systematically vary: 
      - stance: for or against
      - style: political speech, social media comment, newspaper article 
      - tense: past, present, future 
      
      For each combination of stance × style × tense, generate exactly 5 
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

#2.1. combine sentences ----

#deontological - English 
sentences_deont_en <- rbind(sentences_deont_t0_en, 
                              sentences_deont_t1_en, 
                              sentences_deont_t10_en,  
                              sentences_deont_t11_en,  
                              sentences_deont_t12_en, 
                              sentences_deont_t13_en,  
                              sentences_deont_t14_en, 
                              sentences_deont_t15_en, 
                              sentences_deont_t16_en, 
                              sentences_deont_t2_en,   
                              sentences_deont_t3_en,  
                              sentences_deont_t4_en,  
                              sentences_deont_t5_en,   
                              sentences_deont_t6_en,   
                              sentences_deont_t7_en, 
                              sentences_deont_t8_en,   
                              sentences_deont_t9_en) %>%
  mutate(language = "English")

#deontological - German 
sentences_deont_ger <- rbind(sentences_deont_t0_ger, 
                            sentences_deont_t1_ger, 
                            sentences_deont_t10_ger,  
                            sentences_deont_t11_ger,  
                            sentences_deont_t12_ger, 
                            sentences_deont_t13_ger,  
                            sentences_deont_t14_ger, 
                            sentences_deont_t15_ger, 
                            sentences_deont_t16_ger, 
                            sentences_deont_t2_ger,   
                            sentences_deont_t3_ger,  
                            sentences_deont_t4_ger,  
                            sentences_deont_t5_ger,   
                            sentences_deont_t6_ger,   
                            sentences_deont_t7_ger, 
                            sentences_deont_t8_ger,   
                            sentences_deont_t9_ger) %>%
  mutate(language = "German")

sentences_deont_main <- rbind(sentences_deont_en, sentences_deont_ger)
rm(sentences_deont_en)
rm(sentences_deont_ger)

sentences_deont_main %<>% 
  mutate(
    moral_style = case_when(
      moral_style %in% c("Deontologie", "Deontologie (regelbasiert)", "Deontology",
                         "Deontology (rule-based moral reasoning)", "deontology", "deontology_rule_based")
      ~ "deontological"
    ),
    stance = case_when(
      stance %in% c("against", "contra", "dagegen", "gegen")   ~ "against",
      stance %in% c("dafür", "for", "pro", "für") ~ "for",
    ),
    style = case_when(
      style %in% c("Social-Media-Kommentar", "social media comment", "social_media_comment")   ~ "social media",
      style %in% c("Zeitungsartikel", "newspaper article", "newspaper_article") ~ "newspaper",
      style %in% c("political speech", "politische Rede", "political_speech") ~ "politics",
    ),
    tense = case_when(
      tense %in% c("Gegenwart", "present")   ~ "present",
      tense %in% c("Zukunft", "future") ~ "future",
      tense %in% c("Vergangenheit", "past") ~ "past",
    ),
    topic = case_when(
      topic %in% c("actions against climate change", "Klimaschutzmaßnahmen", "climate change actions", "climate change action")   ~ "t1_climate",
      topic %in% c("COVID-19 restrictions", "COVID-19-Beschränkungen") ~ "t2_covid",
      topic %in% c("Immigration and asylum policies", "immigration and asylum policies", "refugees_immigration", "refugees and immigration") ~ "t3_immigration",
      topic %in% c("arms deliveries and military support in foreign conflicts", "Waffenlieferungen und militärische Unterstützung in Auslandskonflikten")   ~ "t4_militarysupport",
      topic %in% c("Sanktionen gegen ausländische Staaten", "sanctions against foreign countries") ~ "t5_sanctions",
      topic %in% c("actions to support gender equality") ~ "t6_genderequality",
      topic %in% c("ethnicity, diversity, minority rights, anti-discrimination actions", "ethnicity_diversity_anti_discrimination", "ethnicity_diversity_minority_rights_anti_discrimination")   ~ "t7_ethincity",
      topic %in% c("Rechte und Antidiskriminierung zu sexueller Orientierung und geschlechtlicher Vielfalt", "rights and anti-discrimination related to sexual orientation and diverse gender identities", "Rechte und Antidiskriminierung zu sexueller Orientierung und Geschlechtsidentität") ~ "t8_lgbtqrights",
      topic %in% c("actions against hate speech, harassment and misinformation", "actions against hate speech, harassment, and misinformation", "Maßnahmen gegen Hassrede, Belästigung und Desinformation") ~ "t9_hatespeech",
      topic %in% c("Sozialschutz und soziale Sicherungssysteme", "social protection and social security systems")   ~ "t10_socialwelfare",
      topic %in% c("corporate taxation and economic redistribution", "Unternehmensbesteuerung und Umverteilung") ~ "t11_taxation",
      topic %in% c("liberalere Abtreibungsgesetze", "more liberal abortion laws") ~ "t12_abortionlaws",
      topic %in% c("democratic self-determination referendums", "politische Selbstbestimmung per Volksabstimmung", "democratic votes on political self-determination") ~ "t13_referendums",
      topic %in% c("Recht auf traditionelle Schönheitsideale durch Kosmetik und Eingriffe", "right to pursue traditional beauty ideals through cosmetic products and procedures", "cosmetic products and procedures for traditional beauty ideals")   ~ "t14_beautyideals",
      topic %in% c("less meat consumption", "weniger Fleischkonsum", "Tierrechte & Fleischkonsum reduzieren", "animal rights and reducing meat consumption", "Tierrechte und Fleischkonsum reduzieren") ~ "t15_meat",
      topic %in% c("Maßnahmen für Vielfalt, Minderheitenrechte und Antidiskriminierung (Religion/Kultur)", "religion_culture_diversity_minority_rights_anti_discrimination", "Religion und Kultur: Diversität, Minderheitenrechte, Antidiskriminierung") ~ "t16_religion",
      topic %in% c("generic decision/action", "unspecified") ~ "t0_notopic",
    )
  )

sentences_deont_main %>%
  count(topic)


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
   sentences_deont_t16_en, sentences_deont_t16_ger, 
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

#consequentialism  - English 
sentences_conseq_en <- rbind(sentences_conseq_t0_en, 
                            sentences_conseq_t1_en, 
                            sentences_conseq_t10_en,  
                            sentences_conseq_t11_en,  
                            sentences_conseq_t12_en, 
                            sentences_conseq_t13_en,  
                            sentences_conseq_t14_en, 
                            sentences_conseq_t15_en, 
                            sentences_conseq_t16_en,
                            sentences_conseq_t2_en,   
                            sentences_conseq_t3_en,  
                            sentences_conseq_t4_en,  
                            sentences_conseq_t5_en,   
                            sentences_conseq_t6_en,   
                            sentences_conseq_t7_en, 
                            sentences_conseq_t8_en,   
                            sentences_conseq_t9_en) %>%
  mutate(language = "English")

#consequentialism - German 
sentences_conseq_ger <- rbind(sentences_conseq_t0_ger, 
                             sentences_conseq_t1_ger, 
                             sentences_conseq_t10_ger,  
                             sentences_conseq_t11_ger,  
                             sentences_conseq_t12_ger, 
                             sentences_conseq_t13_ger,  
                             sentences_conseq_t14_ger, 
                             sentences_conseq_t15_ger, 
                             sentences_conseq_t16_ger, 
                             sentences_conseq_t2_ger,   
                             sentences_conseq_t3_ger,  
                             sentences_conseq_t4_ger,  
                             sentences_conseq_t5_ger,   
                             sentences_conseq_t6_ger,   
                             sentences_conseq_t7_ger, 
                             sentences_conseq_t8_ger,   
                             sentences_conseq_t9_ger) %>%
  mutate(language = "German")

sentences_conseq_main <- rbind(sentences_conseq_en, sentences_conseq_ger)
rm(sentences_conseq_en)
rm(sentences_conseq_ger)

sentences_conseq_main %<>% 
  mutate(
    moral_style = case_when(
      moral_style %in% c("Outcome-Based Reasoning (Consequentialism)",
                         "outcome-based reasoning (consequentialism)",
                         "consequentialist", "outcome_based_reasoning")
      ~ "consequentialist",
    ),
    stance = case_when(
      stance %in% c("against", "contra", "dagegen", "gegen")   ~ "against",
      stance %in% c("dafür", "for", "pro", "für") ~ "for",
    ),
    style = case_when(
      style %in% c("Social-Media-Kommentar", "social media comment", "social_media_comment")   ~ "social media",
      style %in% c("Zeitungsartikel", "newspaper article", "newspaper_article") ~ "newspaper",
      style %in% c("political speech", "politische Rede", "political_speech") ~ "politics",
    ),
    tense = case_when(
      tense %in% c("Gegenwart", "present")   ~ "present",
      tense %in% c("Zukunft", "future") ~ "future",
      tense %in% c("Vergangenheit", "past") ~ "past",
    ),
    topic = case_when(
      topic %in% c("actions against climate change", "Klimaschutzmaßnahmen", "climate change actions", "climate change action")   ~ "t1_climate",
      topic %in% c("COVID-19 restrictions", "COVID-19-Beschränkungen") ~ "t2_covid",
      topic %in% c("Immigration and asylum policies", "immigration and asylum policies", "refugees_immigration", "refugees and immigration") ~ "t3_immigration",
      topic %in% c("arms deliveries and military support in foreign conflicts", "Waffenlieferungen und militärische Unterstützung in Auslandskonflikten")   ~ "t4_militarysupport",
      topic %in% c("Sanktionen gegen ausländische Staaten", "sanctions against foreign countries") ~ "t5_sanctions",
      topic %in% c("actions to support gender equality") ~ "t6_genderequality",
      topic %in% c("ethnicity, diversity, minority rights, anti-discrimination actions", "ethnicity_diversity_anti_discrimination", "ethnicity_diversity_minority_rights_anti_discrimination")   ~ "t7_ethincity",
      topic %in% c("Rechte und Antidiskriminierung zu sexueller Orientierung und geschlechtlicher Vielfalt", "rights and anti-discrimination related to sexual orientation and diverse gender identities", "Rechte und Antidiskriminierung zu sexueller Orientierung und Geschlechtsidentität") ~ "t8_lgbtqrights",
      topic %in% c("actions against hate speech, harassment and misinformation", "actions against hate speech, harassment, and misinformation", "Maßnahmen gegen Hassrede, Belästigung und Desinformation") ~ "t9_hatespeech",
      topic %in% c("Sozialschutz und soziale Sicherungssysteme", "social protection and social security systems")   ~ "t10_socialwelfare",
      topic %in% c("corporate taxation and economic redistribution", "Unternehmensbesteuerung und Umverteilung") ~ "t11_taxation",
      topic %in% c("liberalere Abtreibungsgesetze", "more liberal abortion laws") ~ "t12_abortionlaws",
      topic %in% c("democratic self-determination referendums", "politische Selbstbestimmung per Volksabstimmung", "democratic votes on political self-determination") ~ "t13_referendums",
      topic %in% c("Recht auf traditionelle Schönheitsideale durch Kosmetik und Eingriffe", "right to pursue traditional beauty ideals through cosmetic products and procedures", "cosmetic products and procedures for traditional beauty ideals")   ~ "t14_beautyideals",
      topic %in% c("less meat consumption", "weniger Fleischkonsum", "Tierrechte & Fleischkonsum reduzieren", "animal rights and reducing meat consumption", "Tierrechte und Fleischkonsum reduzieren") ~ "t15_meat",
      topic %in% c("Maßnahmen für Vielfalt, Minderheitenrechte und Antidiskriminierung (Religion/Kultur)", "religion_culture_diversity_minority_rights_anti_discrimination", "Religion und Kultur: Diversität, Minderheitenrechte, Antidiskriminierung") ~ "t16_religion",
      topic %in% c("generic decision/action", "unspecified") ~ "t0_notopic",
    )
  )

sentences_conseq_main %>%
  count(topic)

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
   sentences_conseq_t16_en, sentences_conseq_t16_ger, 
   sentences_conseq_t2_en,   sentences_conseq_t2_ger, 
   sentences_conseq_t3_en,   sentences_conseq_t3_ger,  
   sentences_conseq_t4_en,  sentences_conseq_t4_ger,  
   sentences_conseq_t5_en,   sentences_conseq_t5_ger, 
   sentences_conseq_t6_en,   sentences_conseq_t6_ger,  
   sentences_conseq_t7_en, sentences_conseq_t7_ger,  
   sentences_conseq_t8_en,   sentences_conseq_t8_ger, 
   sentences_conseq_t9_en,   sentences_conseq_t9_ger)

#####read consequentialis sentences from file ----
sentences_conseq_main <- readRDS("content/sentences_conseq_main.rds")

sentences_main <- rbind(sentences_conseq_main, sentences_deont_main)
write_excel_csv(sentences_main, "content/sentences_main.csv")
