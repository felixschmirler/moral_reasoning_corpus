#classification of moral reasoning styles

#load packages 
library(tidyverse)
library(magrittr)
library(xml2)

#Dictionary-based ----

# Load the .dicx file
doc <- read_xml("moral-justification-dictionary.dicx")

# Extract categories and words
categories <- xml_find_all(doc, "//category")
category_names <- xml_attr(categories, "name")
category_ids <- xml_attr(categories, "id")

# Get word entries and their associated categories
entries <- xml_find_all(doc, "//entry")

word_list <- lapply(entries, function(e) {
  word <- xml_attr(e, "word")
  cats <- xml_find_all(e, "category/@id")
  list(word = word, categories = xml_text(cats))
})

# Convert to a data frame or process as needed
