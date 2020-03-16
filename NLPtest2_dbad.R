library(pdftools)
library(tidyverse)
library(cleanNLP)
library(reticulate)
##cleannlp <- import("cleannlp")
use_python("C:/Users/atder/AppData/Local/Programs/Python/Python38/python.exe", required = TRUE)
# initiate the backend, using spacy for entity recognition
#py_install("cleannlp")
#cleannlp <- import("cleannlp")

cnlp_download_spacy("en")
cnlp_init_spacy()

reticulate::py_config()
## reading and importing pdf
#setwd ("G:/My Drive/KNIME/PDFparsing")
#files = list.files(pattern = "pdf$")

files <- c('https://twiplomacy.com/wp-content/uploads/2019/04/World-Leaders-on-Facebook-Study-2019.pdf')
histories = pdf_text(files[1])
histories = lapply(files, pdf_text)

histories_df <- 
  histories %>% 
  map(as_tibble) %>% 
  bind_rows %>%
  rowid_to_column(var='page_number') %>% 
  mutate(page_number = as.numeric(page_number)) %>%
  # split the pages at the sentence level
  # you could also use a MAXENT sentence recognition to be more exact
  mutate(value = str_split(value, "\\.[\\s+]")) %>% 
  tidyr::unnest() %>% 
  mutate(value = trimws(value)) %>% 
  group_by(page_number) %>% 
  mutate(sentence_number = as.numeric(1:n())) %>% 
  ungroup() %>% 
  mutate(page_sentence_id = paste(page_number, sentence_number, sep = '_'))

# annotate the documents
ann <- cnlp_annotate(input = c(histories_df %>% pull(value) %>% head(50)), backend='spacy')
ann$entity

## next step - identify names, orgs, etc. collocated in a para with a relevant location, then scan
## proximate paragraphs for relevant inclusion

# the types of entities analyze when looking for keywords (https://spacy.io/api/annotation)
included_entities <- c('LOCATION', 'PERSON', 'GPE', 'ORG', 'NORP')
keywords <- c('Indian', 'Asia', 'China', 'UN')

# pages which contain the keyword
keyword_page_numbers <- 
  ann$entity %>% 
  filter(entity_type %in% included_entities) %>% 
  # this filter is based on detected words, rather than exact match
  filter(str_detect(entity, paste(keywords, collapse = '|'))) %>% 
  # to do an exact match, uncomment the line below
  #filter(entity %in% keywords) %>% 
  select(doc_id, sid) %>% 
  mutate(page_sentence_id = paste(doc_id, sid, sep="_"))

# extract the sentences that match the page number (could also be done at the page level)
# or the paragraph level with a little more preparation
keyword_matches <-
  histories_df %>% 
  filter(page_sentence_id %in% keyword_page_numbers$page_sentence_id) %>% 
  select(page_number, sentence_number, value)

keyword_matches


