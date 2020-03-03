## java location
#Sys.setenv(JAVA_HOME='C:\\Program Files\\R\\R-3.6.2\\etc\\java\\jdk1.8')
Sys.setenv(JAVA_HOME="C:/Program Files/R/R-3.6.2/etc/java/jdk1.8/jre")

library(rJava)

#install.packages("openNLPmodels.en",
#                 repos = "http://datacube.wu.ac.at/",
#                 type = "source")
#install.packages("RWeka")
#install.packages("magrittr")
#install.packages("NLP")
#install.packages("openNLP")
#install.packages("pdftools")
#install.packages("openNLP")
#install.packages("devtools")
install.packages("C:/Users/aaron.derner/Downloads/pdfbox-2.0.19.jar")

[CRAN_Status_Badge](http://www.r-pkg.org/badges/version/pdfbox)
(https://cran.r-project.org/package=pdfbox)

library(NLP)
library(openNLP)
library(RWeka)
library(pdftools)
library(openNLP)
library(magrittr)
library(pdfbox)
devtools::install_github("hrbrmstr/pdfboxjars")
devtools::install_github("hrbrmstr/pdfbox")


## new pdf reader (PDFBox)
# java from - https://stackoverflow.com/questions/9451312/pdfbox-extracting-paragraphs

#public static void main(String[] args) throws InvalidPasswordException, IOException {
#  File file = new File("File Path");
#  PDDocument document = PDDocument.load(file);        
#  PDFTextStripper pdfStripper = new PDFTextStripper();
#  pdfStripper.setParagraphStart("/t");
#  pdfStripper.setSortByPosition(true);
  
  
#  for (String line: pdfStripper.getText(document).split(pdfStripper.getParagraphStart()))
#  {
#    System.out.println(line);
#    System.out.println("********************************************************************");
#  }
#}

## for later development - read out exact location of paragraph in originating document
## include that value in parentheses at the conclusion of the paragraph
## of course document name (a variable), ideally an html link; e.g. https://adst.org/collection/lastname.firstname.toc.pdf


## reading and importing pdf
setwd ("G:/My Drive/KNIME/PDFparsing")
files = list.files(pattern = "pdf$")
histories = lapply(files, pdf_text)
length(histories)
lapply(histories, length)
pdf_data(files)

print(histories)

histories = as.String(histories)

##annotations

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

hist_annotations <- annotate(histories, list(sent_ann, word_ann))

class(hist_annotations)

head(hist_annotations)

hist_doc <- AnnotatedPlainTextDocument(histories, hist_annotations)

sents(hist_doc) %>% head(2)
words(hist_doc) %>% head(10)

##nlp

person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")
organization_ann = Maxent_Entity_Annotator(kind = "organization")

pipeline = list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
hist_annotations <- annotate(histories, pipeline)
hist_doc <- AnnotatedPlainTextDocument(histories, hist_annotations)

sents(hist_doc) %>% head(2)
words(hist_doc) %>% head(10)
tagged_paras(hist_doc, )



entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

##extract

## next step - identify names, orgs, etc. collocated in a para with a relevant location, then scan
## proximate paragraphs for relevant inclusion

#entities(hist_doc, kind = "person")
#entities(hist_doc, kind = "organization")

entities(hist_doc, kind = "location")


##paragraphs and sentences

hist_doc = as.character(hist_doc)

toMatch = ("Pakistan")

sentences<-unlist(strsplit(hist_doc,split="\\."))
sentences[grep(paste(toMatch, collapse="|"),sentences)]


foo<-function(Match){sentences[grep(Match,sentences)]}
lapply(toMatch,foo)



