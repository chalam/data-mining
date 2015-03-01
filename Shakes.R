# http://www.exegetic.biz/blog/2013/09/text-mining-the-complete-works-of-william-shakespeare/
  
TEXTFILE = "shakespeare.txt"
if (!file.exists(TEXTFILE)) {
      download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}
shakespeare = readLines(TEXTFILE)
length(shakespeare)

shakespeare = shakespeare[-(1:173)] # copyright header
shakespeare = shakespeare[-(124195:length(shakespeare))] # copyright footer
shakespeare = paste(shakespeare, collapse = " ")
nchar(shakespeare)

shakespeare = strsplit(shakespeare, "<<[^>]*>>")[[1]] # split on play header
length(shakespeare)

(dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE)) # Person info
length(shakespeare)
shakespeare = shakespeare[-dramatis.personae]
length(shakespeare)

## make a corpus
library(tm)
doc.vec = VectorSource(shakespeare)
doc.corpus = Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))

## stemming -> "run", "runs" and "running" all become "run"
library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)

doc.corpus <- tm_map(doc.corpus, stripWhitespace)

inspect(doc.corpus[8])

# create TDM
TDM <- TermDocumentMatrix(doc.corpus)
TDM
inspect(TDM[1:10,1:10])

# create DTM, transpose
# DTM <- DocumentTermMatrix(doc.corpus)
# DTM
# inspect(DTM[1:10,1:10])

# what are the most frequently occurring terms?
findFreqTerms(TDM, 2000)

# What about associations between words? Let's have a look at what other words had a high association with "love".
findAssocs(TDM, "love", 0.8)

# remove these sparse terms
TDM.common = removeSparseTerms(TDM, 0.1)
dim(TDM)
dim(TDM.common)

# From the 18651 terms that we started with, we are now left with a TDM which considers on 71 commonly occurring terms.

inspect(TDM.common[1:10,1:10])

# convert sparse matrix to normal matrix
library(slam)
TDM.dense <- as.matrix(TDM.common)
TDM.dense
object.size(TDM.common)
object.size(TDM.dense)


library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color = palette)


# convert data
library(reshape2)
TDM.dense.clean = melt(TDM.dense, value.name = "count")
head(TDM.dense.clean)

# ggplot2 to make up an attractive heat map.
library(ggplot2)
ggplot(TDM.dense.clean, aes(x = Docs, y = Terms, fill = log10(count))) +
  +     geom_tile(colour = "white") +
  +     scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  +     ylab("") +
  +     theme(panel.background = element_blank()) +
  +     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
