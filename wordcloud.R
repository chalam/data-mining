library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(slam)

setwd("~/project/data-mining")
# doc <- readLines("SOTU 2015.txt")
doc <- readLines("SOTU_Small.txt")
# doc <- readLines("shakespeare.txt")
length(doc)
head(doc)
tail(doc)

doc.vec <- VectorSource(doc)

doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, tolower)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removePunctuation)

# doc.corpus <- tm_map(doc.corpus, removeWords, c("nytimes", "2014"))
# doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"), lazy=TRUE)
# doc.corpus <- tm_map(doc.corpus, stemDocument, lazy=TRUE)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
doc.corpus <- tm_map(doc.corpus, PlainTextDocument)

inspect(doc.corpus[1])

# wordcloud(doc.corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

# doc.dtm <- DocumentTermMatrix(doc.corpus)
# inspect(doc.dtm)

doc.tdm <- TermDocumentMatrix(doc.corpus)

findFreqTerms(doc.tdm, 2) #most frequently occurring terms

findAssocs(doc.tdm, "thank", 0.1) #associations between words for thank

doc.tdm.common = removeSparseTerms(doc.tdm, 0.1)
doc.tdm.common = doc.tdm

dim(doc.tdm)
dim(doc.tdm.common)
inspect(doc.tdm)
inspect(doc.tdm.common)
doc.tdm.dense <- as.matrix(doc.tdm.common)
object.size(doc.tdm.common)
object.size(doc.tdm.dense)
head(doc.tdm.dense)

palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(doc.tdm.dense), rowSums(doc.tdm.dense), min.freq = 1, color = palette)

library(ggplot2)
ggplot(doc.tdm.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
       geom_tile(colour = "white") 
       scale_fill_gradient(high="#FF0000" , low="#FFFFFF")
       ylab("") 
       theme(panel.background = element_blank()) 
       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
