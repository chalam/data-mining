library(arules)

# ## Create transaction data set.
# data <- list(
#   c("a","b","c"),
#   c("a","b"),
#   c("a","b","d"),
#   c("b","e"),
#   c("b","c","e"),
#   c("a","d","e"),
#   c("a","c"),
#   c("a","b","d"),
#   c("c","e"),
#   c("a","b","d","e")
# )

tr<-read.transactions("transactions.txt",format="basket",sep=",")
inspect(tr)
image(tr)
itemFrequencyPlot(tr, support = 0.1)
length(tr)
rules <- apriori(tr, parameter= list(supp=0.5, conf=0.5))
rules
length(rules)
inspect(rules)
summary(rules)


## graph it
library(arulesViz)
plot(rules)
plot(rules, method="paracoord", control=list(reorder=TRUE))

## Mine itemsets with tidLists.
f <- eclat(tr, parameter = list(support = 0.5, tidLists = TRUE))

## Get dimensions of the tidLists.
dim(tidLists(f))

## Coerce tidLists to list.
as(tidLists(f), "list")

## Inspect visually.
image(tidLists(f))

##Show the Frequent itemsets and respectives supports
inspect(f)


