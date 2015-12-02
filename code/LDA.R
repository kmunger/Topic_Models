
#### Dec 2015 NYU Data Lab--Topic Models
#     Kevin Munger



###Running LDA


###Make sure you have the appropriate packages installed

install.packages("quanteda")
install.packages("topicmodels")
install.packages("ggplot2")

library(quanteda)
library(topicmodels)
library(ggplot2)

###First, you need to go to my github and download the data

###Save the two folders to your desktop


setwd("C:/Users/kevin/Desktop")


###Get the list of files
g1 <- list.files("MA paper/govdates/", full.names=TRUE)
g2 <- list.files("MA paper/oppdates/",  full.names=TRUE)
files<-c(g1, g2)



###read in the tweets
tweets <- lapply(files, readLines)


##Combine all the tweets per day to form the documents
tweets<-lapply(tweets, function(x) paste(x, collapse=" "))
txt <- unlist(tweets)



##Tokenize and clean the text

txt<-tokenize(txt, removePunct = TRUE, removeTwitter = FALSE )

##Convert to lower case

txt<-toLower(txt)

##Convert to Document Feature Matrix (AKA Document Term Matrix)

mat <-dfm(txt, stem=TRUE, language = "spanish", ignoredFeatures = stopwords(kind="spanish"))



##Run LDA 

##Set number of topics
k <-50


SEED<-2010

##Run the topic model
TM<-list(Gibbs = LDA(mat, k = k, method = "Gibbs",  control = list(seed = SEED, burnin = 300,thin = 30, iter = 300)))

##Store the results of the distribution of topics over documents
doc_topics<-TM[["Gibbs"]]@gamma


##Store the results of words over topics

words_topics<-TM[["Gibbs"]]@beta

###Look at a visualization of the topics

###transpose the data so that the days are columns
doc_topics<-t(doc_topics)

View(doc_topics)

#arrange the topic assignments from greatest to least

#find the max from each column
max<-apply(doc_topics, 1, which.max)

##write a function that finds the second max
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
  
}

##find the second max
max2<- apply(doc_topics, 1, which.max2)


##make an index of the days for the plot
dates<-seq(as.Date("2013/12/18"), by="days", length=162)

##divide the topic-days by coaltion
gov1<-max[1:162]
gov2<-max2[1:162]

opp1<-max[163:324]
opp2<-max2[163:324]

##make data frames
gov_topics<-data.frame(dates, gov1, gov2)
opp_topics<-data.frame(dates, opp1, opp2)

##Plot the topic topics over time

##government

z<-ggplot(gov_topics, aes(x=dates, y=gov1, pch="First")) 

z + geom_point(aes(x=dates, y=gov2, pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Government")  + xlab(NULL) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point()+ 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



##opposition

z<-ggplot(opp_topics, aes(x=dates, y=opp1, pch="First")) 

z + geom_point(aes(x=dates, y=opp2, pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Opposition")  + xlab(NULL) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point()+ 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 






###Now let's look at the words in each of these topics

### Rather than just looking at the highest-probability words, we'll look at the most specific words

### This code I got from Pablo, and we won't spend much time looking through it 

normalized.topics <- exp(TM[["Gibbs"]]@beta) / rowSums(exp(TM[["Gibbs"]]@beta))
calculate.specificity <- function(mod) {
  if(!inherits(mod,"LDA") & !inherits(mod,"CTM") ) stop("mod object must inherit from LDA or CTM")
  terms <- posterior(mod)$terms
  topics <- posterior(mod)$topics
  Nwords<-ncol(terms)
  Ntopics<-ncol(topics)
  Ndocs<-nrow(topics)
  ptopic <- apply(topics,2,sum)/Ndocs
  pwords <- apply(terms,2,function(x) sum(x*ptopic))
  numer <- terms*ptopic
  denom  <- matrix(pwords,nrow=Ntopics,ncol=Nwords,byrow=TRUE)
  return(numer/denom)
}
K<-k
normalized.words <- calculate.specificity(TM[["Gibbs"]])
normalized.words <- apply(exp(TM[["Gibbs"]]@beta), 2, function(x) x/sum(x))

scores <- apply(normalized.topics, 2, function(x) 
  x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) )
colnames(scores) <- TM[["Gibbs"]]@terms
words <- apply(scores, 1, function(x) 
  colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])
f.scores <- apply(scores, 1, function(x) 
  x[order(x, decreasing = TRUE)[1:num.words]])
n.topics <- rep(seq(1, K, 1), each=num.words)
order.topics <- rep(seq(1, num.words, 1), times=K)
info.df <- data.frame(
  topic = n.topics,
  word = c(words),
  order = as.character(order.topics),
  score = c(f.scores),
  stringsAsFactors=F)
info.df$order <- factor(info.df$order, levels=as.character(10:1))

info.df$specificity <- NA
for (i in 1:length(info.df$topic)){
  info.df$specificity[i] <- normalized.words[info.df$topic[i], which(colnames(scores) %in% info.df$word[i])]
}
info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))

topten<-vector("list", K)

for (i in 1:K){
  j<-10*(i-1)+1
  m<-10*i
  topten[[i]]<-cbind(info.df$word[j:m])
}


####Now let's look at a few of the topics of interest

gov1
topten[20]
##Seems like a reasonable list of words the government is gonna tweet about a lot

gov2

topten[21]
##A very specific attack related to a specific event

opp1
topten[37]
##Another reasonable list of opposition party words
topten[2]
###But this also seems reasonable, and we see too many stop words

opp2
topten[25]
##seems to switch to a more economics-focused discussion

topten[34]
##maybe we need to remove the twitter handles?


