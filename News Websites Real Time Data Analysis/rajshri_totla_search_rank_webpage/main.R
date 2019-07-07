library(tidyverse)  # General-purpose data wrangling
library(stringr)   # String manipulation
library(htm2txt)
library(tm)
library(SnowballC)
library(RColorBrewer) # wordcloud 
library(wordcloud) # wordcloud 
library(stringi)
library(googleVis) # gvisBubbleChart function
library(stats)
library(stringdist)
# library(STAT)
library(cluster)
# par()              # view current settings
opar <- par()      # make a copy of current settings
data <- data.frame(
  url=c('https://www.wsj.com/','https://www.nbcnews.com/','https://www.usatoday.com/',
        'https://www.bbc.com/news','https://www.latimes.com/','https://www.huffpost.com/',
        'https://www.foxnews.com/','https://www.reuters.com/','https://news.google.com/?hl=en-US&gl=US&ceid=US:en',
        'https://news.yahoo.com/'),
  name=c('The Wall Street Journal','NBC News',"USA Today","BBC","LA Times","Huffington Post",
         "Fox News","Reuters","Google News","Yahoo News")
)

 # lexicon <-c("news","say","also","video","barr","morevert","edt","said","reuters","yahoo","view","wsj","bbc","can","will","said","-","cnn","says","said","have","images","get","amid","","can","via","top","fox","times","newsletters")
 # lexiconSpecialSymbols <- c("","."," . "," ."," -"," -","- "," - ","-","_"," _ ","_ "," _","","''s","'","'s")

# write(lexicon,file = "words.txt", sep = '\n')
# write(lexiconSpecialSymbols, file="specialSymobols.txt", sep = '\n')

lexicon <- as.character(scan('words.txt', what = 'character'))
lexiconSpecialSymbols <- as.character(scan('specialSymobols.txt', what = 'character'))



#_________________________________________________________________________________________________________
#_________________________________________________________________________________________________________
getTextFromURL <- function(url){
  text <- c()
  for(link in url){
    text <- c(text,gettxt(link))
  }
  text
}




#_________________________________________________________________________________________________________
#_________________________________________________________________________________________________________
# GenerateDendogram <- function(url, names){
  url <-data$url;names<-data$name
  # url <- as.vector(url)    
  url.text <- lapply(X = url,FUN=function(i)getTextFromURL(i))
  
  docs <- Corpus(VectorSource((url.text))) # Get Web Pages Corpus
  
  #_________________________________________________________________________________________________________
  # Text analysis - Preprocessing
  transform.words <- content_transformer(
    function(x,from,to)
      gsub(from,to,x)
  )

  data.corpus <- tm_map(docs, transform.words, "<.+?>", " ")
  data.corpus <- tm_map(data.corpus, transform.words, "\t", " ") # tabs
  data.corpus <- tm_map(data.corpus, transform.words, "\n", " ") # new line
  data.corpus <- tm_map(data.corpus, transform.words, ".", " ") # bulletpoints
  data.corpus <- tm_map(data.corpus, transform.words, "-", " ") # 
  data.corpus <- tm_map(data.corpus, transform.words, "<\"+?>", " ") # new line
  data.corpus <- tm_map(data.corpus, transform.words, "([[:digit:]]+)([[:alnum:]])*", " ") # number words
  # data.corpus <- tm_map(data.corpus, transform.words, "'", " ") # part of words
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords),stopwords("english"))
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords),lexicon)
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords),lexiconSpecialSymbols)
  # data.corpus <- tm_map(data.corpus, content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus, content_transformer(stripWhitespace))

  
  #_________________________________________________________________________________________________________
  # Create dtm
  dtm <- DocumentTermMatrix(data.corpus)

  # Frequent terms for each website
  #_________________________________________________________________________________________________________
  freq <- colSums(as.matrix(dtm)) # Term Frequencies
  ord <- order(freq, decreasing = TRUE) # Indices of frequencies order
  FFT <- freq[ord] # Ordered Freq Frequent terms
  
  # dtm <- removeSparseTerms(dtm,0.4)
  dtm$dimnames$Docs <- url
  
  #_________________________________________________________________________________________________________
  # FOR DENDOGRAM
  # Distance Measure
  docs <- dist( as.matrix(dtm), method = "euclidean")
  # Group Results
  d <- as.dist(docs)
  d[is.na(d)] <- 0
  d[is.nan(d)] <- 0
  sum(is.infinite(d))
  hclust_dist <- d 
  h <- hclust(hclust_dist, "ward.D2")
  par(mar=c(5.1,4.1, 4.1, 2.1))
  plot(h, labels = names, sub="", main="News Website Rank")
  
  
  # docs <- dist( as.matrix(dtm), method = "euclidean")
  # # Group Results
  # h <- hclust( as.dist(docs), "ward.D2")
  # par(mar=c(5.1,4.1, 4.1, 2.1))
  # plot(h, labels = names, sub="", main="News Website Rank")
  # 
  # library(cluster) #load similarity measures
  # d <- dist(as.matrix(dtm), method="binary") # find dist between terms
  # d[is.na(d)] <- 0
  # d[is.nan(d)] <- 0
  # sum(is.infinite(d))
  # cl <- hclust(as.dist(d)) #perform clustering
  # cl$labels <- names # assign labels to cluster leaves
  # plot(cl, main="News Website Rank")
  
  #wordcloud for top 75 words
  palette <- brewer.pal(8,"Dark2")
  set.seed(137)
  # For wordcloud title
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, "Top 75 words in all websites", col="darkblue", font = 8,cex = 2.7)
  # wordcloud
  wordcloud(words = names(FFT[1:250]),
            freq = FFT[1:250],
            random.order = F, 
            colors = palette,
            min.freq = 1,
            max.words = 75)
  
  #wordcloud for after top 5 words
  palette <- brewer.pal(8,"Dark2")
  set.seed(137)
  # For wordcloud title
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, "Top words in all websites excluding first 4", col="darkblue", font = 8,cex = 2.5)
  # wordcloud
  wordcloud(words = names(FFT[5:250]),
            freq = FFT[5:250],
            random.order = F, 
            colors = palette,
            min.freq = 1,
            max.words = 75)
 
  
# }
# GenerateDendogram(data$url,data$name)


#_________________________________________________________________________________________________________
#_________________________________________________________________________________________________________
generateWordCloud <- function(url, name){
  # url='https://news.yahoo.com/'; name="GN"
  getCorpus <- function(url){
    # text <- c()
    # for(link in url){
    #   text <- c(text,gettxt(link))
    # }
    text <- getTextFromURL(url)
    data.source <- VectorSource(text)
    data.corpus <- Corpus(data.source)
    return(data.corpus)
  }
  
  data.corpus <- getCorpus(url)
 
  # Text analysis - Preprocessing
  transform.words <- content_transformer(
    function(x,from,to)
      gsub(from,to,x)
  )
  
  getTransCorpus <- function(data.corpus){
    data.corpus <- tm_map(data.corpus, transform.words, "<.+?>", " ")
    data.corpus <- tm_map(data.corpus, transform.words, "\t", " ") # tabs
    data.corpus <- tm_map(data.corpus, transform.words, "\n", " ") # new line
    data.corpus <- tm_map(data.corpus, transform.words, ".", " ") # bulletpoints
    data.corpus <- tm_map(data.corpus, transform.words, "-", " ") # 
    data.corpus <- tm_map(data.corpus, transform.words, "<\"+?>", " ") # new line
    data.corpus <- tm_map(data.corpus, transform.words, "([[:digit:]]+)([[:alnum:]])*", " ") # number words
    data.corpus <- tm_map(data.corpus, transform.words, "'", " ") # part of words
    data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
    data.corpus <- tm_map(data.corpus, content_transformer(tolower))
    data.corpus <- tm_map(data.corpus, content_transformer(removeWords),stopwords("english"))
    data.corpus <- tm_map(data.corpus, content_transformer(removeWords),lexicon)
    data.corpus <- tm_map(data.corpus, content_transformer(removeWords),lexiconSpecialSymbols)
    # data.corpus <- tm_map(data.corpus, content_transformer(stemDocument))
    data.corpus <- tm_map(data.corpus, content_transformer(stripWhitespace))
    return(data.corpus)
  }
  
  data.Trans.corpus <- getTransCorpus(data.corpus)
  
  # Create the document-term matrix
  #_________________________________________________________________________________________________________
  dtm <- DocumentTermMatrix(data.Trans.corpus)
  dtm <- as.matrix(dtm)
  
  # Frequent terms for each website
  #_________________________________________________________________________________________________________
  freq <- colSums(dtm) # Term Frequencies
  ord <- order(freq, decreasing = TRUE) # Indices of frequencies order
  FFT <- freq[ord] # Ordered Freq Frequent terms

  # Word cloud
  #_________________________________________________________________________________________________________
  
  palette <- brewer.pal(8,"Dark2")
  set.seed(173)
  # FFT #Display words in console
  # For wordcloud title
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, toupper(name), col="darkblue", font = 8,cex = 2.7)
  # wordcloud
  wordcloud(words = names(FFT[c(1:300)] ),
            freq = FFT[c(1:300)],
            random.order = T, 
            colors = palette,
            min.freq = 1,
            max.words = 75)# 75 # Required as per assignment

  # barplot
  par(opar)
  par(mar=c(5.1,4.1, 4.1, 2.1))
  barplot(height=FFT[c(1:15)],width=1, main=name, xlab=names(FFT[c(1:15)]), col = palette)
  abline(h=0)
 
  # creating dataframe for bubble plot
  FFT3 <- data.frame(wordName=names(FFT[1:3]),frequency=FFT[1:3])
  FFT3 #return
}

# apply(X = data,FUN=generateWordCloud(data$url,data$name))

FFT <- data.frame()
FFT <- generateWordCloud('https://www.wsj.com/', "The Wall Street Journal")
FFT <- rbind(FFT,generateWordCloud('https://www.nbcnews.com/', "NBC News"))
FFT <- rbind(FFT,generateWordCloud('https://www.usatoday.com/', "USA Today"))
FFT <- rbind(FFT,generateWordCloud('https://www.bbc.com/news', "BBC"))
FFT <- rbind(FFT,generateWordCloud('https://www.latimes.com/', "LA Times"))
FFT <- rbind(FFT,generateWordCloud('https://www.huffpost.com/', "Huffington Post"))
FFT <- rbind(FFT,generateWordCloud('https://www.foxnews.com/', "Fox News"))
FFT <- rbind(FFT,generateWordCloud('https://www.reuters.com/',"Reuters"))
FFT <- rbind(FFT,generateWordCloud('https://news.google.com/?hl=en-US&gl=US&ceid=US:en',"Google News"))
FFT <- rbind(FFT,generateWordCloud('https://news.yahoo.com/',"Yahoo News"))
FFT$id <- rep(1:10, each=3) 
# FFT$websiteName <- rep(data$name[1:2], each=3)

Bubble <- gvisBubbleChart(FFT, idvar="wordName",
                          xvar="id", yvar="frequency",
                          sizevar = "frequency",
                          colorvar = "wordName",
                          options=list(
                            hAxis='{minValue:1, maxValue:11, title:"Websites"}',
                            vAxis='{minValue:0, maxValue:100, title:"Frequency"}')
                          )

plot(Bubble)

