
################################################################################
# This script is created for hands-on/demonstration sessions on Text Analytics #
#                                                                              #
#                   Author: Swagata Duari                                      #
#              Designation: Ph.D. Scholar                                      #
#                Institute: Dept. of Computer Science, University of Delhi     #
#   Github repository link: https://github.com/SDuari/Text-Analytics           #
#                                                                              #
# This script can be used for learning how to perform text analysis with R     #
################################################################################

###################################################################
# If libraries are not installed, install them using command:     #
#       > install.packages("package_name")  or GUI. This command  #
# is under the preinstalled util system library.                  #
###################################################################

##########
# PART 1 #
##########


### loading required libraries....Libraries will be loaded as and when necessary in this 
### tutorial. You can always load them together at the beginning.
library(tm)

# Create a corpus
# 1. First create some dummy documents for this experiment.
doc = c("This is a sample document.", "My second document is a dummy document.", "This is another example of random text.")
# 2. Check if doc is a data frame or coerce if possible.
df = as.data.frame(doc)     # this function is under base system library
# 3. Create the corpus (using VectorSource)
corp = Corpus(VectorSource(df$doc))


# Or create a corpus by reading from a txt file
f = "/Users/swagata/Documents/DATASETS/sample/2.txt" # provide the path to the file
texts<-readChar(f, file.info(f)$size) # read the file
doc2<-c(texts) # bind the characters together
corp2 = Corpus(VectorSource(doc2)) # create the corpus


# Third way to create a corpus from a directory (using DirSource)
filepath = ("/Users/swagata/Documents/DATASETS/sample")     # provide the path to your files
corp3 = Corpus(DirSource(filepath))

corp # metadata of corpus
corp[[1]] # metadata of document
inspect(corp) # metadata and details

corp2 # metadata of corpus
corp2[[1]] # metadata of document
inspect(corp2) # metadata and details

corp3 # metadata of corpus
corp3[[1]] # metadata of document
inspect(corp3) # metadata and details


#############################################################################
# Experiments on corpus
#############################################################################

# create document-term matrix
dtm = DocumentTermMatrix(corp)

# create dtm with a list of control options
dtm_weighted <- DocumentTermMatrix(corp, control = list(weighting = function(x) 
  weightTfIdf(x, normalize =TRUE),stopwords = TRUE)) 

# create term-document matrix
tdm = TermDocumentMatrix(corp)

# convert to matrix
dtm = as.matrix(dtm)
dtm
tdm = as.matrix(tdm)
tdm

dtm_weighted = as.matrix(dtm_weighted)
dtm_weighted

# term-term matrix
ttm = t(dtm) %*% dtm
ttm
# ttm = tdm %*% t(tdm)

# document-document matrix
ddm = dtm %*% t(dtm)
ddm
#ddm = t(tdm) %*% tdm




# tm_map(corpus, function) function is an interface to apply functions to the corpora 
# instead of individual documents. To apply functions to individual documents, use 
# function(doc,...).

#Tokenization
token1 = tm_map(corp, MC_tokenizer) # regard punctuations as a character
inspect(token1)

token2 = tm_map(corp, scan_tokenizer) # disregards punctuations
inspect(token2) 

# Cleaning of data in corpus
corp <- tm_map(corp, stripWhitespace)     # remove whitespaces
inspect(corp)

corp <- tm_map(corp, content_transformer(tolower))    # transforms the content to lowercase
inspect(corp)

corp <- tm_map(corp, removePunctuation)   # remove punctuations
inspect(corp)

corp <- tm_map(corp,removeNumbers)          # remove numbers from text.
inspect(corp)




###   POS TAGGING   ###
# load library
library(openNLP)
library(NLP)

# POS tagging: list of POS tags can be found at https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html


# POS tagging: need to define a function first

tagPOS <-  function(x) {   # user defined function to perform POS tagging
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator() 
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))  ###creates an annotation object
  a2 <- annotate(s, word_token_annotator, a2)  ### compute annotation for the given annotators
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"] 
  POStags <- unlist(lapply(a3w$features, `[[`, "POS")) ## extract from vector for named object
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")   ## formatting the result
  list(POStagged = POStagged, POStags = POStags)   ## returned output
}

###### illustration of tagPOS usage:
#str <- "this is a the first sentence."
#tagged_str <-  tagPOS(str)
#tagged_str

# tagging the corpus...
tagged_text <- tagPOS(corp[[1]])  #as single individual strings
tagged_text       #view output

# tagging whole corpus as a single string
tagged_text_all <-tagPOS(as.String(paste(doc, collapse = " ")))   
tagged_text_all       #view output


# Function to remove tags
RemoveTags <- function(Words) {
  sub("/[A-Z]{2,4}","",Words)
}

# Function to split text
SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}

tagged_words = unlist(strsplit(as.character(tagged_text$POStagged)," "))  # split the tagged string into separate terms with tags and bind them together as a list/array
#or split by calling the function as
# words_tag = SplitText(tagged_text$POStagged)
untagged_words = c(RemoveTags(tagged_words)) #removes associated tag from each term using RemoveTags function

# Function to select terms with specified tags, e.g., select only nouns
SelectTaggedWords <- function(Words,tagID) {
  Words[ grep(tagID,Words) ]    # grep command selects all words with a particular tagID
}

# Usage: select only nouns
tagged_words_nn <- c(SelectTaggedWords(tagged_words,"/NN"))  # keep only NN tagged words 
# or without defining a function
# tagged_words_nn <- c(words_tag[grep("/JJ", tagged_words)])
# To retrieve several words with different POS tags
# tagged_words_nn_jj <- c(SelectTaggedWords(tagged_words,"/NN"), SelectTaggedWords(tagged_words,"/JJ"), ...)


# stopwords removal
corp<-tm_map(corp,removeWords,stopwords(kind = "english"))  # using standard list in english
corp<-tm_map(corp,removeWords,stopwords(kind = "smart"))   # using "smart" list
# to add custom defined words in addition to the list provided, do
# corp<-tm_map(corp,removeWords,c(stopwords(kind = "english"), "word1", "word2", "word3"...))

#Stemming
corp <- tm_map(corp, stemDocument, language = "english")  # uses Porter's stemmer


#############################      END OF PART ONE       ###################################
#############################################################################################################################




##########
# PART 2 #
##########

# loading required libraries....
library(igraph)

file_name = "/Users/swagata/Documents/DATASETS/sample/2.txt"
texts<-readChar(file_name, file.info(file_name)$size)
texts
doc<-c(texts)
corp <- Corpus(VectorSource(doc))

corp <- tm_map(corp, stripWhitespace)
inspect(corp)

corp <- tm_map(corp, content_transformer(tolower))
inspect(corp)

corp <- tm_map(corp, removePunctuation)
inspect(corp)

corp<-tm_map(corp,removeNumbers)
inspect(corp)


#continuing from part one
tagged_text <- tagPOS(corp[[1]])  # POS tagging
tagged_text

tagged_words <- SplitText(as.character(tagged_text$POStagged))  #creates a vector
tagged_words

# to select only nouns and adjectives:
tagged_words <- c(SelectTaggedWords(tagged_words,"/NN"),SelectTaggedWords(tagged_words,"/JJ"))  # keep only NN & JJ tagged words 
tagged_words

# removes tags from the selected terms
tagged_words<- RemoveTags(tagged_words) # remove un-used POS tags
tagged_words    # to view the output

tagged_words <- as.String(tagged_words)
tagged_words <- gsub("\n"," ",tagged_words)
tagged_words <- c(tagged_words)
tagged_words

tw_corp <- Corpus(VectorSource(tagged_words))
tw_corp<-tm_map(tw_corp,removeWords,stopwords(kind = "english"))
corp<-tm_map(corp,removeWords,stopwords(kind = "english"))

words <- SplitText(as.character(corp[[1]]))
words
tagged_words<- SplitText(as.character(tw_corp[[1]]))

tagged_words      
selected_words <- unique(tagged_words)  
selected_words

# remove empty words from selected_words
selected_words = selected_words[-(which(selected_words == ""))]
selected_words


# GRAPH CONSTRUCTION
# load library
library(igraph)

# Function to check if the word is in the list of candidates (selected words)
IsSelectedWord <- function(Word) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}

# Identifies co-occurrences
GetWordLinks <- function(position,scope) {
  scope <- ifelse(position+scope > length(words), length(words), position+scope)
  links <- ""
  
  for (i in (position+1):scope) {
    if ( IsSelectedWord(words[i]) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}



# function for graph construction
ConstructGraph <- function(win_size, d) { 
  if (d == 1){
    word_graph <- make_empty_graph(n=0, directed=TRUE)    # creates an directed, empty graph
  }else{
    word_graph <- make_empty_graph(n=0, directed=FALSE)   # creates an undirected, empty graph
  }
  
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i]) ) {                                   
      links <- GetWordLinks(i, (win_size-1) )       # Call to function - gets the list of connected words                        
      if (links[1] != "") {                                     
        cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n") # for printing the links
        if ( length(which(V(word_graph)$name==words[i]))==0  ) {     # If vertex is not present in the graph, add it
          word_graph <- add.vertices(word_graph,1,name = words[i])
        }                                               
        for (j in 1:length(links)) {   # For all the words linked to this vertex
          if ( length(which(V(word_graph)$name==links[j]))==0 ) {    # if the linked vertex is not present in graph
            word_graph <- add.vertices(word_graph,1,name = links[j])   # Add it to the graph
            word_graph <- add.edges(word_graph, c(words[i],links[j]), attr = list(weight = 1))   # and add an edge between them with weight 1
          } 
          else {    # if the linked vertex is present
            if(!are.connected(word_graph, words[i],links[j])){     # if they are not already connected
              word_graph <- add.edges(word_graph, c(words[i],links[j]), attr = list(weight = 1))   # add an edge between them with weight 1
            }
            else{   # if they are already connected
              prev_weight = get.edge.attribute(word_graph, "weight", get.edge.ids(word_graph, c(words[i],links[j])))    # get previous edge weight
              word_graph <- set.edge.attribute(word_graph, "weight", get.edge.ids(word_graph, c(words[i],links[j])), as.numeric(prev_weight + 1))    # increment the edge weight by 1
            }
          } 
        }
      }
    }
    i <- i+1
  }
  word_graph
}




# ============================== DegExt and TEXTRANK=====================================
# Create DEGEXT graph with win_size = 2
win_size = 2

#call the function to create text graph
deg_graph <- ConstructGraph(win_size, 1)

# delete self-loops, if any
deg_graph <- simplify(deg_graph, remove.multiple = T, remove.loops = T)




# Create TEXTRANK graph
# Prompt the user to pass win_size
readinteger <- function()
{ 
  n <- readline(prompt="Enter window size[2-10]: ")
  return(as.integer(n))
}
win_size = readinteger()

#call the function to create text graph
text_graph <- ConstructGraph(win_size, 0)

# delete self-loops, if any
text_graph <- simplify(text_graph, remove.multiple = T, remove.loops = T)




# VISUALIZATION
#plot text graph
plot(text_graph)
plot(deg_graph)
# with edge weight plotted as width of the edges
tkplot.igraph(text_graph, edge.label = E(text_graph)$weight)
tkplot.igraph(deg_graph, edge.label = E(deg_graph)$weight)
# or use tkplot(g)


# DegExt ALGORITHM
nodes_deg_w = strength(deg_graph, mode = "all", loops = FALSE, weights = get.edge.attribute(deg_graph, name = "weight")) # strength calculates weighted degree

# Sort and select top 10 ranked words
key_deg <- nodes_deg_w[order(nodes_deg_w, decreasing = TRUE)]
key_deg
key_deg <- head(key_deg, 10)
key_deg


# PAGERANK ALGORITHM
nodes_rank = page.rank(text_graph, algo = "prpack", directed = FALSE)$vector


# Sort and select top 10 ranked words
key_TR <- nodes_rank[order(nodes_rank, decreasing = TRUE)]
key_TR
key_TR <- head(key_TR, 10)
key_TR


# WORD CLOUD
# load library
library(wordcloud)

dtm <- DocumentTermMatrix(corp)		# create document-term matrix for the corpus
m <- as.matrix(dtm)   #turns dtm into a matrix
v <- sort(colSums(m),decreasing=TRUE)   # sort terms based on frequency of occurrence
head(v,14)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=1)




#                           ********* END OF PART 1 AND 2 *********


