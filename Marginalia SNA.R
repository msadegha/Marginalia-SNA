# This file contains the code to create a semantic network to analyze the marginalia of al-Urmawi's manuscripts on the science of music 

text = readLines("~/Desktop/BNF 4867_farasa_stemmed_parsed.txt")
length(text)

text = subset(text, text != "")
text
grepl('[0-9]+', text)



text = subset(text, !grepl('[0-9]+', text) )
text

length(text)

text = gsub('[[:punct:] ]+',' ', text)
text

wordstoppers = readLines("~/Desktop/Arabic_wordstoppers.txt")
wordstoppers

tokenized_paragraphs = strsplit(text, " ")
tokenized_paragraphs = lapply(tokenized_paragraphs, FUN = function(x) subset(x, !x %in% wordstoppers))
tokenized_paragraphs

#wordcloud is the final goal for now

#counting

#identify indeces longer than one letter

vectored_paragraphs = unlist (tokenized_paragraphs)

letterized_words = strsplit(vectored_paragraphs, "")

word_lengths = lapply(letterized_words, length)

good_list = unlist(word_lengths) > 1

vectored_paragraphs = vectored_paragraphs [good_list]

vectored_paragraphs

#doing the actual counting, using a function called table

counting_words = table(vectored_paragraphs)  
counting_words

#sorting the table

counting_words_sorted = sort(counting_words, decreasing = TRUE)

#putting them into an easily legible table

View (counting_words_sorted)

#write to a file, as in, export it

#write.csv(counting_words_sorted, "~/Desktop/Sadegh_Tutorials/BNF_word_count.csv")


#now with making the wordcloud

#install.packages("wordcloud2")
library(wordcloud2)

words = names(counting_words_sorted)
frequencies = counting_words_sorted
df = data.frame( word = words, freq = as.numeric(frequencies), stringsAsFactors = F)

wordcloud2(df, fontWeight = "normal")

# Co-occurences 

#install.packages("tidytext")


library(tidytext)
library(dplyr)
library(janeaustenr)

bnf_comments = data.frame(text = text, book = "BNF", stringsAsFactors = F)
View(bnf_comments)
bnf_comments

#Let's start with marginalia level co-occurences, the following is just a recap of what we had already done.

all_words = names(counting_words)
all_words

marginalia_3 = table(tokenized_paragraphs[[3]])
marginalia_3

#the problem is it is only counting the words it sees, but doesn't track the words that exist in our entire corpus but are absent in marginalia 3

#this pulls out the contents of the marginalia 3

marginalia3_content = tokenized_paragraphs[[3]]
marginalia3_content_factor = factor(marginalia3_content, levels = all_words)

table(marginalia3_content_factor)

#now this above line is representing all the words, with those that are not in marginalia 3 with 0 counts

# now we are going to write a function that will iterate through our entire marginalia corpus
full_text_tabulation = function(marginalia, all_possible_words){
  marginalia_factor = factor(marginalia, levels = all_possible_words) 
  marginalia_tabulated = table(marginalia_factor)
  return(marginalia_tabulated)
}

#Now we will test it on marginalia 6 and count the total words in it
marginalia6_tabulated = full_text_tabulation(tokenized_paragraphs[[6]], all_words)
sum(marginalia6_tabulated)

#now we apply this to the entire corpus using a lapply function

bnf_tabulated_marginalia = lapply(tokenized_paragraphs, FUN = function(x) full_text_tabulation(x, all_words))

bnf_tabulated_marginalia

#We conceptually have a matrix now. But we have to make it look like what R knows as a matrix

bnf_doc2term_matrix = do.call("rbind", bnf_tabulated_marginalia)

#We are using a function called do.call, which itself calls a function and then iterates a list through it. Here, the second function called was rbind which binds rows one after the other into a larger matrix that is legible by R

dim(bnf_doc2term_matrix)

#This line gave us the deminsions of the matrix, which shoud be 114 (the number of marginalia) by 713 (total number of words)

#now if we multiply a doc2term matrix by the transpose of itself, we get a co-occurence matrix. Basically we are multiplying the columns onto themselves.

bnf_doc2term_matrix_transposed = t(bnf_doc2term_matrix)

bnf_co_occurence_marginalia = bnf_doc2term_matrix %*% bnf_doc2term_matrix_transposed

dim(bnf_co_occurence_marginalia)

bnf_co_occurence_words = bnf_doc2term_matrix_transposed %*% bnf_doc2term_matrix

dim(bnf_co_occurence_words)

#The first matrix tells us which documents share words (in this case the marginalia). The second matrix tells us which words co-occur within the said documents.

#We want to set the diagonal of the matrix to 0,because it only gives us the number of times a word has appeared in the text, which is useless

diag(bnf_co_occurence_words) = 0
diag(bnf_co_occurence_marginalia) = 0

#now let's graph it as a network

#install.packages("igraph")

library(igraph)

# first we are going to graph the matrix (i.e. turn it into an igraph object) using the graph.adjacency function which graphs square matrices.

bnf_net = graph.adjacency(bnf_co_occurence_words, mode = "undirected", weighted = T)

#install.packages("rgexf")

# Converts the given igraph object to GEXF format and saves it at the given filepath location

saveAsGEXF = function(g, filepath="converted_graph.gexf", author = "Mohammad Sadegh Ansari")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(igraph::V(g)$label)){
    igraph::V(g)$label <- as.character(igraph::V(g))}
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(igraph::E(g)$weight)){
    igraph::E(g)$weight <- rep.int(1, igraph::ecount(g))}
  
  nodes_df <- data.frame(cbind(igraph::V(g), igraph::V(g)$label))
  edges_df <-  igraph::ends(g, 1:ecount(g))
  
  
  
  #edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodes_Att <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edges_Att <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # print the gexf object to the file
  rgexf::write.gexf(nodes = nodes_df, 
                    edges = edges_df,
                    nodesAtt = nodes_Att,
                    edgesWeight=E(g)$weight,
                    edgesAtt = edges_Att,
                    defaultedgetype = "undirected",
                    meta=c(list(creator = author, 
                                description="igraph -> gexf converted file", 
                                keywords="igraph, gexf, R, rgexf"), 
                           graphAtt),
                    output=filepath)
}

saveAsGEXF(bnf_net, "bnf_net.gexf")


