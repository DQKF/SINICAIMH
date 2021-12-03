#R packages required for parsing: jiebaR, jiebaRD, tm, NLP
install.packages("jiebaRD")
install.packages("jiebaR")
install.packages("NLP")
install.packages("tm")
install.packages("tibble")

library(jiebaRD)
library(jiebaR)
library(NLP)
library(tm)
library(tibble)

setwd("/Library/Frameworks/R.framework/Versions/3.6/Resources/library/jiebaRD/dict")
cutter <- worker(type="mix", dict = "jieba.dict.utf8", stop_word = "AIMH_STP.txt", user = "AIMH_user.txt")
#cutter==分詞, dict=jiebaRD內建辭典，其餘兩項為作者自定義辭典。
##需檢查你的jiebaRD內建詞典是簡體中文還是繁體中文。


write.csv(R8_1, "R8_1.csv")
R8_1$text<-paste(R8_1$abstract,"uuu")
#在每個摘要後面貼上"uuu"，最為之後分割的記號。paste "uuu" as marks.
contentX8<-as.character(R8_1$text)
View(contentX8)
segtextX8 <- segment(contentX8, cutter)#進行分詞
segtextpasteX8 <- paste(segtextX8, collapse = " ")
setwd("/users/georgenee/Documents/AIHS/AIHS")#轉回原先的工作目錄
write(segtextpasteX8,"X8JP.txt")

#利用sublime text3 開啟X8JP.txt，開啟後用分行符號\n 取代uuu。
#在第一行鍵入"newc"，這是作為分詞後的column's name。
#sublime text3➡File➡save with encoding➡utf-8"
X8TTT <- read.table('X8JP.txt',sep = '\t',header = TRUE)#另存新檔
nrow(X8TTT)
nrow(R8_1)#兩者數目必須一致
View(test08)
R8_1<-cbind(R8_1, X8TTT)#將分詞後的結果與原先的檔案通過cbind函數貼在一起
View(R8_1)

#將"一個字"拿掉 Remove CN word with number of characters: ONE.
tr <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", R8_1$newc)
View(tr)
tr
tr<-tibble(tr)
write.csv(tr, "tr.csv")

#記得使用sublime text，進行空格處理
#Open "tr.cav" in sublime text3 to remove SOME whitespace.



#主題建模:找尋最佳的主題數目 
#search K to determine suitable number of topics
library(NLP)
library(stm)

processed <- textProcessor(R8_1$newc, R8_1,wordLengths = c(2,Inf))
out <-  prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
#NOT RUN.Lack of "date" item in this dataset.  
out$meta$speech_date<- as.numeric(as.Date(out$meta$speech_date))




set.seed(02139)
K<-c(5,10,15,20,25,30,35,40) 
#NOT RUN
results百年 <- searchK(docs, vocab, K, prevalence = ~term+s(occasionK), data=KFC21, max.em.its = 500)
results百年 <- searchK(docs, vocab, K, data=R8_1, max.em.its = 500)
plot(results百年)

set.seed(02139)
K<-c(5:25) 
results百年_1 <- searchK(docs, vocab, K, data=R8_1, max.em.its = 500)
plot(results百年_1)




#NOT RUN
stm.out_09 <- stm(out$documents, out$vocab, K=9, prevalence = ~ term+ s(occasionK), data=out$meta, init.type="Spectral")
#Run this Syntax
stm.out_09 <- stm(out$documents, out$vocab, K=9,  data=out$meta, init.type="Spectral")



install.packages("tibble")
library(tibble)
mdt09<-make.dt(stm.out_09, meta = NULL)
mdt09<-tibble(mdt09)
View(mdt09)
#remove 1st column of mdt9 in order to cal. colMeans
mdt09[1]<-NULL
colMeans(mdt09)
#tramsform into data.frame or tibble format
mdt09df<-as.data.frame(colMeans(mdt09))
View(mdt09df)
mdt09tibble<-tibble(colMeans(mdt09))
View(mdt09tibble)

#change the colname
colnames(mdt09)<-c("Topic01", "Topic02", "Topic03", "Topic04",
                   "Topic05", "Topic06", "Topic07", "Topic08", "Topic09")


#merge metadata & original text data into one. 
nrow(mdt09)
NR8_1<-cbind(R8_1, mdt09)
View(NR8_1)


#plotting keywords in each topic (in "frex"=stm)
plot.STM(stm.out_09, n=8,xlim = c(0, .3), labeltype="frex", family = "Songti SC")
#plotting keywords in each topic (in "score"=LDA) 
plot.STM(stm.out_09, n=8,xlim = c(0, .3), labeltype="score", family = "Songti SC")


#查看每一個主題內的四種關鍵詞排序方式
#keywords information in 4 types of Algorithm.
#For more info. about 4 types of Algorithm, 
##see p.13 at https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf

labelTopics(stm.out_09, topics = c(1), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(2), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(3), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(4), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(5), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(6), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(7), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(8), n = 20, frexweight = 0.5)
labelTopics(stm.out_09, topics = c(9), n = 20, frexweight = 0.5)

#which document holds the highest Expected Topic Proportion?

##Topic01
T01_top<-NR8_1[c("title", "abstract", "Topic01")]
T01_top<- filter(T01_top, Topic01 >= 0.5)
T01_top<-T01_top[rev(order(T01_top$Topic01)),]
View(T01_top)
##Topic09
T09_top<-NR8_1[c("title", "abstract", "Topic09")]
T09_top<- filter(T09_top, Topic09 >= 0.5)
T09_top<-T09_top[rev(order(T09_top$Topic09)),]
View(T09_top)


#找出關鍵詞 tf-idf
library(tm)
d.corpus <- Corpus(VectorSource(R8_1$newc))
d.corpus <- tm_map(d.corpus, removeNumbers)
tdm <- TermDocumentMatrix(d.corpus, control = list(weighting=weightTfIdf, wordLengths = c(4,Inf)))
freq<-rowSums(as.matrix(tdm))
tail(sort(freq), n=50)

library(ggplot2) 
hf<-tail(sort(freq), n=15)
hfdf<-as.data.frame(sort(hf))
hfdf$names<-rownames(hfdf)
ggplot(hfdf, aes(reorder(names, hf),hf))+ geom_bar(stat="identity", fill="steelblue")+coord_flip()+theme_minimal()+xlab("Terms")+ylab("Frequency")+ggtitle("TF-IDF Scores")+theme(text=element_text(size=16,  family="Songti TC"))


#找出關鍵詞 textrank
#APD-textrank
install.packages("textrank")
library(dplyr)
library(textrank)
library(tidyverse)
library(tidyr)
library(tidytext)
library(ggplot2)

id<-rep(1:nrow(R8_1))
la19aa<-cbind(id, R8_1)
la19aa_select <- la19aa%>% select(id,title, newc)
tok99 <- function(t) str_split(t,"[ ]{1,}")
View(la19aa_select)
la19aa_token <- la19aa_select %>% unnest_tokens(output='word', input='newc', token = 'regex')
set.seed = 0502
start = 1
end = 278#noticed!the number represents the numbers of document. 
textrank_modelAPD <- data.frame()
i = start
system.time(while (i <=end) {
  tryCatch({
    la19aa_textrank_item <- textrank_keywords(la19aa_token[(la19aa_token$id == i),]$word, ngram_max = 2)
    la19aa_textrank_total <- la19aa_textrank_item$keywords
    doc_num <- i
    output <- cbind(doc_num, la19aa_textrank_total)
    textrank_modelAPD <- rbind(textrank_modelAPD, output)
    print(i)}, error=function(e) return(NULL)) 
  i=i+1})

textrank_ungroupAPD <- textrank_keywords(la19aa_token$word, ngram_max = 2)
options(max.print=1000)
print(textrank_ungroupAPD)#可以叫出$pagerank$vector 
#了解哪些可能是不需要分離的詞彙 to know terms that does not need to be separated.
#了解哪些是可以捨棄的詞彙 to know terms can be listed in STW list.
#可以順便看一下詞頻分佈 check word-freq. distribution.

print(textrank_ungroupAPD$pagerank$vector)#可以直接叫出$pagerank$vector

high.freq=tail(sort(textrank_ungroupAPD$pagerank$vector),n=30)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df)
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +theme_bw()+ geom_bar(stat="identity", fill="#bfb911") + coord_flip() + theme_bw()+ggtitle("by Textrank,N=302")+theme(text=element_text(size=16,  family="黑體-繁 中黑"))+ylab("")+xlab("")+theme(plot.title = element_text(hjust = 0.5))

View(high.freq)
View(hfp.df)



#network analysis & community detection
#採用KWIC處理“百年未有之大变局”
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.corpora)
library(readtext)
tokss <- tokens(R8_1$newc, what="fastestword")
KWIC<-kwic(tokss, pattern = "百年未有之大变局", valuetype = "regex", window = 5)
View(KWIC)


R8_1$newc<-gsub("世界百年未有之大变局","百年未有之大变局",  R8_1$newc) #replace "from","to"
write.csv(R8_1, "R8_1.csv")


KWIC$prekeypost<-paste(KWIC$pre,KWIC$keyword, KWIC$post, sep=" ")
library(tibble)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(dplyr)
library(tidytext)
library(stringr)
library(widyr)
library(networkD3)
library(igraph)
library(glue)
library(magrittr)

KWIC<-tibble(KWIC)
View(KWIC)
addj2<-KWIC[c("docname","prekeypost")]
KWIC$ID<-seq.int(nrow(KWIC))




KWIC_select <- KWIC %>% select(ID,prekeypost)
View(KWIC_select)
tok99 <- function(t) str_split(t,"[ ]{1,}")
KWIC_token <- KWIC_select %>% unnest_tokens(output='word', input='prekeypost', token = tok99)
KWIC_token <- KWIC_token[nchar(KWICS_token$word) > 3, ]
View(KWIC_token)

edges_00<-KWIC_token %>% pairwise_count(word, ID, sort = TRUE)
colnames(edges_00) <- c("from", "to", "weight")
View(edges_00)
write.csv(edges_0,"edges_0.csv" )


#以下開始以edges_00開始製圖 makeing a Eng. graph by employing "edges_00"
edges_00 %>% head()
edges_00$weight<-as.integer(edges_00$weight)
View(edges_00)


edges_00 %>%
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

bi.gram.words %>%
  mutate(weight = log(weight + 1)) %>%
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram log-Weight Distribution")

ScaleWeight <- function(x, lambda) {
  x / lambda}
threshold <- 1
#這表示，只有3以上，2是排除的。


network <- edges_00 %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 8000)) %>% 
  graph_from_data_frame(directed = FALSE)
network
is.weighted(network)

set.seed(0124)
plot(
  network,
  edge.weight=2,
  vertex.size = 3,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  vertex.shape='circle',
  edge.color = '#e4fc9a', vertex.label.family="黑體-繁 中黑",
  vertex.label.font=7,edge.width=(E(network)$weight*700),
  main = ' pairwise_count network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 600
)

clusters(graph = network)
V(network)$cluster <- clusters(graph = network)$membership
network <- induced_subgraph(graph = network, 
                            vids = which(V(network)$cluster == 
                                           which.max(clusters(graph = network)$csize)))
network

V(network)$degree <- strength(graph = network)
E(network)$width <- E(network)$weight/max(E(network)$weight)
set.seed(05020)
plot( network, 
      vertex.color = 'lightblue', # Scale node size by degree.
      vertex.size = 10*V(network)$degree,
      vertex.label.color = 'black', 
      vertex.label.cex = 0.9, 
      vertex.label.dist = 2.0,
      edge.color = 'gray', # Set edge width proportional to the weight relative value.
      edge.width = 3*E(network)$width ,
      main = ' pairwise_count network', 
      sub = glue('Weiight Threshold: {threshold}'), 
      alpha = 50)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (150)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 15*E(network)$width
forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)
library(networkD3)


#-----New graph-------

# Compute the centrality measures for the biggest connected component from above.
node.impo.df <- tibble(
  word = V(network)$name,  
  degree = strength(graph = network),
  closeness = closeness(graph = network), 
  betweenness = betweenness(graph = network)
)

#Now we rank the nodes with respect to these centrality measures.
#Degree centrality
node.impo.df %>% 
  arrange(- degree) %>%
  head(20)

#Closeness centrality
node.impo.df %>% 
  arrange(- closeness) %>%
  head(20)

#Betweenness centrality
node.impo.df %>% 
  arrange(- betweenness) %>% 
  head(20)

#Let us see the distribution of these centrality measures.
library(ggplot2)
library(cowplot)
plt.deg <- node.impo.df %>% 
  ggplot(mapping = aes(x = degree)) +
  theme_light() +
  geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)

plt.clo <- node.impo.df %>% 
  ggplot(mapping = aes(x = closeness)) +
  theme_light() +
  geom_histogram(fill = 'red', alpha = 0.8, bins = 30)

plt.bet <- node.impo.df %>% 
  ggplot(mapping = aes(x = betweenness)) +
  theme_light() +
  geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)

plot_grid(
  ... = plt.deg, 
  plt.clo, 
  plt.bet, 
  ncol = 1, 
  align = 'v'
)


#We can try to find clusters within the network. 
#We use the Louvain Method for community detection:

comm.det.obj <- cluster_louvain(
  graph = network, 
  weights = E(network)$weight
)
comm.det.obj
View(comm.det.obj)


#再跑一次，需要調整degree, width可在此進行最終處理。
# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = 250*V(network)$degree)
# Define color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 15*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

#分群製圖（最後一步）
V(network)$membership <- membership(comm.det.obj)
# We use the membership label to color the nodes.
network.D3$nodes$Group <- V(network)$membership

set.seed(025345)
F41<-forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.8,
  Value = 'Width',
  Nodesize = 'Degree', 
  linkColour = "#abb0b3",
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  linkDistance = JS("function(d){return d.value * 50}"),
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
  fontSize = 16,
  zoom = TRUE, 
  opacityNoHover = 1, 
  radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"), charge = -500, bounded = FALSE, 
)
F41
networkD3::saveNetwork(F41, "EPIST_great_changes_unseen_in_a_century.html", selfcontained = TRUE)
webshot("Xi_great_changes_unseen_in_a_century.html","Xi_great_changes_unseen_in_a_century.html.png", vwidth = 1000, vheight = 1000)
library(webshot)

