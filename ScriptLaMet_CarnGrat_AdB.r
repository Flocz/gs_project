library(googlesheets)
library(psych)
library(igraph)
# install.packages("tm")  # pour le text mining
# install.packages("SnowballC") # pour le text stemming
# install.packages("wordcloud") # générateur de word-cloud 
# install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("magrittr")

(my_sheets <- gs_ls())
# my_sheets %>% glimpse()
gs_ls("")[1]
gs_ls("Carnet de Gratitude")

CarnGrat <- gs_title("Carnet de Gratitude")
CarnGrat
CarnGrat %>% gs_browse()
CarnGrat
gs_ws_ls(CarnGrat)

CarnGrat2 <- CarnGrat %>% gs_read(ws = "Réponses au formulaire 1")
CarnGrat2
str(CarnGrat2)
glimpse(CarnGrat2)
CarnGrat2 <- gs_read(ws=1)

dim(CarnGrat2)
class(CarnGrat2)

CarnGratDF <- data.frame(CarnGrat2)


class(CarnGratDF)
dim(CarnGratDF)

CarnGratDF_AdB <- CarnGratDF[CarnGratDF$ID=="AdB",]


CarnGratDF_AdB$Horodateur2 <- as.Date(CarnGratDF_AdB$Horodateur, "%d/%m/%y")


# etat de bien être

psych::describe(CarnGratDF_AdB$Etat.de.bien.être)

plot(CarnGratDF_AdB$Etat.de.bien.être ~ CarnGratDF_AdB$Horodateur2, ylim=c(0,10))
abline(a = NULL, b = NULL, h = 5, v = NULL, col="black")
abline(lm(CarnGratDF_AdB$Etat.de.bien.être ~ CarnGratDF_AdB$Horodateur2), col="red")
# lo <- loess(CarnGratDF_AdB$Etat.de.bien.être ~ CarnGratDF_AdB$Horodateur2)
# lines(predict(lo), col='green', lwd=2)
       
       
boxplot(CarnGratDF_AdB$Etat.de.bien.être, ylim=c(0,10))
points(mean(CarnGratDF_AdB$Etat.de.bien.être), col = "red", pch = 1)
abline(a = NULL, b = NULL, h = 5, v = NULL, col="black")

####

x <- c(CarnGratDF_AdB[,4],CarnGratDF_AdB[,5],CarnGratDF_AdB[,6])
x2 <- Corpus(VectorSource(x))

inspect(x2)

#### wordclouds

# http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
x2 <- tm_map(x2, toSpace, "/")
x2 <- tm_map(x2, toSpace, "@")
x2 <- tm_map(x2, toSpace, "\\|")
x2 <- tm_map(x2, toSpace, "'")


# Convertir le texte en minuscule
x2 <- tm_map(x2, content_transformer(tolower))
# Supprimer les nombres
x2 <- tm_map(x2, removeNumbers)
# Supprimer les mots vides français
# x2 <- tm_map(x2, removeWords, stopwords("french"))
# # Supprimer votre propre liste de mots non désirés
# x2 <- tm_map(x2, removeWords, c("blabla1", "blabla2")) 
# Supprimer les ponctuations
x2 <- tm_map(x2, removePunctuation)
# Supprimer les espaces vides supplémentaires
x2 <- tm_map(x2, stripWhitespace)
# Text stemming
# x2 <- tm_map(x2, stemDocument)

inspect(x2)


# toElse <- content_transformer(function (x, y, pattern ) gsub(pattern, y, x))


estoE <- content_transformer(function (x) gsub("[é,è,ê,ë]", "e", x))
astoA <- content_transformer(function (x) gsub("[â,à,á,ä]", "a", x))
istoI <- content_transformer(function (x) gsub("[î,í,ì,ï]", "i", x))
ustoU <- content_transformer(function (x) gsub("[û,ú,ù,ü]", "u", x))
ostoO <- content_transformer(function (x) gsub("[ô,ó,ò,ö]", "o", x))

x2 <- tm_map(x2, estoE)
x2 <- tm_map(x2, istoI)
x2 <- tm_map(x2, ostoO)
x2 <- tm_map(x2, ustoU)
x2 <- tm_map(x2, astoA)

inspect(x2)
       



#### avec les stopwiords french

dtm <- TermDocumentMatrix(x2)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale=c(5.5,0.25))


#### SANS les stopwords french

# Supprimer les mots vides français
x3 <- tm_map(x2, removeWords, stopwords("french"))

dtm <- TermDocumentMatrix(x3)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale=c(2.5,0.25))


####### 



findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "bonne", corlimit = 0.3)

# barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
#         col ="lightblue", main ="Most frequent words",
#         ylab = "Word frequencies")

findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 1), corlimit = 0)

findFreqTerms(dtm, lowfreq = 1)


# findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 4), corlimit = 0.3)
# findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 1), corlimit = 0)
# 
# 
# (cor_1 <- findAssocs(dtm, colnames(dtm)[1:2], 0))
# 

n.entries <- nrow(df)/3

df <- data.frame(t(as.matrix(dtm)))
df$VI_JOUR <- rep(1:n.entries,3)
df$VI_MOTIF <- c(rep("M1",n.entries),rep("M2",n.entries),rep("M3",n.entries))

df_long <- melt(data=df, 
id.vars=c("soir","television","fondue","papa",
"saletta","derby","servettels","chamois",
"rencontre","marche","montagne","pere",
"faites","lessive","paperasse","corseaux",
"dehors","natation","edita","retour",
"vacances","bonne","meditation","chez",
"mere","repas","agreable","grangettes",
"famille","copine","auto","route",
"nicolas","course","minutes","pied",
"reussi","joux","lac","maman",
"tour","enrichissante","matin","menage",
"apero","carnet","gratitude","bonnes",
"resolutions","soiree","reprise","travail",
"paccotssaletta","bain","murdoch","tele",
"avant","avion","messages","prenne",
"ballade","montreux","lectures","achat",
"anniversaire","cadeau","cote","gestion",
"sexuel","reveil","amis","foot",
"match","VI_JOUR","VI_MOTIF")
                  ,
                    measure.vars=c("soir","television","fondue","papa",
                                   "saletta","derby","servettels","chamois",
                                   "rencontre","marche","montagne","pere",
                                   "faites","lessive","paperasse","corseaux",
                                   "dehors","natation","edita","retour",
                                   "vacances","bonne","meditation","chez",
                                   "mere","repas","agreable","grangettes",
                                   "famille","copine","auto","route",
                                   "nicolas","course","minutes","pied",
                                   "reussi","joux","lac","maman",
                                   "tour","enrichissante","matin","menage",
                                   "apero","carnet","gratitude","bonnes",
                                   "resolutions","soiree","reprise","travail",
                                   "paccotssaletta","bain","murdoch","tele",
                                   "avant","avion","messages","prenne",
                                   "ballade","montreux","lectures","achat",
                                   "anniversaire","cadeau","cote","gestion",
                                   "sexuel","reveil","amis","foot",
                                   "match"),
                    variable.name = "variable")

df_long

Rmisc::summarySEwithin(df_long, df_long$value, df_long$VI_MOTIF)

tapply(df_long$value, c(df_long$variable), sum)


#####

# https://www.r-bloggers.com/an-example-of-social-network-analysis-with-r-using-package-igraph/

psych::describe(df_long$value)

colnames(df)

colnames(df)
nrow(df[,1:76])

K <- as.matrix(t(df[,1:76]),nrow(t(df[,1:76])),ncol(t(df[,1:76])))
K
L <- K %*% t(K)
L

# colnames(L) <- colnames(df)[,1:76]


# library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(L, weighted=TRUE, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Plot a Graph
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .9)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(0, .5, .6, egam)
E(g)$width <- egam*5

V(g)$color <- rgb(0, .5, .6, egam)

# plot the graph in layout1

# jpeg("Plot3.jpeg", width = 4, height = 4, units = 'in', res = 800)
plot(g, layout=layout.kamada.kawai, res=800, cex=3)
dev.off()

####

# paste("M1",,sep="_")
# paste("M2",rep(1:14,1),sep="_")
# paste("M3",rep(1:14,1),sep="_")
 
groupe <- rep(1:14,3)

qgraph::qgraph(cor_2, minimum=0.25,cut=0.4,group=,legend=TRUE,borders=FALSE)

