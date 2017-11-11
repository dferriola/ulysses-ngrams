library(ggplot2)
library(data.table)
library(tm)
library(dplyr)
library(tidytext)
library(gutenbergr)
library(RWeka)

df=scan("https://www.gutenberg.org/files/4300/4300-0.txt", what="charecter", sep="\n")
df
df[1]
start.df <- which(df == "— I —") 
end.df <- which(df == "1914-1921")
start.df
end.df
length(df)
start.metadata.v <- df[1:start.df -1] 
end.metadata.v <- df[(end.df+1):length(df)] 
metadata.v <- c(start.metadata.v, end.metadata.v) 
novel.lines.v <- df[start.df:end.df]
novel.v=paste(novel.lines.v,collapse = " ")
novel.lower.v=tolower(novel.v)
ulysses.words.1=strsplit(novel.lower.v, "\\W")
class(novel.lower.v)
class(ulysses.words.1)
str(ulysses.words.1)
ulysses.words.v=unlist(ulysses.words.1)
not.blanks.v=which(ulysses.words.v!="")
not.blanks.v
ulysses.words.v=ulysses.words.v[not.blanks.v]
ulysses.words.v
ulysses.words.v[1:10]
which(ulysses.words.v=="fear")
ulysses.words.v[which(ulysses.words.v=="fear")]
length(ulysses.words.v[which(ulysses.words.v=="fear")])
ulysses.freqs.t=table(ulysses.words.v)
sorted.ulysses.freqs.t=sort(ulysses.freqs.t, decreasing = T)
sorted.ulysses.freqs.t
ul_corpus=Corpus(VectorSource(ulysses.words.v))
clean_ul_corpus=tm_map(ul_corpus,removeWords,stopwords())
myStopwords <- c('youre','dont','like','theyre','theres', 'thats','whats','let','two','youve','ill','didnt','just','cant','youll','isnt','get','got','can','ive', 'mo','o','s','t', 'm')
clean_ul_corpus <- tm_map(clean_ul_corpus, removeWords, myStopwords)

clean_ul_corpus
Onegram <- NGramTokenizer(clean_ul_corpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
Bigram <- NGramTokenizer(clean_ul_corpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
Trigram <- NGramTokenizer(clean_ul_corpus, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))

Tab_onegram <- data.frame(table(Onegram))
Tab_bigram <- data.frame(table(Bigram))
Tab_trigram <- data.frame(table(Trigram))

OnegramGrp <- Tab_onegram[order(Tab_onegram$Freq,decreasing = TRUE),]
BigramGrp <- Tab_bigram[order(Tab_bigram$Freq,decreasing = TRUE),]
TrigramGrp <- Tab_trigram[order(Tab_trigram$Freq,decreasing = TRUE),]

OneSamp <- OnegramGrp[1:35,]
colnames(OneSamp) <- c("Word","Frequency")
BiSamp <- BigramGrp[1:35,]
colnames(BiSamp) <- c("Word","Frequency")
TriSamp <- TrigramGrp[1:35,]
colnames(TriSamp) <- c("Word","Frequency")

ggplot(OneSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Most Frequent 1-gram in James Joyces "Ulysses"')

library(openNLPmodels.en)
library(cleanNLP)
library(ggthemes)
ggplot(BiSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Most Frequent 2-gram in James Joyces "Ulysses"')
ggplot(TriSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Most Frequent 3-gram in James Joyces "Ulysses"')
init_tokenizers()
ul=run_annotators(novel.lines.v, as_strings = T)
get_token(ul)