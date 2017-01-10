
## packages
lapply(c('tm', 'slam', 'ggplot2', 'wordcloud', 'RWeka', 'Rgraphviz', 'topicmodels', 'data.table'), require, character.only = T)

## options
options(stringsAsFactors = F)

## functions
# build corpus and normalise docs
to.Corpus <- function(text, stop_words){
  if(require(tm)) {
    corpus <- Corpus(VectorSource(text))
    # transform
    corpus <- tm_map(corpus, content_transformer(removeNumbers))
    corpus <- tm_map(corpus, content_transformer(removePunctuation))
    corpus <- tm_map(corpus, content_transformer(stripWhitespace))
    corpus <- tm_map(corpus, content_transformer(tolower))
    # remove stopwords
    if(!is.null(stop_words)) {
      corpus <- tm_map(corpus, removeWords, stop_words[,'words'])
    } else {
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
    }
    # stem the words
    corpus <- tm_map(corpus, stemDocument)
  }
}

# compute TDM
to.TDM <- function(corpus, sparsity = 0.999){
  if(require(tm)) {
    tdm <- TermDocumentMatrix(corpus, control = list(stopwords = F))
    tdm <- removeSparseTerms(tdm, sparsity)
    tdm
  }
}

# compute word freq
to.WF <- function(tdm){
  if(require(tm) & require(slam)) {
    freq <- row_sums(tdm, na.rm = T)   
    freq <- sort(freq, decreasing = T)
    word.freq <- data.frame(word = factor(names(freq), levels = names(freq)), freq = freq)
    word.freq['cum_dis'] <- cumsum(word.freq['freq'])/sum(word.freq$freq)
    word.freq
  }
}

# plot frequencies
bar.TDF <- function(word.freq, maxwords = 50){
  if(require(ggplot2)) {
    ggplot(word.freq[1:maxwords,], 
           aes(word, freq)) +
           geom_bar(stat = 'identity') +
           ylab('Frequency') +
           theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}

# plot cumulative distribution frequency
cdf.TDF <- function(word.freq, maxwords = 50){
  if(require(ggplot2)) {
    ggplot(word.freq[1:maxwords,], 
           aes(word, cum_dis)) +
           geom_bar(stat = 'identity') +
           ylab('Cumulative frequency') +
           theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}

# calculate cosine similarity matrix
cos.sim <- function(ma, mb){
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}

# create 1-gram wordcloud
word1cloud.TDF <- function(word.freq, maxwords = 50){
  if(require(wordcloud)) {
    pal = brewer.pal(9,"BuPu")
    wordcloud(words = word.freq$word,
              freq = word.freq$freq,
              ##min.freq=60, 
              max.words = maxwords,
              scale = c(8,.3),
              random.order = F,
              colors = pal)
  }
}

# create bi-gram wordcloud
word2cloud.TDF <- function(corpus, maxwords = 50){
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
  if(require(RWeka)) {
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    tdm.ng <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
    tdm.ng <- removeSparseTerms(tdm.ng, 0.999)
    m = as.matrix(tdm.ng)
    v = sort(rowSums(m),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
    pal = brewer.pal(9,"BuPu")
    wordcloud(words = d$word,
              freq = d$freq,
              max.words = maxwords,
              scale = c(8, .3),
              random.order = F,
              colors = pal)
  }
}

# words dendogram
hclust.TDM <- function(tdm, k = 6){
  if(require(Rgraphviz)){
    m <- scale(as.matrix(tdm))
    m <- dist(m)
    fit <- hclust(m, method = "ward.D")
    plot(fit)
    rect.hclust(fit, k = k)
  }
}

# heatmap
heatmap.M <- function(df, a = T){
  if(require(Rgraphviz)){
    m <- scale(as.matrix(df))
    if(a) { 
      m <- cos.sim(m, m)
    }
    heatmap(m)
  }
}

# model topics
to.LDA <- function(dtm, maxtopics = 8){
  if(require(tm) & require(topicmodels)){
    lda <- LDA(dtm, k = maxtopics)
    lda
    # topics(lda, 1)
    # terms(lda, 6, 0.01)
    # posterior(lda, SqlBits16.dtm)
  }
}

# get rows containing normalised term
filter.DF <- function(df, tdm, term, cols){
  if(require(tm)) {
    idx <- which((as.matrix(tm_term_score(tdm, term)) > 0))
    df[idx,cols]
  }
}

# collapse data frame
collapse.DT <- function(df, keycol, minN = 0){
	if (require('data.table')) {
		dt <- data.table(df, key = keycol)
		dt[, N := .N, by = keycol]
		if (minN > 0) {
			dt <- dt[N >= minN,][order(N)]
		}
		dt
	}
}
