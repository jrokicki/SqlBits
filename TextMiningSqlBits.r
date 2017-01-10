
setwd('D:\\GitHub\\SqlBits\\')
source('.\\TextMiningSrc.r')
# source(gzcon(url('https://github.com/jrokicki/SqlBits/TextMiningSrc.r', 'rb')))

## prepare data
# load stopwords
stop.words <- read.csv('.\\Data\\stopwords.csv')
# load SqlBits 16 data
SqlBits16.df <- read.csv('.\\Data\\SqlBits16.csv', header = F, encoding = 'UTF8')
colnames(SqlBits16.df) <- c('Session', 'Author', 'Track', 'Level', 'Text')

## prepare objects
SqlBits16.corpus <- to.Corpus(SqlBits16.df$Text, stop.words)
SqlBits16.tdm <- to.TDM(SqlBits16.corpus)
SqlBits16.dtm <- as.DocumentTermMatrix(SqlBits16.tdm)
SqlBits16.wf <- to.WF(SqlBits16.tdm)

## words
inspect(SqlBits16.tdm[100:130, 10:30])
findFreqTerms(SqlBits16.tdm, 100)
head(SqlBits16.wf, n = 50)
bar.TDF(SqlBits16.wf)
cdf.TDF(SqlBits16.wf)
word1cloud.TDF(SqlBits16.wf)
word2cloud.TDF(SqlBits16.corpus)

tail(SqlBits16.wf, n = 50)
filter.DF(SqlBits16.df, SqlBits16.tdm, 'airbnb', c('Author', 'Session', 'Track', 'Level'))
filter.DF(SqlBits16.df, SqlBits16.tdm, 'spark', c('Author', 'Session', 'Track', 'Level'))
filter.DF(SqlBits16.df, SqlBits16.tdm, 'tensorflow', c('Author', 'Session', 'Track', 'Level'))
findFreqTerms(weightTfIdf(SqlBits16.tdm, normalize = TRUE), 2)
filter.DF(SqlBits16.df, SqlBits16.tdm, 'inmemori', c('Author', 'Session', 'Track', 'Level'))
filter.DF(SqlBits16.df, SqlBits16.tdm, 'machin', c('Author', 'Session', 'Track', 'Level'))
filter.DF(SqlBits16.df, SqlBits16.tdm, 'gdpr', c('Author', 'Session', 'Track', 'Level'))

hclust.TDM(SqlBits16.tdm, 4)
SqlBits16.tdm <- removeSparseTerms(SqlBits16.tdm, 0.85)
heatmap.M(SqlBits16.tdm)

## sessions
inspect(SqlBits16.dtm[40:50, 50:60])
SqlBits16.dtm <- removeSparseTerms(SqlBits16.dtm, 0.85)
heatmap.M(SqlBits16.dtm)
heatmap.M(SqlBits16.dtm, F)

sessions.Jan <- as.numeric(rownames(SqlBits16.df[SqlBits16.df$Author == 'Jan Rokicki',]))
m.tmp <- scale(as.matrix(SqlBits16.dtm))
m.tmp <- cos.sim(m.tmp, m.tmp)
heatmap(m.tmp[sessions.Jan,])
tail(sort(m.tmp[sessions.Jan[2],]))
head(sort(m.tmp[sessions.Jan[2],]))
SqlBits16.df[as.numeric(names(tail(sort(m.tmp[sessions.Jan[2],])))), c("Session", "Author", "Track")]
SqlBits16.df[as.numeric(names(head(sort(m.tmp[sessions.Jan[2],])))), c("Session", "Author", "Track")]

## topics
SqlBits16.lda <- to.LDA(SqlBits16.dtm, 8)
terms(SqlBits16.lda, 8)
posterior(SqlBits16.lda, SqlBits16.dtm)
topics(SqlBits16.lda, 1)
SqlBits16.df[which(as.matrix(topics(SqlBits16.lda, 1)) == 6), c('Author', 'Session', 'Track')]

## authors
SqlBits16.dt <- collapse.DT(SqlBits16.df, 'Author', 3)
tail(unique(SqlBits16.dt[, list(Author, N)]), n = 20)

ggplot(SqlBits16.dt, aes(Author)) + geom_bar() + 
scale_x_discrete(limits = unique(SqlBits16.dt[, Author])) + 
scale_y_continuous(breaks=c(3,4,5,6,7,8,9,10,11,12)) + coord_flip()
