
library(readr)
library(NLP)
library(tm)
library(SnowballC)

#To pull the required data tables
Reviews_export <- read.csv(file="Reviews_export.csv", header=TRUE, sep=",") #restaurent reviews
Pos_Words <- read.csv(file="Pos_Words.csv", header=TRUE, sep=",")      #Postive words
Neg_Words <- read.csv(file="Neg_Words.csv", header=TRUE, sep=",")      #Negative words
Res_categories <- read.csv(file="Res_categories.csv", header=TRUE, sep=",") #restaurent data scraped with the categories

#Storing it in daa_input for further processing
data_input=as.data.frame(Reviews_export)


#cleaning data to get correct words
Data=gsub("-","",data_input$Review)
Data=gsub("\\?",",",Data)
Data=gsub("\\!",",",Data)
Data=gsub("\\*",",",Data)
Data=gsub("[\r\n]", ",",Data)
Data=gsub("\\(", ",",Data)
Data=gsub(")", ",",Data)
Data=gsub("\\{", ",",Data)
Data=gsub("}", ",",Data)
Data=gsub("but ", ",",Data)
Data=gsub("-", "",Data)
Data=gsub("[^[:alnum:] ]", "", Data)
Data=gsub("comma", ",",Data) #Since the ',' in the input CSV file is converted to 'comma' for importing purpose

#Converting to dataframe
Data=as.data.frame(Data)

#to convert Data to lower case
Data=sapply(Data,tolower)

# defining my stopwords
exceptions   <- c("not","never","no","don't","but")
my_stopwords <- setdiff(stopwords("en"), exceptions)

#Cleaning data
corpus <- VCorpus(VectorSource(Data))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords,my_stopwords)
corpus <- tm_map(corpus,stemDocument)

#Create most frequent Bi grams
library(tm)
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(corpus,control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

#Create most frequent tri grams
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram = TermDocumentMatrix(corpus,control = list(tokenize = TrigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.trigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

#To create the stemmed data for reviews
k=1
data_input$stemmedtext='-'
for(k  in 1:nrow(data_input))
  data_input$stemmedtext[k]=paste("",as.character(corpus[[k]]),"",sep = "")

#To create document term matrix from the corpus
dtm <- DocumentTermMatrix(VCorpus(VectorSource(corpus)))

#Remove sparse terms
Sparse_Matrix= removeSparseTerms(dtm,0.99)

#To find the topics for related words
library(topicmodels)
topic_matrix= LDA(Sparse_Matrix,k=10)
lda= posterior(topic_matrix,Sparse_Matrix)
lda$topics

library(tidytext)
ap_topics <- tidy(topic_matrix, matrix = "beta")
ap_topics

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#To view the topics and the most frequent words
library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Creating a data frame from sparse matrix
Reviews_Input_dtm=as.data.frame(as.matrix(Sparse_Matrix))

#as.matrix(dtm)
Words_Frequency <- colSums(Reviews_Input_dtm)
Words_Frequency <- sort(Words_Frequency, decreasing = TRUE)

#To create a word cloud for most frequent terms
library(RColorBrewer)
library(wordcloud)
words <- names(Words_Frequency)

#To view the word cloud of the most frequent words
wordcloud(words[1:30],Words_Frequency[1:30])

Words_Frequency=as.data.frame(Words_Frequency)
Words_Frequency$Word=row.names(Words_Frequency)
rownames(Words_Frequency) =seq.int(nrow(Words_Frequency))
Words_Frequency$No.of.Reviews=0
Data=Reviews_Input_dtm=NULL

#to find the no. of reviews in which a particula word id repeated
j=1
for(j in 1:100)
{
  x= paste("",Words_Frequency$Word[j],"",sep = "")
  temp=subset(data_input,grepl(x,data_input$stemmedtext)==TRUE)
  Words_Frequency$No.of.Reviews[j]=length(temp$stemmedtext)       
  }     

#to find how many times are the words repeating in a review
Words_Frequency$Multiple_times=Words_Frequency$Words_Frequency/Words_Frequency$No.of.Reviews
hist(Words_Frequency$Multiple_times)

temp=NULL

#########Code to filet at restaurent level and filter the restaurent data

#Creating restaurent dataframe in which restaurent level data is stored
restaurent=as.data.frame(unique(data_input$Res_ID))
colnames(restaurent)[1]='Name'
j=1
restaurent$Food='-'
restaurent$Service='-'
restaurent$place='-'
restaurent$time='-'
restaurent$price='-'

#loop running at restaurent level
for(res_count in 1:length(restaurent$Name)) #loops runs for all restaurent for which we have reviews
{
Reviews_res=data_input[data_input$Res_ID==unique(data_input$Res_ID)[res_count],] # to filter the reviews for the only one restaurent

#to break the review at sentence level and find the no of sentences and sentiment of sentence

#To break the reviews to sentence level, replaing all possible conjunctions to ','
Data=NULL
Data=gsub("\\.",",",Reviews_res$Review)
Data=gsub("\\?",",",Data)
Data=gsub("\\!",",",Data)
Data=gsub("\\*",",",Data)
Data=gsub("[\r\n]", ",",Data)
Data=gsub("\\(", ",",Data)
Data=gsub(")", ",",Data)
Data=gsub("\\{", ",",Data)
Data=gsub("}", ",",Data)
Data=gsub("but ", ",",Data)
Data=gsub("-", "",Data)
Data=gsub("comma_", ",",Data)
Data=gsub("_", " ",Data)
Data=as.data.frame(Data)
Data$Review_ID <- seq.int(nrow(Data))

library(tidyr)
library(dplyr)

#To break each review to sentence level
y=Data %>% 
  mutate(Data=strsplit(as.character(Data), ",")) %>% 
  unnest(Data)

#Cleaning Sentence level data
Sentence_data=y[(y$Data!=""),]
Sentence_data=Sentence_data[Sentence_data$Data!=" ",]
Sentence_data$Length=nchar(Sentence_data$Data)
Sentence_data=subset.data.frame(Sentence_data,Sentence_data$Length>3)
Sentence_data=Sentence_data[is.na(Sentence_data$Review_ID)==FALSE,]
Sentence_data$Data=gsub("[\r\n]", "", Sentence_data$Data)

#removing unneccesary data
y=Data=temp=NULL

#stemming the sentence data to removes the tenses
Sentence_data$stemmed_text='-'
for( i in 1:length(Sentence_data$Data))
{
Sentence_data$stemmed_text[i]=stemDocument(Sentence_data$Data[i])  
}

#stemming the positive and negative words to remove any possible tenses
for(i in 1:length(Pos_Words$Words))
{
Pos_Words$stemmed[i]=stemDocument(Pos_Words$Words[i])  
}

Neg_Words$stemmed='-'
for(i in 1:length(Neg_Words$Column1))
{
  Neg_Words$stemmed[i]=stemDocument(Neg_Words$Column1[i])  
}

exceptions   <- c("not","never","no","don't")
my_stopwords <- setdiff(stopwords("en"), exceptions)

library(NLP)
library(tm)
library(SnowballC)

Sentence_data$Sentence.ID=seq.int(nrow(Sentence_data))

# to assign sentiment to each sentence
i=1
for(i in 1:length(Sentence_data$Data))
{
  negate_s=0
  words <- strsplit(as.character(Sentence_data$stemmed_text[[i]])," ")
  word <- unlist(words)
  positive=0
  negative=0
  
  j=1
  for (j in 1:100) #Since the sentence will not be higher than 100 words
  {
    if(is.na(match(as.character(word[j]),table = Pos_Words$stemmed))==FALSE) #to check for positive words
      positive=positive+1
    if(is.na(match(as.character(word[j]),table = Neg_Words$stemmed))==FALSE) #to check for negative words
      negative=negative+1
    if(is.na(match(as.character(word[j]),c("no","never","not","Doesn't")))==FALSE) # to check for negation
      negate_s=1
  }
  
  #To assign sentiment for each sentence
  if(positive>negative)
  {
    if(negate_s==1)
      Sentence_data$Sentence_sentiment[i]="Negative"
    else
      Sentence_data$Sentence_sentiment[i]="Positive"
    
  }
  if(positive==negative)
  {
    Sentence_data$Sentence_sentiment[i]="Nutral"
  }
  if(negative>positive)
  {
    if(negate_s==1 )
      Sentence_data$Sentence_sentiment[i]="Positive"
    else
      Sentence_data$Sentence_sentiment[i]="Negative"
    
  }
  Sentence_data$S_postive[i]=positive
  Sentence_data$S_negative[i]=negative
}

# on observing the topics found in the topic modelling and to group the related words
Sentence_data$stemmed_text=gsub("wait","time",Sentence_data$stemmed_text)
Sentence_data$stemmed_text=gsub("order","servic",Sentence_data$stemmed_text)
Sentence_data$stemmed_text=gsub("menu","food",Sentence_data$stemmed_text)
Sentence_data$stemmed_text=gsub("burger","food",Sentence_data$stemmed_text)
Sentence_data$stemmed_text=gsub("pancake","food",Sentence_data$stemmed_text)
Sentence_data$stemmed_text=gsub("deal","price",Sentence_data$stemmed_text)

#To update the restaurent table with the polarity scores for the topics
print(res_count)
  temp=subset(Sentence_data,grepl("food",Sentence_data$stemmed_text)==TRUE) #filter to teh sentences which have only the word required
  restaurent$Food[res_count]=mean((temp$S_postive-temp$S_negative))       #calculating the mean of the polarity
  temp=subset(Sentence_data,grepl("servic",Sentence_data$stemmed_text)==TRUE)
  restaurent$Service[res_count]=mean((temp$S_postive-temp$S_negative))       
  temp=subset(Sentence_data,grepl("place",Sentence_data$stemmed_text)==TRUE)
  restaurent$place[res_count]=mean((temp$S_postive-temp$S_negative))       
  temp=subset(Sentence_data,grepl("time",Sentence_data$stemmed_text)==TRUE)
  restaurent$time[res_count]=mean((temp$S_postive-temp$S_negative))       
  temp=subset(Sentence_data,grepl("price",Sentence_data$stemmed_text)==TRUE)
  restaurent$price[res_count]=mean((temp$S_postive-temp$S_negative))       
}     

#Res_categories=df_details_final
View(Res_categories)

#Remove unnecessary columns
Res_categories$Link=NULL

#Merging the restuarent table with polarity with the restaurents data scraped with code
Final_ResData=merge(x = Res_categories,y = restaurent,by = 'Name', all.x = TRUE)
str(Final_ResData)

#Removing uncessary columns and chaging data types
Final_ResData_init=Final_ResData
Final_ResData$Name=NULL
Final_ResData$Name.1=NULL
Final_ResData$ï..Link=NULL
Final_ResData$Parking=NULL
Final_ResData$Food=as.numeric(Final_ResData$Food)
Final_ResData$Service=as.numeric(Final_ResData$Service)
Final_ResData$place=as.numeric(Final_ResData$place)
Final_ResData$time=as.numeric(Final_ResData$time)
Final_ResData$price=as.numeric(Final_ResData$price)
Final_ResData$NA=NULL


#dim(Final_ResData[!complete.cases(Final_ResData),])[[1]]/nrow(Final_ResData)*100

is.na(Final_ResData)='NA'

#To replace missing values
library(mice)
# creating a list of imputed values using a CART model
imputedValues <- mice(data=Final_ResData, m=3, method="cart", seed=2016)

str(Final_ResData[1:13])
summary(Final_ResData)

#Creating a linear regression equation
model=lm(data = Final_ResData[1:13],formula = Final_ResData$Rating~.)

summary(model)

#To perform backword regression
library(MASS)
step.model <- stepAIC(model, direction = "backward", 
                       trace = FALSE)
summary(step.model)


#########################################################

#Outputing required files
write.csv(x = Words_Frequency,file = "Review frequent words.csv")
write.csv(x = Final_ResData_init,file = "Final joined data.csv")
write.csv(x = Sentence_Sentiment_Result,file = "Reviews Sentences.csv")
