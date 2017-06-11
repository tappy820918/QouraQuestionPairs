##
#train.csv需到 https://www.kaggle.com/c/quora-question-pairs/data 下載
#Training.csv可以從 https://www.dropbox.com/s/a45topjtwzsk9eh/Training.csv?dl=0 下載
#Distance.csv可以從 https://www.dropbox.com/s/u0qw9qq5xdxmf4e/Distance.csv?dl=0 下載
#NOTE:需先將下面三個csv黨放置C:下
#第一分train在大迴圈中會跑很久，可直接跳過讀取Training.csv處理完成之資料
#Distance情況亦相同
#########################################
trainPath = "C:\\train.csv"            ##
DATAPath = "C:\\Training.csv"          ##
DistancePath = "C:\\Distance.csv"      ##
#########################################
##Require packages and Install if needed
package = c("gbm","corrplot","knitr", "readr","stringr","ggthemes","ggplot2","GGally","syuzhet", "SnowballC","tm","xgboost")
Already.Installed.Package = package %in% rownames(installed.packages())
if(any(!Already.Installed.Package)) install.packages(package[!Already.Installed.Package])
sapply(package, require, character.only = TRUE)



train =read_csv(trainPath)
cat("number of rows:  ",dim(train)[1],"\nnumber of columns : ",dim(train)[2])


#since  it is a question pairs, the 6W's are inportant
StopWord = stopwords("english")
for (i in 1:length(stopwords("english"))){
  if ( StopWord[i] %in% c("who","what","when","where","why","how","which") ){
    StopWord[i] = NA
  }
}
StopWord = as.character(na.exclude(StopWord))

Train = data.frame()
Train.row = data.frame()

for (i in 1:nrow(train))
#for (i in 1:1000)
{
  q1 = Corpus(VectorSource(train$question1[i]))
  q2 = Corpus(VectorSource(train$question2[i]))
  
  #Removing punctuations
  q1 = tm_map(q1, removePunctuation)   
  q2 = tm_map(q2, removePunctuation)   
  
  #Removing Numbers removeNumbers
  q1 = tm_map(q1, removeNumbers)
  q2 = tm_map(q2, removeNumbers)   
  
  #To avoid duplicacy converting all to lower case tolower
  q1 = tm_map(q1, tolower)
  q2 = tm_map(q2, tolower)   
  
  # removing common word endings stemDocument
  q1 = tm_map(q1, stemDocument)
  q2 = tm_map(q2, stemDocument)
  # to remove white space stripWhitespace
  q1 = tm_map(q1, stripWhitespace)
  q2 = tm_map(q2, stripWhitespace)
  #Removing Stop Words as they don't add any value
  q1 = tm_map(q1, removeWords, StopWord)
  q2 = tm_map(q2, removeWords, StopWord)
  # to convert documents into text documents
  q1 = tm_map(q1, PlainTextDocument)
  q2 = tm_map(q2, PlainTextDocument)
  
  a = TermDocumentMatrix(q1)$dimnames$Terms
  b = TermDocumentMatrix(q2)$dimnames$Terms
  
  same_items = sum(a %in% b)
  distinct_items = length(a) + length(b)
  #distinct_items
  match_count = (2*same_items)/(distinct_items)
  #match_count
  
  sentiment1 = get_nrc_sentiment(train$question1[i])
  sentiment2 = get_nrc_sentiment(train$question2[i])

  if(sum(sentiment1)!= 0 ) sentiment1 = sentiment1/sum(sentiment1)
  if(sum(sentiment2)!= 0 ) sentiment2 = sentiment2/sum(sentiment2)
  
  Q1_anger =  sum(sentiment1$anger)
  Q1_anticipation =  sum(sentiment1$anticipation)
  Q1_disgust =  sum(sentiment1$disgust)
  Q1_fear =  sum(sentiment1$fear)
  Q1_joy =  sum(sentiment1$joy)
  Q1_sadness =  sum(sentiment1$sadness)
  Q1_surprise =  sum(sentiment1$surprise)
  Q1_trust =  sum(sentiment1$trust)
  Q1_negative =  sum(sentiment1$negative)
  Q1_positive =  sum(sentiment1$positive)
  
  Q2_anger =  sum(sentiment2$anger)
  Q2_anticipation =  sum(sentiment2$anticipation)
  Q2_disgust =  sum(sentiment2$disgust)
  Q2_fear =  sum(sentiment2$fear)
  Q2_joy =  sum(sentiment2$joy)
  Q2_sadness =  sum(sentiment2$sadness)
  Q2_surprise =  sum(sentiment2$surprise)
  Q2_trust =  sum(sentiment2$trust)
  Q2_negative =  sum(sentiment2$negative)
  Q2_positive =  sum(sentiment2$positive)
  
  Train.row = cbind(match_count,
                 Q1_anger,Q1_anticipation,Q1_disgust,Q1_fear,Q1_joy,Q1_sadness,Q1_surprise,Q1_trust,Q1_negative,Q1_positive,
                 Q2_anger,Q2_anticipation,Q2_disgust,Q2_fear,Q2_joy,Q2_sadness,Q2_surprise,Q2_trust,Q2_negative,Q2_positive)
  Train = rbind(Train,Train.row)
  print(i)
}
write.csv(Train,"C:\\Training.csv")
#============================================================================#
Training = read.csv(DATAPath)
#fetch the column names
sentiment = get_nrc_sentiment(iconv(train$question1[1], "latin1", "ASCII", sub=""))
#Compare and select the label that is intersected
T_compared = pmin(as.matrix(Training[,3:12]),as.matrix(Training[,3:12]))
#remake the data frame
colnames(T_compared) = colnames(sentiment)
DATA = cbind.data.frame(Match = Training$match_count,T_compared,Y = train$is_duplicate)
#Resample
set.seed(12)
DATA = DATA[sample(nrow(DATA), nrow(DATA)), ]
DATA[,1:11] = as.data.frame(DATA[,1:11])
#Calculate feature "Distance" and write to csv
Distance = matrix(matrix(0,nrow = nrow(DATA) ,ncol = 1))
for(x in 1:nrow(DATA)){
  Distance[x,1] =sum((DATA[x,2:11])^2)
  print(i)
}
Distance = sqrt(Distance)
write.csv( Distance,"C:\\Distance.csv")
Distance = read.csv(DistancePath)
DATA = cbind.data.frame(DATA, Distance = Distance$V1)
ggcorr(DATA[,-12]) + ggtitle("Correlation Plot") + theme_pander()
#Turn to negative for negative sentiment
DATA$anger = (-1)*TRAIN$anger
DATA$disgust = (-1)*TRAIN$disgust
DATA$fear = (-1)*TRAIN$fear
DATA$sadness = (-1)*TRAIN$sadness
DATA$negative = (-1)*TRAIN$negative



#Rescale amd resample
set.seed(12)
DATA = DATA[sample(nrow(DATA), nrow(DATA)), ]
DATA[,1:11] = as.data.frame(scale(DATA[,1:11]))
#Seperate into training and testing data
Label_train = c(data.matrix(DATA$Y[1:250000]))
Label_test = as.factor(DATA$Y[250001:404290])
X_train = DATA[1:250000,c(1:11)]
X_test = DATA[250001:404290,c(1:11)]

#============================================================================#
##Start training

##XGBoost
XGBModel = xgboost(data = data.matrix(X_train), label =Label_train, nthread = 8, nround = 2000, objective = "binary:logistic")
IMP = xgb.importance(colnames(DATA), model = XGBModel)
IMP = IMP[order(IMP$Cover,decreasing = TRUE),]
IMP = as.data.frame(IMP)
qplot(x = c(1:length(IMP$Gain)),y = IMP$Cover, xlab = "Variable", ylab = "Gain") + 
  ggtitle("Importance Plot") + geom_line() + theme_economist() 
predict_XGB = predict(XGBModel, data.matrix(X_test))
prediction_XGB = as.numeric(predict_XGB > 0.5)
Table_XGB = table(prediction_XGB,Label_test)
Accruacy_XGB = sum(diag(Table_XGB))/sum(Table_XGB)
##Logistic regression
GLMModel = glm(Label_train ~., family=binomial(link='logit'),data = X_train)
pred_GLM = predict(GLMModel, X_test)
prediction_GLM = as.numeric(pred_GLM > 0.5)
Accruacy_GLM = sum( prediction_GLM == Label_test ) / length( prediction_GLM )
##CART classification tree
CRTModel = rpart(as.factor(Label_train)~.,data=X_train)
pred_CRT = predict(CRTModel, X_test, type = "class")
prediction_CRT = as.numeric(as.matrix(pred_CRT))
Accruacy_CRT = sum( prediction_CRT == Label_test ) / length( prediction_CRT )

##C5.0 boosting tree
C50Model = C5.0(as.factor(Label_train)~ . ,data=X_train , trials=50)
prediction_C50 = predict(C50Model, X_test, type = "class")
Accruacy_C50 = sum( prediction_C50 == Label_test ) / length( prediction_C50 )

#####Ensembling models
Vote = as.numeric(prediction_XGB) + 
  as.numeric(prediction_GLM) + 
  as.numeric(prediction_CRT) + 
  as.numeric(prediction_C50) -1
prediction_Vote = as.numeric(Vote > 2)
Ensembled_Accruacy = sum( prediction_Vote == Label_test ) / length(prediction_Vote)
##
FinalTable = cbind.data.frame(Ensembled_Accruacy, Accruacy_XGB, Accruacy_GLM,
                              Accruacy_CRAT=Accruacy_CRT, Accruacy_C50)
kable(FinalTable)