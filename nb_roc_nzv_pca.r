# Near Zero + PCA + NORMALIZED TRAIN + RANDOM FOREST + ROC

library(caret)
library(stats)
library(pROC)
library(ROCR)
library(e1071)
library(randomForest)



train <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/train.csv")


train$id=factor(train$id)
train$species=factor(train$species)


#normalize the data for continuous features between 0 and 1

shape <- train[ , grepl( "shape" , names( train ) ) ]
margin <- train[ , grepl( "margin" , names( train ) ) ]
texture <- train[ , grepl( "texture" , names( train ) ) ]

normalize <- function(x) {
  
  return ((x - min(x)) / (max(x) - min(x)))
  
}

normalized_shape <- as.data.frame(lapply(shape, normalize))
normalized_margin <- as.data.frame(lapply(margin, normalize))
normalized_texture <- as.data.frame(lapply(texture, normalize))

train_data <- data.frame(train$id,normalized_margin,normalized_shape,normalized_texture,train$species)

names(train_data)[1] <- paste("id")
names(train_data)[194] <- paste("species")

train_data <- train_data [,-1]

#Non - Zero Variance
nzv <- nearZeroVar(train_data)


filteredData_nzv <- train_data[, -nzv]



train_corr <- cor(filteredData_nzv[,-ncol(filteredData_nzv)])



high_Correlation <- findCorrelation(train_corr, cutoff = .75)
filteredData_HighCorr <- filteredData_nzv[,-high_Correlation]

train <- filteredData_HighCorr
shape <- train[ , grepl( "shape" , names( train ) ) ]
margin <- train[ , grepl( "margin" , names( train ) ) ]
texture <- train[ , grepl( "texture" , names( train ) ) ]


pca_margin<-princomp(margin,cor=TRUE,score=TRUE)
pca_shape<-princomp(shape,cor=TRUE,score=TRUE)
pca_texture<-princomp(texture,cor=TRUE,score=TRUE)

train_pca <- data.frame(train$species,pca_margin$scores[,1:30],pca_shape$scores,pca_texture$scores[,1:30])

colnames(train_pca)<-c('species','Component1','Component2','Component3','Component4','Component5','Component6','Component7','Component8','Component9','Component10','Component11','Component12','Component13','Component14','Component15','Component16','Component17','Component18','Component19','Component20','Component21','Component22','Component23','Component24','Component25','Component26','Component27','Component28','Component29','Component30','Component31','Component32','Component33','Component34','Component35','Component36','Component37','Component38','Component39','Component40','Component41','Component42','Component43','Component44','Component45','Component46','Component47','Component48','Component49','Component50','Component51','Component52','Component53','Component54','Component55','Component56','Component57','Component58','Component59','Component60','Component61','Component62')





nb_model <- naiveBayes(species ~ ., data = train_pca)

#splitting up train into 80:20 ratio for ROC curve

trainIndex <- sample(1:nrow(train_pca), 0.8 * nrow(train_pca))
train_split <- train_pca[trainIndex, ]
test_split <- train_pca[-trainIndex, ]


nb.model.roc <- naiveBayes(species ~ ., data = train_split)
nb_model <- naiveBayes(species ~ ., data = train_pca)




test <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/test.csv")


shape <- test[ , grepl( "shape" , names( test ) ) ]
margin <- test[ , grepl( "margin" , names( test ) ) ]
texture <- test[ , grepl( "texture" , names( test ) ) ]

#normalized_shape <- as.data.frame(lapply(shape, normalize))
#normalized_margin <- as.data.frame(lapply(margin, normalize))
#normalized_texture <- as.data.frame(lapply(texture, normalize))


test_withoutID <- data.frame(margin,shape,texture)

new_test <- data.frame(test_withoutID$margin1 , test_withoutID$margin3 , test_withoutID$margin4 , test_withoutID$margin7 , test_withoutID$margin8 , test_withoutID$margin9 , 
                       test_withoutID$margin10 , test_withoutID$margin11 , test_withoutID$margin12 , test_withoutID$margin13 , test_withoutID$margin14 , test_withoutID$margin17 , 
                       test_withoutID$margin18 , test_withoutID$margin19 , test_withoutID$margin20 , test_withoutID$margin24 , test_withoutID$margin26 , test_withoutID$margin28 , 
                       test_withoutID$margin29 , test_withoutID$margin30 , test_withoutID$margin32 , test_withoutID$margin33 , test_withoutID$margin34 , test_withoutID$margin35 , 
                       test_withoutID$margin36 , test_withoutID$margin37 , test_withoutID$margin38 , test_withoutID$margin39 , test_withoutID$margin41 , test_withoutID$margin42 , 
                       test_withoutID$margin43 , test_withoutID$margin44 , test_withoutID$margin45 , test_withoutID$margin47 , test_withoutID$margin48 , test_withoutID$margin50 , 
                       test_withoutID$margin51 , test_withoutID$margin52 , test_withoutID$margin53 , test_withoutID$margin54 , test_withoutID$margin55 , test_withoutID$margin57 , 
                       test_withoutID$margin59 , test_withoutID$margin60 , test_withoutID$margin61 , test_withoutID$margin63 , test_withoutID$shape1 , test_withoutID$shape49 , 
                       test_withoutID$texture1 , test_withoutID$texture3 , test_withoutID$texture4 , test_withoutID$texture5 , test_withoutID$texture6 , test_withoutID$texture7 , 
                       test_withoutID$texture8 , test_withoutID$texture9 , test_withoutID$texture10 , test_withoutID$texture11 , test_withoutID$texture12 , 
                       test_withoutID$texture13 , test_withoutID$texture14 , test_withoutID$texture17 , test_withoutID$texture18 , test_withoutID$texture19 , 
                       test_withoutID$texture20 , test_withoutID$texture22 , test_withoutID$texture23 , test_withoutID$texture24 , test_withoutID$texture25 , 
                       test_withoutID$texture26 , test_withoutID$texture27 , test_withoutID$texture28 , test_withoutID$texture29 , test_withoutID$texture31 , 
                       test_withoutID$texture33 , test_withoutID$texture34 , test_withoutID$texture37 , test_withoutID$texture38 , test_withoutID$texture39 , 
                       test_withoutID$texture40 , test_withoutID$texture41 , test_withoutID$texture42 , test_withoutID$texture43 , test_withoutID$texture45 , 
                       test_withoutID$texture46 , test_withoutID$texture47 , test_withoutID$texture48 , test_withoutID$texture49 , test_withoutID$texture50 , 
                       test_withoutID$texture51 , test_withoutID$texture52 , test_withoutID$texture53 , test_withoutID$texture54 , test_withoutID$texture55 , 
                       test_withoutID$texture57 , test_withoutID$texture58 , test_withoutID$texture59 , test_withoutID$texture60 , test_withoutID$texture62 , 
                       test_withoutID$texture63 , test_withoutID$texture64)

shape <- new_test[ , grepl( "shape" , names( new_test ) ) ]
margin <- new_test[ , grepl( "margin" , names( new_test ) ) ]
texture <- new_test[ , grepl( "texture" , names( new_test ) ) ]



nb_test <- data.frame(margin[,1:30],shape,texture[,1:30])



colnames(nb_test)<-c('Component1','Component2','Component3','Component4','Component5','Component6','Component7','Component8','Component9','Component10','Component11','Component12','Component13','Component14','Component15','Component16','Component17','Component18','Component19','Component20','Component21','Component22','Component23','Component24','Component25','Component26','Component27','Component28','Component29','Component30','Component31','Component32','Component33','Component34','Component35','Component36','Component37','Component38','Component39','Component40','Component41','Component42','Component43','Component44','Component45','Component46','Component47','Component48','Component49','Component50','Component51','Component52','Component53','Component54','Component55','Component56','Component57','Component58','Component59','Component60','Component61','Component62')



nb.pred<- predict(nb_model,newdata= nb_test,type="raw")

nb.pred.roc<- predict(nb_model,newdata= test_split[,-1],type="raw")


m <- model.matrix( 
  ~ species -1 , 
  data = test_split )

#test.forest = predict(nb.pred.roc, type ='prob', newdata = test_split[,-1])
forestpred <- prediction(as.numeric(nb.pred.roc), as.numeric(m))
forestperf = performance(forestpred, "tpr", "fpr")


auc(as.numeric(m),as.numeric(nb.pred.roc))



## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
plot(forestperf, main="ROC NaiveBayes", colorize=T)

## precision/recall curve (x-axis: recall, y-axis: precision)
f1 <- performance(forestpred, "prec", "rec")
plot(f1, main="Precision Recall", colorize=T)


## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
ss <-  performance(forestpred, "sens", "spec")
plot(ss , main="specificity Sensitivity", colorize=T)




result <- data.frame (test$id,nb.pred)

library(xlsx)
write.xlsx(x=result,file= "C:/Users/renuka/Desktop/Desktop/R working dir/nb_result.xlsx", row.names = FALSE)

